;;; agent-shell-fork-tree-acp.el --- Generic ACP history discovery -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Code:
(require 'agent-shell-fork-tree-store)
(require 'acp)

(defcustom agent-shell-fork-tree-request-timeout 60
  "Maximum seconds for each discovery or history request."
  :type 'number :group 'agent-shell-fork-tree)

(defcustom agent-shell-fork-tree-checkpoint-batch-size 16
  "Number of history reads between disk checkpoints during discovery.
Completed turns are indexed in memory immediately.  Completion, cancellation
and failure flush pending changes regardless of this limit.  One saves after
every history read."
  :type 'integer :group 'agent-shell-fork-tree)

(defcustom agent-shell-fork-tree-scan-concurrency 8
  "Maximum number of histories read concurrently during discovery.
Each concurrent reader owns an independent ACP client.  Concurrency greater
than one is used only when the agent advertises both session/fork and
session/delete, so inspection never attaches multiple clients to original
sessions.  A value of one preserves serial discovery."
  :type '(integer :tag "Concurrent history readers")
  :group 'agent-shell-fork-tree)

(cl-defstruct (agent-shell-fork-tree--reader
               (:constructor agent-shell-fork-tree--reader-create))
  buffer client timer stage row index snapshot reading updates (generation 0) closed)

(defun agent-shell-fork-tree--prioritize (sessions focus related)
  "Return SESSIONS with FOCUS first, then RELATED session IDs.
Each input row is visited once and ordering within each priority group is kept."
  (let ((related-ids (make-hash-table :test #'equal)) focused known other)
    (dolist (id related) (puthash id t related-ids))
    (dolist (session sessions)
      (let ((id (map-elt session 'sessionId)))
        (cond ((equal focus id) (push session focused))
              ((gethash id related-ids) (push session known))
              (t (push session other)))))
    (append (nreverse focused) (nreverse known) (nreverse other))))

(defun agent-shell-fork-tree--append-page (listed tail count rows)
  "Append ROWS to LISTED after TAIL and return updated list state.
The result is (LISTED TAIL COUNT); each row and list spine is visited once."
  (when rows
    (if tail (setcdr tail rows) (setq listed rows))
    (setq tail (last rows)
          count (+ count (length rows))))
  (list listed tail count))

(defun agent-shell-fork-tree--scan (source store focus full progress finish)
  "Read SOURCE's project into STORE for FOCUS, returning a cancel function.
FULL bypasses timestamp caches.  PROGRESS receives STORE and a message.
FINISH receives STORE, an error string or nil, and whether cancelled.
History RPCs may finish out of order, but their results are ingested in the
same priority order used by serial discovery."
  (let* ((config (map-elt (buffer-local-value 'agent-shell--state source) :agent-config))
         (cwd (agent-shell-fork-tree--store-cwd store))
         (default-dir (buffer-local-value 'default-directory source))
         (primary-buffer (generate-new-buffer " *Fork tree ACP*"))
         primary caps listed listed-tail (listed-count 0) total jobs (job-count 0)
         (next-dispatch 0) (next-commit 0) (ready-results (make-hash-table :test #'eql))
         readers deletable-forks effective-concurrency finished stopping stop-error cancelled
         dirty (pending-saves 0) (done 0))
    (with-current-buffer primary-buffer
      (setq default-directory default-dir))
    (setq primary
          (agent-shell-fork-tree--reader-create
           :buffer primary-buffer
           :client (funcall (map-elt config :client-maker) primary-buffer)
           :stage 'starting)
          readers (list primary))
    (cl-labels
        ((publish (message)
           (funcall progress store message))
         (checkpoint (&optional force)
           (when (and (not full) dirty
                      (or force (>= pending-saves agent-shell-fork-tree-checkpoint-batch-size)))
             (agent-shell-fork-tree--save store)
             (setq dirty nil pending-saves 0)))
         (cancel-timer-for (reader)
           (when-let* ((timer (agent-shell-fork-tree--reader-timer reader)))
             (cancel-timer timer)
             (setf (agent-shell-fork-tree--reader-timer reader) nil)))
         (shutdown-reader (reader)
           (unless (agent-shell-fork-tree--reader-closed reader)
             (cancel-timer-for reader)
             (setf (agent-shell-fork-tree--reader-closed reader) t
                   (agent-shell-fork-tree--reader-stage reader) 'closed)
             (condition-case nil
                 (acp-shutdown :client (agent-shell-fork-tree--reader-client reader))
               (error nil))
             (when (buffer-live-p (agent-shell-fork-tree--reader-buffer reader))
               (kill-buffer (agent-shell-fork-tree--reader-buffer reader)))
             (maybe-finish-stopping)))
         (finalize (error-text)
           (unless finished
             (setq finished t)
             (condition-case error
                 (checkpoint t)
               (error (setq error-text (error-message-string error))))
             (dolist (reader readers)
               (cancel-timer-for reader)
               (unless (agent-shell-fork-tree--reader-closed reader)
                 (setf (agent-shell-fork-tree--reader-closed reader) t
                       (agent-shell-fork-tree--reader-stage reader) 'closed)
                 (condition-case nil
                     (acp-shutdown :client (agent-shell-fork-tree--reader-client reader))
                   (error nil))
                 (when (buffer-live-p (agent-shell-fork-tree--reader-buffer reader))
                   (kill-buffer (agent-shell-fork-tree--reader-buffer reader)))))
             (funcall finish store error-text cancelled)))
         (maybe-finish-stopping ()
           (when (and stopping (not finished)
                      (seq-every-p #'agent-shell-fork-tree--reader-closed readers))
             (finalize stop-error)))
         (begin-stop (error-text)
           (unless finished
             (unless stopping
               (setq stopping t stop-error error-text))
             (when (and error-text (not stop-error))
               (setq stop-error error-text))
             ;; Fork/load/delete may own a temporary session.  Let their current
             ;; callback reach cleanup; all other requests can be closed now.
             (dolist (reader readers)
               (unless (or (agent-shell-fork-tree--reader-closed reader)
                           (member (agent-shell-fork-tree--reader-stage reader)
                                   '("session/fork" "session/load" "session/delete")))
                 (shutdown-reader reader)))
             (maybe-finish-stopping)))
         (request-label (reader method)
           (format "%s timed out%s" method
                   (if-let* ((row (agent-shell-fork-tree--reader-row reader)))
                       (concat ": " (map-elt row 'sessionId))
                     "")))
         (send (reader request success failure)
           (cancel-timer-for reader)
           (let ((method (map-elt request :method))
                 (generation (1+ (agent-shell-fork-tree--reader-generation reader))))
             (setf (agent-shell-fork-tree--reader-stage reader) method
                   (agent-shell-fork-tree--reader-generation reader) generation
                   (agent-shell-fork-tree--reader-timer reader)
                   (run-at-time
                    agent-shell-fork-tree-request-timeout nil
                    (lambda ()
                      (when (and (= generation (agent-shell-fork-tree--reader-generation reader))
                                 (not finished)
                                 (not (agent-shell-fork-tree--reader-closed reader)))
                        (cl-incf (agent-shell-fork-tree--reader-generation reader))
                        (funcall failure `((message . ,(request-label reader method))) nil)))))
             (condition-case error
                 (acp-send-request
                  :client (agent-shell-fork-tree--reader-client reader)
                  :buffer (agent-shell-fork-tree--reader-buffer reader)
                  :request request
                  :on-success
                  (lambda (response)
                    (when (= generation (agent-shell-fork-tree--reader-generation reader))
                      (cancel-timer-for reader))
                    (when (and (= generation (agent-shell-fork-tree--reader-generation reader))
                               (not finished)
                               (not (agent-shell-fork-tree--reader-closed reader)))
                      (condition-case error
                          (funcall success response)
                        (error (reader-failed reader
                                              `((message . ,(error-message-string error))) nil)))))
                  :on-failure
                  (lambda (error raw)
                    (when (= generation (agent-shell-fork-tree--reader-generation reader))
                      (cancel-timer-for reader))
                    (when (and (= generation (agent-shell-fork-tree--reader-generation reader))
                               (not finished)
                               (not (agent-shell-fork-tree--reader-closed reader)))
                      (funcall failure error raw))))
               (error
                (when (= generation (agent-shell-fork-tree--reader-generation reader))
                  (cancel-timer-for reader)
                  (cl-incf (agent-shell-fork-tree--reader-generation reader))
                  (funcall failure `((message . ,(error-message-string error))) nil))))))
         (failure-text (reader error)
           (format "%s%s"
                   (if-let* ((row (agent-shell-fork-tree--reader-row reader)))
                       (format "%s: " (or (map-elt row 'title) (map-elt row 'sessionId)))
                     "")
                   (or (map-elt error 'message) "ACP request failed")))
         (record-result (reader complete)
           (when-let* ((row (agent-shell-fork-tree--reader-row reader))
                       ((or complete (agent-shell-fork-tree--reader-updates reader))))
             (let ((updates (nreverse (agent-shell-fork-tree--reader-updates reader))))
               (setf (agent-shell-fork-tree--reader-updates reader) nil)
               (puthash (agent-shell-fork-tree--reader-index reader)
                        (list row (agent-shell-fork-tree--turns updates complete) complete)
                        ready-results)
               (drain-results))))
         (reader-after-task (reader)
           (setf (agent-shell-fork-tree--reader-row reader) nil
                 (agent-shell-fork-tree--reader-index reader) nil
                 (agent-shell-fork-tree--reader-snapshot reader) nil
                 (agent-shell-fork-tree--reader-reading reader) nil
                 (agent-shell-fork-tree--reader-updates reader) nil)
           (if stopping
               (shutdown-reader reader)
             (setf (agent-shell-fork-tree--reader-stage reader) 'idle)
             (dispatch)
             (maybe-complete)))
         (delete-snapshot (reader)
           (if-let* ((id (agent-shell-fork-tree--reader-snapshot reader)))
               (send reader (acp-make-session-delete-request :session-id id)
                     (lambda (_)
                       (setf (agent-shell-fork-tree--reader-snapshot reader) nil)
                       (reader-after-task reader))
                     (lambda (error _raw)
                       (setf (agent-shell-fork-tree--reader-snapshot reader) nil)
                       (if stopping
                           (shutdown-reader reader)
                         (begin-stop
                          (format "Temporary session %s could not be deleted: %s"
                                  id (or (map-elt error 'message) "ACP request failed")))
                         (shutdown-reader reader))))
             (reader-after-task reader)))
         (reader-failed (reader error _raw)
           (unless (or finished (agent-shell-fork-tree--reader-closed reader))
             (let ((stage (agent-shell-fork-tree--reader-stage reader)))
               (begin-stop (failure-text reader error))
               (cond
                ((equal stage "session/load")
                 ;; As in serial discovery, completed older turns from an
                 ;; interrupted replay may still advance a partial checkpoint.
                 (record-result reader nil)
                 (delete-snapshot reader))
                ((equal stage "session/delete")
                 (setf (agent-shell-fork-tree--reader-snapshot reader) nil)
                 (shutdown-reader reader))
                (t (shutdown-reader reader))))))
         (loaded (reader _response)
           (record-result reader (not stopping))
           (setf (agent-shell-fork-tree--reader-reading reader) nil)
           (delete-snapshot reader))
         (read-history (reader id)
           (setf (agent-shell-fork-tree--reader-reading reader) id
                 (agent-shell-fork-tree--reader-updates reader) nil)
           (send reader
                 (acp-make-session-load-request :session-id id :cwd cwd :mcp-servers [])
                 (lambda (response) (loaded reader response))
                 (lambda (error raw) (reader-failed reader error raw))))
         (start-job (reader index row)
           (setf (agent-shell-fork-tree--reader-index reader) index
                 (agent-shell-fork-tree--reader-row reader) row
                 (agent-shell-fork-tree--reader-snapshot reader) nil
                 (agent-shell-fork-tree--reader-reading reader) nil
                 (agent-shell-fork-tree--reader-updates reader) nil)
           (publish (format "Reading %d/%d · %s" (1+ index) total
                            (or (map-elt row 'title) (map-elt row 'sessionId))))
           (if deletable-forks
               (send reader
                     (acp-make-session-fork-request
                      :session-id (map-elt row 'sessionId) :cwd cwd :mcp-servers [])
                     (lambda (result)
                       (setf (agent-shell-fork-tree--reader-snapshot reader)
                             (map-elt result 'sessionId))
                       (if stopping
                           (delete-snapshot reader)
                         (read-history reader
                                       (agent-shell-fork-tree--reader-snapshot reader))))
                     (lambda (error raw) (reader-failed reader error raw)))
             ;; Backends without deletable forks retain the old safe behavior:
             ;; one reader loads each original session at a time.
             (read-history reader (map-elt row 'sessionId))))
         (dispatch ()
           (unless stopping
             (dolist (reader readers)
               (when (and (eq (agent-shell-fork-tree--reader-stage reader) 'idle)
                          (< next-dispatch job-count))
                 (let ((index next-dispatch))
                   (cl-incf next-dispatch)
                   (start-job reader index (aref jobs index)))))))
         (drain-results ()
           (let (result)
             (while (and (not finished)
                         (setq result (gethash next-commit ready-results)))
               (remhash next-commit ready-results)
               (condition-case error
                   (progn
                     (agent-shell-fork-tree--ingest
                      store (nth 0 result) (nth 1 result) (nth 2 result) focus)
                     (setq dirty t)
                     (cl-incf pending-saves)
                     (cl-incf done)
                     (cl-incf next-commit)
                     (checkpoint)
                     (publish (format "Read %d/%d · %s" done total
                                      (or (map-elt (nth 0 result) 'title)
                                          (map-elt (nth 0 result) 'sessionId)))))
                 (error
                  (begin-stop (error-message-string error))
                  (setq result nil))))
             (dispatch)))
         (reconcile ()
           ;; Reconcile only after every requested history completed, so
           ;; cancellation cannot remove undiscovered endpoints.
           (let ((known (make-hash-table :test #'equal)))
             (dolist (info listed) (puthash (map-elt info 'sessionId) t known))
             (dolist (id (hash-table-keys (agent-shell-fork-tree--store-sessions store)))
               (unless (gethash id known)
                 (remhash id (agent-shell-fork-tree--store-sessions store))
                 (setq dirty t)
                 (cl-incf (agent-shell-fork-tree--store-revision store))))))
         (maybe-complete ()
           (when (and (not stopping) (= next-commit job-count)
                      (seq-every-p
                       (lambda (reader)
                         (eq (agent-shell-fork-tree--reader-stage reader) 'idle))
                       readers))
             (condition-case error
                 (progn (reconcile) (finalize nil))
               (error (begin-stop (error-message-string error))))))
         (subscribe-reader (reader)
           (acp-subscribe-to-notifications
            :client (agent-shell-fork-tree--reader-client reader)
            :buffer (agent-shell-fork-tree--reader-buffer reader)
            :on-notification
            (lambda (notification)
              (when (and (agent-shell-fork-tree--reader-reading reader)
                         (equal (agent-shell-fork-tree--reader-reading reader)
                                (map-nested-elt notification '(params sessionId))))
                (push (map-nested-elt notification '(params update))
                      (agent-shell-fork-tree--reader-updates reader)))))
           (acp-subscribe-to-errors
            :client (agent-shell-fork-tree--reader-client reader)
            :buffer (agent-shell-fork-tree--reader-buffer reader)
            :on-error (lambda (error) (reader-failed reader error nil))))
         (reader-ready (reader)
           (if stopping
               (shutdown-reader reader)
             (setf (agent-shell-fork-tree--reader-stage reader) 'idle)
             (dispatch)
             (maybe-complete)))
         (initialize-reader (reader ready-callback &optional primary-p)
           (send reader (acp-make-initialize-request :protocol-version 1)
                 (lambda (response)
                   (let ((reader-caps (map-elt response 'agentCapabilities)))
                     (unless (and (eq t (map-elt reader-caps 'loadSession))
                                  (or (not primary-p)
                                      (assq 'list (map-elt reader-caps 'sessionCapabilities))))
                       (error "Agent must advertise session/list and session/load"))
                     (when primary-p (setq caps reader-caps))
                     (if (map-elt config :needs-authentication)
                         (send reader (funcall (map-elt config :authenticate-request-maker))
                               (lambda (_) (funcall ready-callback reader))
                               (lambda (error raw) (reader-failed reader error raw)))
                       (funcall ready-callback reader))))
                 (lambda (error raw) (reader-failed reader error raw))))
         (make-reader ()
           (let* ((buffer (generate-new-buffer " *Fork tree ACP reader*"))
                  reader)
             (with-current-buffer buffer (setq default-directory default-dir))
             (setq reader
                   (agent-shell-fork-tree--reader-create
                    :buffer buffer :client (funcall (map-elt config :client-maker) buffer)
                    :stage 'starting))
             (setq readers (append readers (list reader)))
             (subscribe-reader reader)
             reader))
         (prepare-jobs ()
           (let (needed)
             (dolist (info listed)
               (if (agent-shell-fork-tree--needs-read store info focus full)
                   (push info needed)
                 (let* ((old (gethash (map-elt info 'sessionId)
                                      (agent-shell-fork-tree--store-sessions store)))
                        (title (or (map-elt info 'title) (map-elt info 'sessionId))))
                   (unless (equal title (agent-shell-fork-tree--session-title old))
                     (setf (agent-shell-fork-tree--session-title old) title)
                     (setq dirty t)
                     (cl-incf (agent-shell-fork-tree--store-revision store)))
                   (cl-incf done))))
             (setq jobs (vconcat (nreverse needed))
                   job-count (length jobs)
                   deletable-forks
                   (and (assq 'fork (map-elt caps 'sessionCapabilities))
                        (assq 'delete (map-elt caps 'sessionCapabilities)))
                   effective-concurrency
                   (if deletable-forks
                       (max 1 (min job-count
                                   (max 1 agent-shell-fork-tree-scan-concurrency)))
                     1))
             (setf (agent-shell-fork-tree--reader-stage primary) 'idle)
             (dotimes (_ (1- effective-concurrency))
               (initialize-reader (make-reader) #'reader-ready))
             (dispatch)
             (maybe-complete)))
         (page (reader cursor)
           (send reader
                 `((:method . "session/list")
                   (:params (cwd . ,cwd) ,@(when cursor `((cursor . ,cursor)))))
                 (lambda (response)
                   (if stopping
                       (shutdown-reader reader)
                     (let ((rows (append (map-elt response 'sessions) nil)))
                       (pcase-let ((`(,head ,tail ,count)
                                    (agent-shell-fork-tree--append-page
                                     listed listed-tail listed-count rows)))
                         (setq listed head listed-tail tail listed-count count)))
                     (publish (format "Discovering sessions · %d candidates" listed-count))
                     (if-let* ((next-cursor (map-elt response 'nextCursor)))
                         (page reader next-cursor)
                       (unless (seq-find (lambda (s) (equal focus (map-elt s 'sessionId))) listed)
                         (push `((sessionId . ,focus) (title . ,(buffer-name source))) listed))
                       (setq listed
                             (seq-filter
                              (lambda (s)
                                (or (not (map-elt s 'cwd))
                                    (equal cwd (directory-file-name (map-elt s 'cwd)))))
                              listed))
                       (let ((related
                              (mapcar #'agent-shell-fork-tree--session-id
                                      (agent-shell-fork-tree--related store focus))))
                         (setq listed (agent-shell-fork-tree--prioritize listed focus related)))
                       (setq total (length listed))
                       (prepare-jobs))))
                 (lambda (error raw) (reader-failed reader error raw))))
         (primary-ready (reader)
           (if stopping (shutdown-reader reader) (page reader nil))))
      (subscribe-reader primary)
      (publish "Discovering project sessions…")
      (initialize-reader primary #'primary-ready t)
      (lambda ()
        (unless finished
          (setq cancelled t)
          (begin-stop nil))))))

(provide 'agent-shell-fork-tree-acp)
;;; agent-shell-fork-tree-acp.el ends here
