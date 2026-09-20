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
FINISH receives STORE, an error string or nil, and whether cancelled."
  (let* ((config (map-elt (buffer-local-value 'agent-shell--state source) :agent-config))
         (cwd (agent-shell-fork-tree--store-cwd store))
         (worker (generate-new-buffer " *Fork tree ACP*"))
         client caps timer listed listed-tail (listed-count 0) total queue row snapshot reading updates stage
         finished cancelled dirty (pending-saves 0) (done 0))
    (with-current-buffer worker
      (setq default-directory (buffer-local-value 'default-directory source)
            client (funcall (map-elt config :client-maker) worker)))
    (cl-labels
        ((publish (message) (funcall progress store message))
         (checkpoint (&optional force)
           (when (and (not full) dirty
                      (or force (>= pending-saves agent-shell-fork-tree-checkpoint-batch-size)))
             (agent-shell-fork-tree--save store)
             (setq dirty nil pending-saves 0)))
         (close (error-text)
           (when timer (cancel-timer timer))
           (condition-case error (checkpoint t)
             (error (setq error-text (error-message-string error))))
           (acp-shutdown :client client)
           (kill-buffer worker)
           (funcall finish store error-text cancelled))
         (send (request success failure)
           (when timer (cancel-timer timer))
           (setq stage (map-elt request :method)
                 timer (run-at-time agent-shell-fork-tree-request-timeout nil
                                    (lambda () (stop (format "%s timed out%s" stage
                                                            (if row (concat ": " (map-elt row 'sessionId)) ""))))))
           (condition-case error
               (acp-send-request
                :client client :buffer worker :request request
                :on-success (lambda (response)
                              (when timer (cancel-timer timer))
                              (unless finished
                                (condition-case error (funcall success response)
                                  (error (stop (error-message-string error))))))
                :on-failure (lambda (error raw)
                              (when timer (cancel-timer timer))
                              (unless finished (funcall failure error raw))))
             (error (stop (error-message-string error)))))
         (release (next)
           (if snapshot
               (let ((id snapshot))
                 (setq snapshot nil)
                 (send (acp-make-session-delete-request :session-id id)
                       (lambda (_) (funcall next))
                       (lambda (error _raw)
                         (stop (format "Temporary session %s could not be deleted: %s" id (map-elt error 'message))))))
             (funcall next)))
         (commit (complete)
           (when row
             (agent-shell-fork-tree--ingest store row
                                           (agent-shell-fork-tree--turns (reverse updates) complete)
                                           complete focus)
             (setq dirty t)
             (cl-incf pending-saves)
             (checkpoint)))
         (stop (error-text)
           (unless finished
             ;; A cancelled replay can still contain whole, completed older turns.
             (when (and (equal stage "session/load") row updates)
               (condition-case nil (commit nil) (error nil)))
             (let ((id snapshot))
               (setq finished t snapshot nil)
               (when timer (cancel-timer timer))
               (if (and id (process-live-p (map-elt client :process)))
                   (let (closed)
                     (cl-labels ((done (&optional _response _raw)
                                   (unless closed
                                     (setq closed t)
                                     (close error-text))))
                       (setq timer (run-at-time 5 nil #'done))
                       (acp-send-request :client client :buffer worker
                                         :request (acp-make-session-delete-request :session-id id)
                                         :on-success #'done :on-failure #'done)))
                 (close error-text)))))
         (failed (error _raw)
           (stop (format "%s%s" (if row (format "%s: " (or (map-elt row 'title) (map-elt row 'sessionId))) "")
                         (map-elt error 'message))))
         (loaded (_)
           (if cancelled
               (stop nil)
             (commit t)
             (setq reading nil updates nil)
             (publish (format "Read %d/%d · %s" (cl-incf done) total
                              (or (map-elt row 'title) (map-elt row 'sessionId))))
             (release #'next)))
         (read-history (id)
           (setq reading id updates nil)
           (send (acp-make-session-load-request :session-id id :cwd cwd :mcp-servers []) #'loaded #'failed))
         (next ()
           (setq row nil reading nil updates nil)
           (while (and queue (not cancelled)
                       (not (agent-shell-fork-tree--needs-read store (car queue) focus full)))
             (let* ((info (pop queue))
                    (old (gethash (map-elt info 'sessionId) (agent-shell-fork-tree--store-sessions store))))
               (let ((title (or (map-elt info 'title) (map-elt info 'sessionId))))
                 (unless (equal title (agent-shell-fork-tree--session-title old))
                   (setf (agent-shell-fork-tree--session-title old) title)
                   (setq dirty t)
                   (cl-incf (agent-shell-fork-tree--store-revision store))))
               (cl-incf done)))
           (cond
            (cancelled (stop nil))
            ((null queue)
             ;; Reconcile only after a successful complete scan, so cancellation
             ;; cannot remove endpoints that have not yet been discovered.
             (let ((known (make-hash-table :test #'equal)))
               (dolist (info listed) (puthash (map-elt info 'sessionId) t known))
               (dolist (id (hash-table-keys (agent-shell-fork-tree--store-sessions store)))
                 (unless (gethash id known)
                   (remhash id (agent-shell-fork-tree--store-sessions store))
                   (setq dirty t)
                   (cl-incf (agent-shell-fork-tree--store-revision store)))))
             (setq finished t)
             (close nil))
            (t
             (setq row (pop queue))
             (publish (format "Reading %d/%d · %s" (1+ done) total
                              (or (map-elt row 'title) (map-elt row 'sessionId))))
             (if (and (assq 'fork (map-elt caps 'sessionCapabilities))
                      (assq 'delete (map-elt caps 'sessionCapabilities)))
                 (send (acp-make-session-fork-request :session-id (map-elt row 'sessionId) :cwd cwd :mcp-servers [])
                       (lambda (result)
                         (setq snapshot (map-elt result 'sessionId))
                         (if cancelled (stop nil) (read-history snapshot))) #'failed)
               ;; OpenCode, for example, supports load but not delete.  Do not
               ;; create permanent forks just to inspect its histories.
               (read-history (map-elt row 'sessionId))))))
         (page (cursor)
           (send `((:method . "session/list") (:params (cwd . ,cwd) ,@(when cursor `((cursor . ,cursor)))))
                 (lambda (response)
                   (cond (cancelled (stop nil))
                         (t
                          (let ((rows (append (map-elt response 'sessions) nil)))
                            (pcase-let ((`(,head ,tail ,count)
                                         (agent-shell-fork-tree--append-page
                                          listed listed-tail listed-count rows)))
                              (setq listed head listed-tail tail listed-count count)))
                          (publish (format "Discovering sessions · %d candidates" listed-count))
                          (if-let* ((next-cursor (map-elt response 'nextCursor)))
                              (page next-cursor)
                            (unless (seq-find (lambda (s) (equal focus (map-elt s 'sessionId))) listed)
                              (push `((sessionId . ,focus) (title . ,(buffer-name source))) listed))
                            (setq listed (seq-filter (lambda (s) (or (not (map-elt s 'cwd))
                                                                     (equal cwd (directory-file-name (map-elt s 'cwd))))) listed))
                            (let ((related (mapcar #'agent-shell-fork-tree--session-id (agent-shell-fork-tree--related store focus))))
                              (setq listed (agent-shell-fork-tree--prioritize listed focus related)))
                            (setq total (length listed) queue listed)
                            (next))))) #'failed))
         (ready (&optional _)
           (if cancelled (stop nil) (page nil))))
      (acp-subscribe-to-notifications
       :client client :buffer worker
       :on-notification (lambda (notification)
                          (when (and reading (equal reading (map-nested-elt notification '(params sessionId))))
                            (push (map-nested-elt notification '(params update)) updates))))
      (acp-subscribe-to-errors :client client :buffer worker :on-error (lambda (error) (stop (map-elt error 'message))))
      (publish "Discovering project sessions…")
      (send (acp-make-initialize-request :protocol-version 1)
            (lambda (response)
              (setq caps (map-elt response 'agentCapabilities))
              (unless (and (eq t (map-elt caps 'loadSession)) (assq 'list (map-elt caps 'sessionCapabilities)))
                (error "Agent must advertise session/list and session/load"))
              (if (map-elt config :needs-authentication)
                  (send (funcall (map-elt config :authenticate-request-maker)) #'ready #'failed)
                (ready))) #'failed)
      (lambda () (setq cancelled t)))))

(provide 'agent-shell-fork-tree-acp)
;;; agent-shell-fork-tree-acp.el ends here
