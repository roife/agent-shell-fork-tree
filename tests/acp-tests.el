;;; acp-tests.el --- ACP cancellation and capability tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'agent-shell-fork-tree)

(defmacro aft-test-transport (&rest body)
  (declare (indent 0))
  `(let* ((source (generate-new-buffer " *aft-transport-source*"))
          (store (agent-shell-fork-tree--new-store "fixture" "/tmp"))
          (agent-shell-fork-tree-cache-directory nil)
          requests pending cancel result finished callback client worker
          (caps '((loadSession . t) (sessionCapabilities (list) (fork) (delete)))))
     (unwind-protect
         (cl-letf (((symbol-function 'acp-subscribe-to-notifications)
                    (lambda (&rest args) (setq callback (plist-get args :on-notification))))
                   ((symbol-function 'acp-send-request)
                    (lambda (&rest args)
                      (setq client (plist-get args :client) worker (plist-get args :buffer))
                      (unless (map-elt client :process)
                        (setf (map-elt client :process) (make-pipe-process :name "aft-transport" :noquery t)))
                      (push (plist-get args :request) requests)
                      (with-current-buffer worker
                        (pcase (map-elt (plist-get args :request) :method)
                          ("initialize" (funcall (plist-get args :on-success) `((agentCapabilities . ,caps))))
                          ("session/list" (funcall (plist-get args :on-success) '((sessions . [((sessionId . "main") (title . "Main") (updatedAt . "1"))]))))
                          ("session/delete" (funcall (plist-get args :on-success) nil))
                          (_ (setq pending args)))))))
           (with-current-buffer source
             (setq-local agent-shell--state
                         (agent-shell--make-state
                          :buffer source :agent-config `((:identifier . fixture) (:client-maker . ,(lambda (b) (acp-make-client :command "fake" :context-buffer b)))))))
           (cl-labels ((start ()
                         (setq cancel (agent-shell-fork-tree--scan source store "main" nil #'ignore
                                                                  (lambda (_store error cancelled) (setq finished t result (list error cancelled))))))
                       (reply (response &optional failure)
                         (let ((args pending))
                           (setq pending nil)
                           (with-current-buffer (plist-get args :buffer)
                             (if failure
                                 (acp--call-request-failure :client client
                                                           :incoming-response `((:buffer . ,worker) (:on-failure . ,(plist-get args :on-failure)))
                                                           :error-data response :message nil)
                               (funcall (plist-get args :on-success) response)))))
                       (updates (rows)
                         (dolist (row rows)
                           (with-current-buffer worker
                             (funcall callback `((method . "session/update") (params (sessionId . "snapshot") (update . ,row))))))))
             ,@body))
       (when client (acp-shutdown :client client))
       (when (buffer-live-p worker) (kill-buffer worker))
       (kill-buffer source))))

(ert-deftest aft-test-cancelled-fork-deletes-only-late-owned-id ()
  (aft-test-transport
    (start)
    (funcall cancel)
    (reply '((sessionId . "snapshot")))
    (should finished)
    (should (equal '(nil t) result))
    (should (equal "session/delete" (map-elt (car requests) :method)))
    (should (equal "snapshot" (map-nested-elt (car requests) '(:params sessionId))))
    (should-not (seq-find (lambda (r) (equal "session/load" (map-elt r :method))) requests))
    (should-not (buffer-live-p worker))))

(ert-deftest aft-test-cancelled-replay-persists-only-completed-turns ()
  (aft-test-transport
    (start)
    (reply '((sessionId . "snapshot")))
    (updates (aft-test-updates '("one" "unfinished")))
    (funcall cancel)
    (reply nil)
    (should (equal '(nil t) result))
    (let ((session (gethash "main" (agent-shell-fork-tree--store-sessions store))))
      (should (= 1 (length (agent-shell-fork-tree--session-path session))))
      (should (eq 'partial (agent-shell-fork-tree--session-coverage session)))
      (should (agent-shell-fork-tree--session-dirty session)))))

(ert-deftest aft-test-rpc-failure-cleans-up-and-finishes-once ()
  (aft-test-transport
    (start)
    (reply '((sessionId . "snapshot")))
    (reply '((message . "Load rejected")) t)
    (should finished)
    (should (string-match-p "Load rejected" (car result)))
    (should (equal "session/delete" (map-elt (car requests) :method)))
    (should-not (buffer-live-p worker))))

(ert-deftest aft-test-missing-required-capability-fails-before-list ()
  (aft-test-transport
    (setq caps '((loadSession . t)))
    (start)
    (should finished)
    (should (string-match-p "advertise session/list" (car result)))
    (should (= 1 (length requests)))))

(ert-deftest aft-test-no-delete-capability-does-not-create-read-forks ()
  (aft-test-transport
    (setq caps '((loadSession . t) (sessionCapabilities (list) (fork) (close))))
    (start)
    (should (equal "session/load" (map-elt (plist-get pending :request) :method)))
    (reply nil)
    (should finished)
    (should-not (seq-find (lambda (r) (equal "session/fork" (map-elt r :method))) requests))))

(ert-deftest aft-test-current-ids-replace-checkpoint-aliases ()
  (let ((store (agent-shell-fork-tree--new-store "fixture" "/tmp")))
    (aft-test-add store "main" '("one") nil "old-")
    (aft-test-add store "main" '("one") "2" "new-")
    (cl-letf (((symbol-function 'agent-shell-fork-tree--fingerprint) (lambda (&rest _) (ert-fail "Rehashed refreshed checkpoint"))))
      (aft-test-add store "main" '("one") "3" "new-"))))

(ert-deftest aft-test-auto-events-only-schedule-visible-related-trees ()
  (let* ((source (generate-new-buffer " *aft-auto-source*"))
         (view (generate-new-buffer " *aft-auto-tree*"))
         (store (agent-shell-fork-tree--new-store "fixture" "/tmp"))
         (agent-shell-fork-tree-auto-rebuild nil)
         timer-callback (rebuilds 0))
    (unwind-protect
        (progn
          (aft-test-add store "main" '("one"))
          (with-current-buffer source
            (setq-local agent-shell--state (agent-shell--make-state :buffer source))
            (setf (map-elt (map-elt agent-shell--state :session) :id) "main"))
          (with-current-buffer view
            (agent-shell-fork-tree-view-mode)
            (setq agent-shell-fork-tree--store store agent-shell-fork-tree--focus "main" agent-shell-fork-tree--source source))
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) t))
                    ((symbol-function 'shell-maker-busy) (lambda () nil))
                    ((symbol-function 'agent-shell-fork-tree-rebuild) (lambda (&rest _) (cl-incf rebuilds)))
                    ((symbol-function 'run-at-time) (lambda (_time _repeat fn &rest _) (setq timer-callback fn) nil)))
            (with-current-buffer source (agent-shell-fork-tree--event '((:event . turn-complete))))
            (should-not timer-callback)
            (setq agent-shell-fork-tree-auto-rebuild t)
            (with-current-buffer source (agent-shell-fork-tree--event '((:event . turn-complete))))
            (should timer-callback)
            (funcall timer-callback)
            (should (= 1 rebuilds))))
      (kill-buffer view) (kill-buffer source))))
