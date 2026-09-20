;;; view-tests.el --- Redraw and preview regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'agent-shell-fork-tree)

(defmacro aft-test-view (&rest body)
  (declare (indent 0))
  `(let* ((store (agent-shell-fork-tree--new-store "fixture" "/tmp"))
          (view (generate-new-buffer " *aft-view*")))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer view)
           (agent-shell-fork-tree-view-mode)
           (aft-test-add store "main" (cl-loop for i below 100 collect (format "turn %d" i)))
           (setq agent-shell-fork-tree--store store
                 agent-shell-fork-tree--focus "main"
                 agent-shell-fork-tree--selected 50)
           (agent-shell-fork-tree--render)
           ,@body)
       (when (buffer-live-p view)
         (let ((preview (buffer-local-value 'agent-shell-fork-tree--preview view)))
           (when (buffer-live-p preview) (kill-buffer preview)))
         (kill-buffer view)))))

(ert-deftest aft-test-tree-message-truncation ()
  (let ((node (agent-shell-fork-tree--node-create
               :id 1 :prompt "abcdefghijk\nsecond line")))
    (let ((agent-shell-fork-tree-message-truncation 8))
      (should (equal "abcdefg…" (agent-shell-fork-tree--node-text node)))
      (setf (agent-shell-fork-tree--node-label node) "A deliberately long label")
      (should (equal "A deliberately long label"
                     (agent-shell-fork-tree--node-text node))))
    (setf (agent-shell-fork-tree--node-label node) nil)
    (let ((agent-shell-fork-tree-message-truncation nil))
      (should (equal "abcdefghijk second line"
                     (agent-shell-fork-tree--node-text node))))))

(ert-deftest aft-test-progress-does-not-rewrite-unchanged-buffers ()
  (aft-test-view
   (let ((tick (buffer-modified-tick))
         (preview agent-shell-fork-tree--preview))
     (with-current-buffer preview (goto-char (point-max)))
     (let ((preview-tick (buffer-modified-tick preview))
           (preview-point (with-current-buffer preview (point))))
       (dotimes (_ 20) (agent-shell-fork-tree--progress store "Reading"))
       (should (= tick (buffer-modified-tick)))
       (should (= preview-tick (buffer-modified-tick preview)))
       (should (= preview-point (with-current-buffer preview (point))))))))

(ert-deftest aft-test-progress-refreshes-only-visible-changes ()
  (aft-test-view
   (let ((tick (buffer-modified-tick)))
     (aft-test-add store "foreign" '("unrelated"))
     (agent-shell-fork-tree--progress store "Read unrelated")
     (should (= tick (buffer-modified-tick)))
     (aft-test-add store "branch" '("turn 0" "branch") nil "new-")
     (agent-shell-fork-tree--progress store "Read branch")
     (should (string-match-p "branch" (buffer-string)))
     (should (= 50 (get-text-property (point) 'fork-tree-node)))
     (should (= 2 agent-shell-fork-tree--session-count))
     (setq tick (buffer-modified-tick))
     (aft-test-add store "branch" '("turn 0" "branch") "2" "changed-ids-")
     (agent-shell-fork-tree--progress store "Updated aliases and timestamp")
     (should (= tick (buffer-modified-tick)))
     (dotimes (_ 20) (agent-shell-fork-tree--progress store "Reading"))
     (should (= tick (buffer-modified-tick))))))

(ert-deftest aft-test-redraw-retains-window-and-preview-positions ()
  (aft-test-view
   (let* ((window (get-buffer-window view))
          (preview agent-shell-fork-tree--preview)
          (preview-window (get-buffer-window preview)))
     (save-excursion
       (goto-char (point-min)) (forward-line 30)
       (set-window-start window (point) t))
     (with-current-buffer preview
       (goto-char (point-max))
       (set-window-start preview-window (line-beginning-position) t))
     (let ((top-node (get-text-property (window-start window) 'fork-tree-node))
           (preview-start (window-start preview-window))
           (preview-point (with-current-buffer preview (point)))
           (preview-tick (buffer-modified-tick preview)))
       (aft-test-add store "branch" '("turn 0" "branch") nil "new-")
       (agent-shell-fork-tree--render)
       (should (= top-node (get-text-property (window-start window) 'fork-tree-node)))
       (should (= preview-start (window-start preview-window)))
       (should (= preview-point (with-current-buffer preview (point))))
       (should (= preview-tick (buffer-modified-tick preview)))))))

(ert-deftest aft-test-preview-invalidates-on-mode-session-and-store-change ()
  (aft-test-view
   (let ((preview agent-shell-fork-tree--preview)
         (node (agent-shell-fork-tree--node store 50)))
     (setq agent-shell-fork-tree--selected-session "main")
     (agent-shell-fork-tree--preview)
     (with-current-buffer preview (should (string-match-p "Session: main" (buffer-string))))
     (agent-shell-fork-tree-toggle-diff)
     (with-current-buffer preview (should (eq major-mode 'diff-mode)))
     (agent-shell-fork-tree-toggle-diff)
     (let ((replacement (copy-agent-shell-fork-tree--node node)))
       (setf (agent-shell-fork-tree--node-answer replacement) "Replacement reply")
       (puthash 50 replacement (agent-shell-fork-tree--store-nodes store)))
     (agent-shell-fork-tree--preview)
     (with-current-buffer preview
       (should (eq major-mode 'gfm-view-mode))
       (should (string-match-p "Replacement reply" (buffer-string))))
     (kill-buffer preview)
     (agent-shell-fork-tree--preview)
     (should (buffer-live-p agent-shell-fork-tree--preview)))))

(ert-deftest aft-test-full-progress-does-not-render-uncommitted-store ()
  (aft-test-view
   (let ((fresh (agent-shell-fork-tree--new-store "fixture" "/tmp"))
         (tick (buffer-modified-tick)))
     (aft-test-add fresh "main" '("replacement"))
     (agent-shell-fork-tree--progress fresh "Full rebuild")
     (should (= tick (buffer-modified-tick)))
     (should (eq store agent-shell-fork-tree--store)))))

(ert-deftest aft-test-progress-does-not-use-timers ()
  (aft-test-view
   (cl-letf (((symbol-function 'run-at-time)
              (lambda (&rest _) (ert-fail "Progress scheduled a timer"))))
     (aft-test-add store "branch" '("turn 0" "branch"))
     (agent-shell-fork-tree--progress store "Read")
     (should (string-match-p "branch" (buffer-string))))))

(ert-deftest aft-test-replace-content-updates-properties-on-identical-text ()
  (with-temp-buffer
    (insert (propertize "Same row\n" 'fork-tree-node 1 'fork-tree-session "old"))
    (let ((target (current-buffer)))
      (with-temp-buffer
        (insert (propertize "Same row\n" 'fork-tree-node 2))
        (let ((source (current-buffer)))
          (with-current-buffer target
            (agent-shell-fork-tree--replace-content source)
            (should (= 2 (get-text-property (point-min) 'fork-tree-node)))
            (should-not (get-text-property (point-min) 'fork-tree-session))))))))

(ert-deftest aft-test-replace-content-insert-delete-reorder-and-markers ()
  (with-temp-buffer
    (dolist (id '(1 2 3 4))
      (insert (propertize (format "Row %d\n" id) 'fork-tree-node id)))
    (let ((target (current-buffer))
          (marker (copy-marker (point-min))))
      (unwind-protect
          (dolist (ids '((1 5 2 4) (4 1 5 2) (4 1) (1) nil (6 1)))
            (with-temp-buffer
              (dolist (id ids)
                (insert (propertize (format "Row %d\n" id) 'fork-tree-node id)))
              (let ((source (current-buffer)) (expected (buffer-string)))
                (with-current-buffer target
                  (agent-shell-fork-tree--replace-content source)
                  (should (equal-including-properties expected (buffer-string)))
                  (when (equal ids '(1 5 2 4))
                    (should (= 1 (get-text-property marker 'fork-tree-node))))))))
        (set-marker marker nil)))))

(ert-deftest aft-test-unchanged-render-does-not-modify-text ()
  (aft-test-view
   (let ((tick (buffer-modified-tick)))
     (agent-shell-fork-tree--render)
     (should (= tick (buffer-modified-tick))))))

(ert-deftest aft-test-navigation-reuses-rendered-position-index ()
  (aft-test-view
   (cl-letf (((symbol-function 'agent-shell-fork-tree--render)
              (lambda () (ert-fail "Navigation rebuilt an unchanged tree")))
             ((symbol-function 'agent-shell-fork-tree--preview) #'ignore))
     (agent-shell-fork-tree-parent)
     (should (= 49 agent-shell-fork-tree--selected))
     (should (= 49 (get-text-property (point) 'fork-tree-node)))
     (agent-shell-fork-tree-child)
     (should (= 50 agent-shell-fork-tree--selected))
     (should (= 50 (get-text-property (point) 'fork-tree-node))))))

(ert-deftest aft-test-session-selection-follows-new-endpoint ()
  (aft-test-view
   (setq agent-shell-fork-tree--selected-session "main")
   (agent-shell-fork-tree--render)
   (let ((window (get-buffer-window view)))
     (set-window-start window (point) t)
     (aft-test-add store "main"
                   (append (cl-loop for i below 100 collect (format "turn %d" i))
                           '("new endpoint")) "2")
     (agent-shell-fork-tree--progress store "Appended")
     (should (equal "main" (get-text-property (point) 'fork-tree-session)))
     (should (= 101 (get-text-property (point) 'fork-tree-node)))
     (should (equal "main" (get-text-property (window-start window) 'fork-tree-session)))
     (with-current-buffer agent-shell-fork-tree--preview
       (should (string-match-p "new endpoint" (buffer-string)))))))
