;;; agent-shell-fork-tree.el --- Cached conversation branches for agent-shell -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.75.2") (acp "0.14.3") (markdown-mode "2.4"))
;; Keywords: tools, convenience
;;; Commentary:
;; Discover existing ACP sessions and browse the current conversation's shared
;; history.  g updates from cached checkpoints; C-u g rebuilds into a fresh cache.
;; RET continues a native session, f forks it.  Historical turns are previews.
;;; Code:
(require 'agent-shell-fork-tree-store)
(require 'agent-shell-fork-tree-acp)
(require 'markdown-mode)
(require 'diff)

(defcustom agent-shell-fork-tree-auto-rebuild nil
  "Automatically rebuild on opening and after turns while a tree is visible."
  :type 'boolean :group 'agent-shell-fork-tree)
(defcustom agent-shell-fork-tree-preview-action
  '((display-buffer-reuse-window display-buffer-in-side-window)
    (side . right) (slot . 1) (window-width . 0.45))
  "Where to show the selected turn's preview."
  :type 'sexp :group 'agent-shell-fork-tree)
(defcustom agent-shell-fork-tree-message-truncation 80
  "Maximum display width of conversation text in a tree row.
When non-nil, longer prompts are shortened with an ellipsis.  This only
affects tree rows; search and previews keep the complete conversation text.
Custom node labels are always shown in full."
  :type '(choice (const :tag "Do not truncate" nil)
                 (natnum :tag "Display columns"))
  :group 'agent-shell-fork-tree)

(defvar agent-shell-fork-tree--stores (make-hash-table :test #'equal))
(defvar-local agent-shell-fork-tree--store nil)
(defvar-local agent-shell-fork-tree--source nil)
(defvar-local agent-shell-fork-tree--focus nil)
(defvar-local agent-shell-fork-tree--selected 0)
(defvar-local agent-shell-fork-tree--selected-session nil)
(defvar-local agent-shell-fork-tree--preview nil)
(defvar-local agent-shell-fork-tree--preview-key nil)
(defvar-local agent-shell-fork-tree--rendered-revision -1)
(defvar-local agent-shell-fork-tree--rendered-state nil)
(defvar-local agent-shell-fork-tree--positions nil)
(defvar-local agent-shell-fork-tree--visible-nodes nil)
(defvar-local agent-shell-fork-tree--session-count 0)
(defvar-local agent-shell-fork-tree--diff nil)
(defvar-local agent-shell-fork-tree--loading nil)
(defvar-local agent-shell-fork-tree--cancel nil)
(defvar-local agent-shell-fork-tree--status "Press g to discover sessions")
(defvar-local agent-shell-fork-tree--auto-timer nil)
(defvar-local agent-shell-fork-tree--subscription nil)

(defvar agent-shell-fork-tree-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (dolist (binding '(("g" . agent-shell-fork-tree-rebuild) ("a" . agent-shell-fork-tree-toggle-auto)
                       ("n" . next-line) ("p" . previous-line) ("j" . next-line) ("k" . previous-line)
                       ("h" . agent-shell-fork-tree-parent) ("l" . agent-shell-fork-tree-child)
                       ("/" . agent-shell-fork-tree-find) ("s" . agent-shell-fork-tree-label)
                       ("d" . agent-shell-fork-tree-toggle-diff) ("RET" . agent-shell-fork-tree-continue)
                       ("f" . agent-shell-fork-tree-fork) ("q" . agent-shell-fork-tree-quit)
                       ("C-c C-k" . agent-shell-fork-tree-cancel)))
      (define-key map (kbd (car binding)) (cdr binding)))
    map))

(define-derived-mode agent-shell-fork-tree-view-mode special-mode "Fork-Tree"
  "Browse related conversation histories and native session endpoints."
  (setq-local truncate-lines t)
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'agent-shell-fork-tree--follow nil t)
  (add-hook 'kill-buffer-hook #'agent-shell-fork-tree-cancel nil t))

(defun agent-shell-fork-tree--visible ()
  "Return current conversation's visible node IDs as a hash set."
  (or (and (= agent-shell-fork-tree--rendered-revision
              (agent-shell-fork-tree--store-revision agent-shell-fork-tree--store))
           agent-shell-fork-tree--visible-nodes)
      (setq agent-shell-fork-tree--visible-nodes
            (let ((ids (make-hash-table :test #'eql)))
              (puthash 0 t ids)
              (dolist (session (agent-shell-fork-tree--related agent-shell-fork-tree--store agent-shell-fork-tree--focus))
                (dolist (record (agent-shell-fork-tree--session-path session)) (puthash (map-elt record 'node) t ids)))
              ids))))

(defun agent-shell-fork-tree--update-header ()
  "Update progress without touching tree text or the preview."
  (setq header-line-format
        (format " Fork tree · %d sessions · auto %s · %s "
                agent-shell-fork-tree--session-count
                (if agent-shell-fork-tree-auto-rebuild "on" "off")
                agent-shell-fork-tree--status))
  (force-mode-line-update))

(defun agent-shell-fork-tree--view-state (&optional sessions)
  "Describe the displayed endpoints, excluding timestamps and message aliases.
SESSIONS, when supplied, is the sorted list of related sessions.  Paths within
a store are append-only, so the endpoint identifies the displayed path."
  (list agent-shell-fork-tree--store agent-shell-fork-tree--focus
        agent-shell-fork-tree-message-truncation
        (mapcar (lambda (session)
                  (list (agent-shell-fork-tree--session-id session)
                        (agent-shell-fork-tree--session-title session)
                        (agent-shell-fork-tree--session-coverage session)
                        (agent-shell-fork-tree--session-dirty session)
                        (agent-shell-fork-tree--last session)))
                (or sessions
                    (sort (agent-shell-fork-tree--related agent-shell-fork-tree--store agent-shell-fork-tree--focus)
                          (lambda (a b) (string< (agent-shell-fork-tree--session-id a)
                                                 (agent-shell-fork-tree--session-id b))))))))

(defun agent-shell-fork-tree--progress (store status)
  "Show STATUS; redraw only when committed, visible content changed."
  (setq agent-shell-fork-tree--status status)
  (agent-shell-fork-tree--update-header)
  (when (and (eq store agent-shell-fork-tree--store)
             (/= agent-shell-fork-tree--rendered-revision
                 (agent-shell-fork-tree--store-revision store))
             (get-buffer-window (current-buffer) t))
    (unless (equal agent-shell-fork-tree--rendered-state (agent-shell-fork-tree--view-state))
      (agent-shell-fork-tree--render))
    (setq agent-shell-fork-tree--rendered-revision (agent-shell-fork-tree--store-revision store))))

(defun agent-shell-fork-tree--replace-content (source)
  "Reconcile SOURCE's keyed rows without diffing individual characters.
Unchanged rows retain their text, properties and markers.  Rows moved across
other rows are deleted and reinserted; the renderer restores window anchors."
  (let ((inhibit-read-only t) (inhibit-redisplay t)
        (remaining (make-hash-table :test #'equal)) old rows)
    (cl-labels ((key (line)
                  (let ((session (get-text-property (point) 'fork-tree-session)))
                    (if-let* ((id (get-text-property (point) 'fork-tree-node)))
                        (cons (unless session id) session)
                      (- line)))))
      (save-excursion
        (goto-char (point-min))
        (let ((line 0))
          (while (< (point) (point-max))
            (let ((key (key (cl-incf line))))
              (push key old)
              (puthash key t remaining))
            (forward-line 1))))
      (setq old (nreverse old))
      (with-current-buffer source
        (save-excursion
          (goto-char (point-min))
          (let ((line 0))
            (while (< (point) (point-max))
              (let ((key (key (cl-incf line))) (start (point)))
                (forward-line 1)
                (push (cons key (buffer-substring start (point))) rows)))))))
    (goto-char (point-min))
    (dolist (row (nreverse rows))
      (let ((key (car row)) (text (cdr row)))
        (when (gethash key remaining)
          (let ((start (point)))
            (while (and old (not (equal key (car old))))
              (remhash (pop old) remaining)
              (forward-line 1))
            (delete-region start (point))))
        (if (equal key (car old))
            (let ((end (save-excursion (forward-line 1) (point))))
              (unless (equal-including-properties text (buffer-substring (point) end))
                (delete-region (point) end)
                (insert text)
                (setq end (point)))
              (goto-char end)
              (remhash (pop old) remaining))
          (insert text))))
    (delete-region (point) (point-max))))

(defun agent-shell-fork-tree--node-text (node)
  "Return NODE's single-line text for display in the tree."
  (let* ((label (agent-shell-fork-tree--node-label node))
         (text (replace-regexp-in-string
                "[\n\r]+" " " (or label (agent-shell-fork-tree--node-prompt node) ""))))
    (if (or label (null agent-shell-fork-tree-message-truncation))
        text
      (truncate-string-to-width
       text agent-shell-fork-tree-message-truncation nil nil "…"))))

(defun agent-shell-fork-tree--render ()
  "Draw the current conversation, preserving its endpoint and window positions."
  (let* ((view (current-buffer)) (store agent-shell-fork-tree--store)
         (sessions (sort (agent-shell-fork-tree--related store agent-shell-fork-tree--focus)
                         (lambda (a b) (string< (agent-shell-fork-tree--session-id a) (agent-shell-fork-tree--session-id b)))))
         (visible (make-hash-table :test #'eql))
         (active (make-hash-table :test #'eql))
         (endpoints (make-hash-table :test #'eql))
         (selected agent-shell-fork-tree--selected)
         (selected-session agent-shell-fork-tree--selected-session)
         (focus agent-shell-fork-tree--focus)
         (positions (make-hash-table :test #'equal))
         (windows (mapcar
                   (lambda (window)
                     (let* ((start (window-start window))
                            (session (get-text-property start 'fork-tree-session)))
                       (list window
                             (cons (unless session (get-text-property start 'fork-tree-node)) session)
                             (window-hscroll window) (window-vscroll window t))))
                   (get-buffer-window-list view nil t)))
         position)
    (puthash 0 t visible)
    ;; Index paths and endpoints once, not once for every displayed node.
    (dolist (session (reverse sessions))
      (let ((endpoint 0) (current (equal focus (agent-shell-fork-tree--session-id session))))
        (dolist (record (agent-shell-fork-tree--session-path session))
          (setq endpoint (map-elt record 'node))
          (puthash endpoint t visible)
          (when current (puthash endpoint t active)))
        (push session (gethash endpoint endpoints))))
    (unless (gethash selected visible)
      (setq selected 0 selected-session nil))
    (with-temp-buffer
      (insert (propertize "g update · C-u g full · a auto · RET continue · f fork · / find · d diff · s label · q close\n\n" 'face 'shadow))
      (let ((stack '((0 "" "" t))))
        (while stack
          (pcase-let* ((`(,id ,prefix ,edge ,last) (pop stack))
                       (node (agent-shell-fork-tree--node store id))
                       (children (seq-filter (lambda (n) (gethash n visible)) (agent-shell-fork-tree--node-children node)))
                       (final-child (car (last children)))
                       (branching (cdr children))
                       (start (point)))
            (insert prefix edge (if (gethash id active) "● " "○ ")
                    (propertize (format "%03d  " id) 'face 'shadow)
                    (agent-shell-fork-tree--node-text node) "\n")
            (add-text-properties start (point) (list 'fork-tree-node id))
            (puthash (cons id nil) start positions)
            (when (and (null selected-session) (= id selected)) (setq position start))
            (dolist (session (gethash id endpoints))
              (let* ((start (point)) (sid (agent-shell-fork-tree--session-id session)))
                (insert prefix "  ↳ " (propertize (replace-regexp-in-string "[\n\r]+" " " (agent-shell-fork-tree--session-title session)) 'face 'font-lock-function-name-face)
                        "  [" sid "]" (if (equal sid focus) "  current" "")
                        (cond ((agent-shell-fork-tree--session-dirty session) "  needs update")
                              ((not (eq 'full (agent-shell-fork-tree--session-coverage session))) "  partial history") (t "")) "\n")
                (add-text-properties start (point) (list 'fork-tree-node id 'fork-tree-session sid))
                (puthash (cons nil sid) start positions)
                (when (equal sid selected-session)
                  (setq position start selected id))))
            (dolist (child (reverse children))
              (let ((child-last (equal child final-child)))
                (push (list child (concat prefix (if (string-empty-p edge) "" (if last "   " "│  ")))
                            (if branching (if child-last "└─ " "├─ ") "") child-last) stack))))))
      (let ((content (current-buffer)))
        (with-current-buffer view
          (agent-shell-fork-tree--replace-content content))))
    (setq agent-shell-fork-tree--selected selected
          agent-shell-fork-tree--selected-session selected-session
          agent-shell-fork-tree--session-count (length sessions)
          agent-shell-fork-tree--positions positions
          agent-shell-fork-tree--visible-nodes visible
          agent-shell-fork-tree--rendered-state (agent-shell-fork-tree--view-state sessions)
          agent-shell-fork-tree--rendered-revision (agent-shell-fork-tree--store-revision store))
    (agent-shell-fork-tree--update-header)
    (goto-char (or position (point-min)))
    (dolist (state windows)
      (when-let* ((start (gethash (nth 1 state) positions)))
        (set-window-start (car state) start t)
        (set-window-hscroll (car state) (nth 2 state))
        (set-window-vscroll (car state) (nth 3 state) t)))
    (when (get-buffer-window (current-buffer)) (agent-shell-fork-tree--preview))))

(defun agent-shell-fork-tree--follow ()
  "Preview the row selected by keyboard or mouse."
  (when-let* ((id (get-text-property (point) 'fork-tree-node)))
    (let ((session (get-text-property (point) 'fork-tree-session)))
      (unless (and (= id agent-shell-fork-tree--selected) (equal session agent-shell-fork-tree--selected-session))
        (setq agent-shell-fork-tree--selected id agent-shell-fork-tree--selected-session session)
        (agent-shell-fork-tree--preview)))))

(defun agent-shell-fork-tree--preview ()
  "Render the selected turn as Markdown or a historical diff."
  (let* ((node (agent-shell-fork-tree--node agent-shell-fork-tree--store agent-shell-fork-tree--selected))
         (session agent-shell-fork-tree--selected-session) (diff agent-shell-fork-tree--diff)
         (unchanged (and (buffer-live-p agent-shell-fork-tree--preview)
                         (eq node (nth 0 agent-shell-fork-tree--preview-key))
                         (equal session (nth 1 agent-shell-fork-tree--preview-key))
                         (eq diff (nth 2 agent-shell-fork-tree--preview-key))))
         (preview (or (and (buffer-live-p agent-shell-fork-tree--preview) agent-shell-fork-tree--preview)
                      (setq agent-shell-fork-tree--preview (generate-new-buffer "*Fork tree preview*")))))
    (unless unchanged
      (with-current-buffer preview
        (let ((inhibit-read-only t) (inhibit-redisplay t))
          (erase-buffer)
          (insert (format "# Turn %d\n\n" (agent-shell-fork-tree--node-id node)))
          (insert (if session (format "Session: %s\n\nRET continues · f forks the current endpoint.\n\n" session)
                    "Historical preview · select a ↳ session row to continue or fork.\n\n"))
          (if diff
              (let (found)
                (dolist (tool (agent-shell-fork-tree--node-tools node))
                  (dolist (change (map-elt (cdr tool) 'diffs))
                    (setq found t)
                    (insert (agent-shell-fork-tree--diff-text change) "\n")))
                (unless found (insert "No file diff reported for this turn.\n")))
            (insert "## You\n\n" (or (agent-shell-fork-tree--node-prompt node) "")
                    "\n\n## Assistant\n\n" (or (agent-shell-fork-tree--node-answer node) "") "\n")
            (dolist (tool (agent-shell-fork-tree--node-tools node))
              (insert (format "\nTool: %s [%s]\n" (map-elt (cdr tool) 'title) (map-elt (cdr tool) 'status)))))
          (unless (eq major-mode (if diff 'diff-mode 'gfm-view-mode))
            (if diff (diff-mode) (gfm-view-mode)))
          (setq-local truncate-lines nil buffer-read-only t)
          (visual-line-mode 1)
          (goto-char (point-min))))
      (setq agent-shell-fork-tree--preview-key (list node session diff)))
    (unless (get-buffer-window preview t)
      (display-buffer preview agent-shell-fork-tree-preview-action))))

(defun agent-shell-fork-tree--diff-text (change)
  "Generate a display-only unified diff for CHANGE."
  (let ((old (generate-new-buffer " *fork-old*")) (new (generate-new-buffer " *fork-new*"))
        (result (generate-new-buffer " *fork-diff*")))
    (unwind-protect
        (progn
          (with-current-buffer old (insert (or (map-elt change :old) "")))
          (with-current-buffer new (insert (or (map-elt change :new) "")))
          (diff-no-select old new "-u" 'noasync result)
          (with-current-buffer result
            (goto-char (point-min))
            (if (re-search-forward "^@@ " nil t)
                (concat "--- a/" (map-elt change :file) "\n+++ b/" (map-elt change :file) "\n"
                        (buffer-substring-no-properties (line-beginning-position)
                                                       (if (re-search-forward "^Diff finished" nil t) (line-beginning-position) (point-max))))
              "No differences.\n")))
      (mapc #'kill-buffer (list old new result)))))

(defun agent-shell-fork-tree--select (id &optional session)
  "Select visible node ID, or its SESSION endpoint, without rebuilding the tree."
  (let ((position (and (= agent-shell-fork-tree--rendered-revision
                          (agent-shell-fork-tree--store-revision agent-shell-fork-tree--store))
                       (hash-table-p agent-shell-fork-tree--positions)
                       (gethash (cons (unless session id) session) agent-shell-fork-tree--positions))))
    (setq agent-shell-fork-tree--selected id
          agent-shell-fork-tree--selected-session session)
    (if position
        (progn
          (goto-char position)
          (when (get-buffer-window (current-buffer)) (agent-shell-fork-tree--preview)))
      (agent-shell-fork-tree--render))))

(defun agent-shell-fork-tree-parent ()
  "Select the parent turn."
  (interactive)
  (when-let* ((parent (agent-shell-fork-tree--node-parent (agent-shell-fork-tree--node agent-shell-fork-tree--store agent-shell-fork-tree--selected))))
    (agent-shell-fork-tree--select parent)))

(defun agent-shell-fork-tree-child ()
  "Select the first visible child."
  (interactive)
  (let ((visible (agent-shell-fork-tree--visible)))
    (when-let* ((child (seq-find (lambda (id) (gethash id visible))
                                (agent-shell-fork-tree--node-children (agent-shell-fork-tree--node agent-shell-fork-tree--store agent-shell-fork-tree--selected)))))
      (agent-shell-fork-tree--select child))))

(defun agent-shell-fork-tree-find ()
  "Find a turn in the current conversation only."
  (interactive)
  (let* ((choices (mapcar (lambda (id)
                            (let ((node (agent-shell-fork-tree--node agent-shell-fork-tree--store id)))
                              (cons (format "%03d %s" id (or (agent-shell-fork-tree--node-label node) (agent-shell-fork-tree--node-prompt node))) id)))
                          (sort (hash-table-keys (agent-shell-fork-tree--visible)) #'<)))
         (choice (completing-read "Find turn: " choices nil t)))
    (agent-shell-fork-tree--select (cdr (assoc choice choices)))))

(defun agent-shell-fork-tree-label (text)
  "Label the selected turn with TEXT; empty TEXT clears it."
  (interactive "sLabel: ")
  (setf (agent-shell-fork-tree--node-label (agent-shell-fork-tree--node agent-shell-fork-tree--store agent-shell-fork-tree--selected))
        (unless (string-empty-p text) text))
  (agent-shell-fork-tree--save agent-shell-fork-tree--store)
  (agent-shell-fork-tree--render))

(defun agent-shell-fork-tree-toggle-diff ()
  "Toggle historical diff preview."
  (interactive)
  (setq agent-shell-fork-tree--diff (not agent-shell-fork-tree--diff))
  (agent-shell-fork-tree--preview))

(defun agent-shell-fork-tree--transfer-labels (old fresh)
  "Carry labels from OLD to equivalent paths in FRESH."
  (dolist (node (hash-table-values (agent-shell-fork-tree--store-nodes old)))
    (when (agent-shell-fork-tree--node-label node)
      (let ((cursor node) fingerprints (target (agent-shell-fork-tree--node fresh 0)))
        (while (agent-shell-fork-tree--node-parent cursor)
          (push (agent-shell-fork-tree--node-fingerprint cursor) fingerprints)
          (setq cursor (agent-shell-fork-tree--node old (agent-shell-fork-tree--node-parent cursor))))
        (dolist (fingerprint fingerprints)
          (when target (setq target (gethash (cons (agent-shell-fork-tree--node-id target) fingerprint)
                                            (agent-shell-fork-tree--store-by-text fresh)))))
        (when target (setf (agent-shell-fork-tree--node-label target) (agent-shell-fork-tree--node-label node)))))))

(defun agent-shell-fork-tree--offer-full (view error-text)
  "Tell the user ERROR-TEXT and offer exactly one full rebuild of VIEW."
  (when (buffer-live-p view)
    (with-current-buffer view
      (message "Fork tree incremental rebuild failed: %s" error-text)
      (when (y-or-n-p "Incremental rebuild failed. Re-read all project histories and rebuild? ")
        (agent-shell-fork-tree-rebuild t)))))

(defun agent-shell-fork-tree-rebuild (&optional full)
  "Incrementally discover sessions.  With prefix FULL, rebuild a fresh cache."
  (interactive "P")
  (when agent-shell-fork-tree--loading (user-error "A rebuild is running; C-c C-k cancels it"))
  (unless (buffer-live-p agent-shell-fork-tree--source) (user-error "Open this tree from a live agent-shell"))
  (when (with-current-buffer agent-shell-fork-tree--source (shell-maker-busy))
    (user-error "Wait for the current turn to finish before reading history"))
  (let* ((view (current-buffer)) (old agent-shell-fork-tree--store)
         (working (if full (agent-shell-fork-tree--new-store (agent-shell-fork-tree--store-agent old) (agent-shell-fork-tree--store-cwd old)) old)))
    (setq agent-shell-fork-tree--loading t)
    (let ((cancel
           (agent-shell-fork-tree--scan
            agent-shell-fork-tree--source working agent-shell-fork-tree--focus full
            (lambda (store status)
              (when (buffer-live-p view)
                (with-current-buffer view
                  (agent-shell-fork-tree--progress store (concat (when full "Full rebuild · ") status)))))
            (lambda (result error-text cancelled)
              (when (and (not error-text) (not cancelled))
                (when full (agent-shell-fork-tree--transfer-labels old result))
                (when full (agent-shell-fork-tree--save result))
                (puthash (agent-shell-fork-tree--store-key result) result agent-shell-fork-tree--stores))
              (when (and full (not error-text) (not cancelled))
                (dolist (other (buffer-list))
                  (with-current-buffer other
                    (when (and (eq major-mode 'agent-shell-fork-tree-view-mode) (eq agent-shell-fork-tree--store old))
                      (setq agent-shell-fork-tree--store result agent-shell-fork-tree--selected 0 agent-shell-fork-tree--selected-session nil)
                      (unless (eq other view) (agent-shell-fork-tree--render))))))
              (when (buffer-live-p view)
                (with-current-buffer view
                  (setq agent-shell-fork-tree--loading nil agent-shell-fork-tree--cancel nil
                        agent-shell-fork-tree--status (cond (cancelled "Cancelled · g continues")
                                                           (error-text (concat "Failed: " error-text)) (t "Up to date")))
                  (when (and full (not error-text) (not cancelled))
                    (setq agent-shell-fork-tree--store result agent-shell-fork-tree--selected 0 agent-shell-fork-tree--selected-session nil))
                  (agent-shell-fork-tree--render)))
              (when (and error-text (not cancelled))
                (if full (message "Full rebuild failed; previous cache retained: %s" error-text)
                  (run-at-time 0 nil #'agent-shell-fork-tree--offer-full view error-text)))))))
      (when agent-shell-fork-tree--loading (setq agent-shell-fork-tree--cancel cancel)))))

(defun agent-shell-fork-tree-cancel ()
  "Cancel discovery after its pending request, retaining confirmed checkpoints."
  (interactive)
  (when agent-shell-fork-tree--auto-timer (cancel-timer agent-shell-fork-tree--auto-timer))
  (when agent-shell-fork-tree--cancel
    (funcall agent-shell-fork-tree--cancel)
    (setq agent-shell-fork-tree--status "Cancelling…")
    (agent-shell-fork-tree--update-header)))

(defun agent-shell-fork-tree-toggle-auto ()
  "Toggle automatic rebuilding globally."
  (interactive)
  (setq agent-shell-fork-tree-auto-rebuild (not agent-shell-fork-tree-auto-rebuild))
  (agent-shell-fork-tree--render)
  (when (and agent-shell-fork-tree-auto-rebuild (not agent-shell-fork-tree--loading)) (agent-shell-fork-tree-rebuild)))

(defun agent-shell-fork-tree--event (event)
  "Invalidate cached history and schedule visible trees after EVENT."
  (let ((sid (map-nested-elt agent-shell--state '(:session :id))))
    (when (memq (map-elt event :event) '(input-submitted turn-complete init-finished))
      (dolist (view (buffer-list))
        (with-current-buffer view
          (when (and (eq major-mode 'agent-shell-fork-tree-view-mode) agent-shell-fork-tree--store
                     (or (equal sid agent-shell-fork-tree--focus)
                         (gethash sid (agent-shell-fork-tree--store-sessions agent-shell-fork-tree--store))))
            (when-let* ((session (gethash sid (agent-shell-fork-tree--store-sessions agent-shell-fork-tree--store))))
              (setf (agent-shell-fork-tree--session-dirty session) t))
            (when (and agent-shell-fork-tree-auto-rebuild (get-buffer-window view)
                       (not agent-shell-fork-tree--loading)
                       (memq (map-elt event :event) '(turn-complete init-finished)))
              (when agent-shell-fork-tree--auto-timer (cancel-timer agent-shell-fork-tree--auto-timer))
              (setq agent-shell-fork-tree--auto-timer
                    (run-at-time 0.1 nil (lambda ()
                                          (when (buffer-live-p view)
                                            (with-current-buffer view
                                              (when (and agent-shell-fork-tree-auto-rebuild (get-buffer-window view)
                                                         (not agent-shell-fork-tree--loading)
                                                         (not (with-current-buffer agent-shell-fork-tree--source (shell-maker-busy))))
                                                (agent-shell-fork-tree-rebuild))))))))))))))

;;;###autoload
(define-minor-mode agent-shell-fork-tree-mode
  "Track changes for fork-tree caches in this agent-shell."
  :lighter " Forks"
  (if agent-shell-fork-tree-mode
      (unless agent-shell-fork-tree--subscription
        (setq agent-shell-fork-tree--subscription
              (agent-shell-subscribe-to :shell-buffer (current-buffer) :on-event #'agent-shell-fork-tree--event)))
    (agent-shell-unsubscribe :subscription agent-shell-fork-tree--subscription)
    (setq agent-shell-fork-tree--subscription nil)))

(defun agent-shell-fork-tree--session-config (config store endpoint)
  "Wrap CONFIG to attach a fork on its creating client and report restore errors."
  (let ((copy (copy-tree config)) (maker (map-elt config :client-maker)))
    (setf (map-elt copy :client-maker)
          (lambda (buffer)
            (let* ((client (funcall maker buffer)) (send (map-elt client :request-sender)) caps)
              (setf (map-elt client :request-sender)
                    (lambda (&rest args)
                      (let* ((request (plist-get args :request)) (method (map-elt request :method)) (success (plist-get args :on-success)))
                        (when (equal method "initialize")
                          (setq args (plist-put args :on-success (lambda (response) (setq caps (map-elt response 'agentCapabilities)) (funcall success response)))))
                        (when (member method '("session/load" "session/resume"))
                          (setq args (plist-put args :on-failure (agent-shell--make-error-handler :state (buffer-local-value 'agent-shell--state buffer) :shell-buffer buffer))))
                        (when (equal method "session/fork")
                          (setq args (plist-put args :on-success
                                               (lambda (fork)
                                                 (let* ((id (map-elt fork 'sessionId))
                                                        (attached (lambda (_)
                                                                    (let* ((session (copy-agent-shell-fork-tree--session endpoint))
                                                                           (path (copy-tree (agent-shell-fork-tree--session-path endpoint))))
                                                                      (setf (agent-shell-fork-tree--session-id session) id
                                                                            (agent-shell-fork-tree--session-title session) (concat (agent-shell-fork-tree--session-title endpoint) " (fork)")
                                                                            (agent-shell-fork-tree--session-updated session) nil
                                                                            (agent-shell-fork-tree--session-path session) path
                                                                            (agent-shell-fork-tree--session-path-tail session) (last path))
                                                                      (puthash id session
                                                                               (agent-shell-fork-tree--store-sessions store)))
                                                                    (funcall success fork)))
                                                        (method (cond ((assq 'resume (map-elt caps 'sessionCapabilities)) "session/resume")
                                                                      ((eq t (map-elt caps 'loadSession)) "session/load"))))
                                                   (if method
                                                       (acp-send-request :client client :buffer buffer
                                                                         :request `((:method . ,method) (:params (sessionId . ,id) (cwd . ,(agent-shell-fork-tree--store-cwd store)) (mcpServers . [])))
                                                                         :on-success attached :on-failure (plist-get args :on-failure))
                                                     (funcall attached nil)))))))
                        (apply send args))))
              client)))
    copy))

(defun agent-shell-fork-tree--visit (fork)
  "Continue the selected endpoint, or create a FORK of it."
  (let* ((sid agent-shell-fork-tree--selected-session)
         (endpoint (and sid (gethash sid (agent-shell-fork-tree--store-sessions agent-shell-fork-tree--store))))
         (state (buffer-local-value 'agent-shell--state agent-shell-fork-tree--source)))
    (unless endpoint (user-error "Select a ↳ session row; historical turns are preview-only"))
    (when (or (agent-shell-fork-tree--session-dirty endpoint) (not (eq 'full (agent-shell-fork-tree--session-coverage endpoint))))
      (user-error "Press g to update this endpoint first"))
    (when (and fork (not (map-elt state :supports-session-fork))) (user-error "Agent does not advertise session/fork"))
    (let* ((default-directory (file-name-as-directory (agent-shell-fork-tree--store-cwd agent-shell-fork-tree--store)))
           (existing (unless fork (seq-find (lambda (buffer) (equal sid (map-nested-elt (buffer-local-value 'agent-shell--state buffer) '(:session :id)))) (agent-shell-buffers))))
           (shell (or existing
                      (progn
                        (unless (or fork (map-elt state :supports-session-load) (map-elt state :supports-session-resume))
                          (user-error "Agent does not advertise session/load or session/resume"))
                        (agent-shell--start
                                :config (agent-shell-fork-tree--session-config (map-elt state :agent-config) agent-shell-fork-tree--store endpoint)
                                :new-session t :session-strategy 'new :no-focus t
                                :session-id (unless fork sid) :fork-session-id (when fork sid))))))
      (with-current-buffer shell (agent-shell-fork-tree-mode 1))
      (pop-to-buffer shell))))

(defun agent-shell-fork-tree-continue ()
  "Continue the selected native session."
  (interactive) (agent-shell-fork-tree--visit nil))
(defun agent-shell-fork-tree-fork ()
  "Fork the selected native session at its current endpoint."
  (interactive) (agent-shell-fork-tree--visit t))

(defun agent-shell-fork-tree-quit ()
  "Cancel discovery and close this tree and its preview."
  (interactive)
  (agent-shell-fork-tree-cancel)
  (when (buffer-live-p agent-shell-fork-tree--preview)
    (dolist (window (get-buffer-window-list agent-shell-fork-tree--preview nil t)) (quit-window nil window))
    (kill-buffer agent-shell-fork-tree--preview))
  (quit-window))

;;;###autoload
(defun agent-shell-fork-tree ()
  "Browse the current agent-shell's related conversation branches."
  (interactive)
  (let* ((source (or agent-shell-fork-tree--source (agent-shell--current-shell) (agent-shell--shell-buffer :no-create t)))
         (state (buffer-local-value 'agent-shell--state source))
         (focus (or (map-nested-elt state '(:session :id)) (user-error "Wait for the session to start")))
         (agent (symbol-name (map-nested-elt state '(:agent-config :identifier))))
         (cwd (with-current-buffer source (directory-file-name (file-truename (agent-shell--resolve-path (agent-shell-cwd))))))
         (key (secure-hash 'sha256 (format "%s:%s" agent cwd)))
         (store (or (gethash key agent-shell-fork-tree--stores)
                    (puthash key (agent-shell-fork-tree--load agent cwd) agent-shell-fork-tree--stores)))
         (view (or (seq-find (lambda (b) (and (eq (buffer-local-value 'major-mode b) 'agent-shell-fork-tree-view-mode)
                                              (equal focus (buffer-local-value 'agent-shell-fork-tree--focus b))
                                              (eq store (buffer-local-value 'agent-shell-fork-tree--store b)))) (buffer-list))
                   (generate-new-buffer (format "*Fork tree: %s*" (buffer-name source))))))
    (with-current-buffer source (agent-shell-fork-tree-mode 1))
    (pop-to-buffer view)
    (unless (derived-mode-p 'agent-shell-fork-tree-view-mode) (agent-shell-fork-tree-view-mode))
    (setq agent-shell-fork-tree--store store agent-shell-fork-tree--source source
          agent-shell-fork-tree--focus focus agent-shell-fork-tree--selected-session focus)
    (agent-shell-fork-tree--render)
    (when (and agent-shell-fork-tree-auto-rebuild (not agent-shell-fork-tree--loading)) (agent-shell-fork-tree-rebuild))))

(provide 'agent-shell-fork-tree)
;;; agent-shell-fork-tree.el ends here
