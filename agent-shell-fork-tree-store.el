;;; agent-shell-fork-tree-store.el --- History index and checkpoints -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Code:
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'json)
(require 'agent-shell)

(defgroup agent-shell-fork-tree nil "Browse related agent-shell sessions." :group 'agent-shell)
(defcustom agent-shell-fork-tree-cache-directory
  (locate-user-emacs-file "var/agent-shell-fork-tree/")
  "Private history cache directory, or nil to disable disk persistence."
  :type '(choice directory (const nil)) :group 'agent-shell-fork-tree)

(define-error 'agent-shell-fork-tree-incremental-error "Incremental history mismatch")
(cl-defstruct (agent-shell-fork-tree--node (:constructor agent-shell-fork-tree--node-create))
  id parent children prompt answer fingerprint tools label)
(cl-defstruct (agent-shell-fork-tree--session (:constructor agent-shell-fork-tree--session-create))
  id title updated path coverage dirty)
(cl-defstruct (agent-shell-fork-tree--store (:constructor agent-shell-fork-tree--store-create))
  key cwd agent nodes sessions by-id by-text (next 1))

(defun agent-shell-fork-tree--new-store (agent cwd)
  "Make an empty index for AGENT and CWD."
  (let ((store (agent-shell-fork-tree--store-create
                :key (secure-hash 'sha256 (format "%s:%s" agent cwd)) :agent agent :cwd cwd
                :nodes (make-hash-table :test #'eql) :sessions (make-hash-table :test #'equal)
                :by-id (make-hash-table :test #'equal) :by-text (make-hash-table :test #'equal))))
    (puthash 0 (agent-shell-fork-tree--node-create :id 0 :prompt "Beginning")
             (agent-shell-fork-tree--store-nodes store))
    store))

(defun agent-shell-fork-tree--fingerprint (prompt answer)
  "Hash exactly PROMPT and ANSWER, preserving the boundary between them."
  (secure-hash 'sha256 (json-serialize (vector prompt answer))))

(defun agent-shell-fork-tree--turns (updates complete)
  "Normalize ACP UPDATES into turns.  COMPLETE seals the last answered turn.
Message IDs delimit chunks only within this replay.  Tool data is preview-only."
  (let (turns prompt answer ids tools last-user last-kind have-user)
    (cl-labels ((seal ()
                 (when have-user
                   (push (list :prompt (apply #'concat (nreverse prompt))
                               :answer (apply #'concat (nreverse answer))
                               :ids (and ids (not (memq nil ids)) (nreverse ids))
                               :tools (nreverse tools)) turns))
                 (setq prompt nil answer nil ids nil tools nil have-user nil)))
      (dolist (update updates)
        (let* ((kind (map-elt update 'sessionUpdate))
               (id (map-elt update 'messageId))
               (content (map-elt update 'content)))
          (pcase kind
            ("user_message_chunk"
             (when (and have-user (or (not (equal last-kind kind)) (not (equal last-user id))))
               (seal))
             (unless have-user (push id ids))
             (setq have-user t last-user id last-kind kind)
             (push (or (map-elt content 'text) "") prompt))
            ("agent_message_chunk"
             (when have-user
               (unless (and (equal kind last-kind) (equal id (car ids))) (push id ids))
               (push (or (map-elt content 'text) "") answer)
               (setq last-kind kind)))
            ((or "tool_call" "tool_call_update")
             (when have-user
               (let* ((tool-id (map-elt update 'toolCallId))
                      (entry (assoc tool-id tools)))
                 (unless entry
                   (setq entry (cons tool-id (list (cons 'title nil) (cons 'status nil) (cons 'diffs nil))))
                   (push entry tools))
                 (dolist (field '(title status))
                   (when (assq field update) (setf (alist-get field (cdr entry)) (map-elt update field))))
                 (when-let* ((diffs (agent-shell--make-diff-infos :acp-tool-call update)))
                   (setf (alist-get 'diffs (cdr entry)) diffs)))
               (setq last-kind kind))))))
      (when (and complete answer) (seal)))
    (nreverse turns)))

(defun agent-shell-fork-tree--node (store id)
  "Return STORE node ID."
  (gethash id (agent-shell-fork-tree--store-nodes store)))

(defun agent-shell-fork-tree--attach (store parent turn)
  "Reuse or attach TURN below PARENT in STORE.  Return the node ID."
  (let* ((ids (plist-get turn :ids))
         (id-key (and ids (cons parent ids)))
         (node (and id-key (gethash id-key (agent-shell-fork-tree--store-by-id store))))
         (prompt (plist-get turn :prompt)) (answer (plist-get turn :answer)))
    ;; IDs are opaque and only session-unique in ACP.  Equal IDs with different
    ;; text must not merge unrelated sessions (e.g. agents using counters).
    (unless (and node (equal prompt (agent-shell-fork-tree--node-prompt node))
                 (equal answer (agent-shell-fork-tree--node-answer node)))
      (let* ((fingerprint (agent-shell-fork-tree--fingerprint prompt answer))
             (key (cons parent fingerprint)))
        (setq node (gethash key (agent-shell-fork-tree--store-by-text store)))
        (unless node
          (setq node (agent-shell-fork-tree--node-create
                      :id (agent-shell-fork-tree--store-next store) :parent parent
                      :prompt prompt :answer answer :fingerprint fingerprint
                      :tools (plist-get turn :tools)))
          (cl-incf (agent-shell-fork-tree--store-next store))
          (puthash (agent-shell-fork-tree--node-id node) node (agent-shell-fork-tree--store-nodes store))
          (puthash key node (agent-shell-fork-tree--store-by-text store))
          (let ((ancestor (agent-shell-fork-tree--node store parent)))
            (setf (agent-shell-fork-tree--node-children ancestor)
                  (append (agent-shell-fork-tree--node-children ancestor)
                          (list (agent-shell-fork-tree--node-id node))))))))
    (when id-key (puthash id-key node (agent-shell-fork-tree--store-by-id store)))
    (agent-shell-fork-tree--node-id node)))

(defun agent-shell-fork-tree--first (session)
  "Return SESSION's first history node, or nil for empty/unread history."
  (map-elt (car (agent-shell-fork-tree--session-path session)) 'node))

(defun agent-shell-fork-tree--related (store focus)
  "Return FOCUS and sessions sharing its nonempty prefix in STORE."
  (let* ((current (gethash focus (agent-shell-fork-tree--store-sessions store)))
         (first (and current (agent-shell-fork-tree--first current))))
    (seq-filter (lambda (session)
                  (or (equal focus (agent-shell-fork-tree--session-id session))
                      (and first (equal first (agent-shell-fork-tree--first session)))))
                (hash-table-values (agent-shell-fork-tree--store-sessions store)))))

(defun agent-shell-fork-tree--ingest (store info turns complete focus)
  "Validate cached prefix then append TURNS for INFO to STORE.
COMPLETE means load finished; FOCUS limits indexing of unrelated sessions
 to their first turn.  Old fingerprints are not recomputed for stable IDs."
  (let* ((id (map-elt info 'sessionId))
         (session (or (gethash id (agent-shell-fork-tree--store-sessions store))
                      (agent-shell-fork-tree--session-create :id id)))
         (path (copy-tree (agent-shell-fork-tree--session-path session)))
         (current (gethash focus (agent-shell-fork-tree--store-sessions store)))
         (focus-first (and current (agent-shell-fork-tree--first current))))
    (when (and complete (< (length turns) (length path)))
      (signal 'agent-shell-fork-tree-incremental-error (list (format "%s: history became shorter" id))))
    ;; Validate against saved node references, not by walking the tree or comparing
    ;; other sessions.  A partial replay ending before the checkpoint adds nothing.
    (cl-loop for record in path for turn in turns for index from 1 do
             (let* ((node (agent-shell-fork-tree--node store (map-elt record 'node)))
                    (same-ids (and (plist-get turn :ids) (equal (map-elt record 'ids) (plist-get turn :ids))))
                    (same-text (and (equal (plist-get turn :prompt) (agent-shell-fork-tree--node-prompt node))
                                    (equal (plist-get turn :answer) (agent-shell-fork-tree--node-answer node)))))
               (unless (if same-ids same-text
                         (equal (agent-shell-fork-tree--node-fingerprint node)
                                (agent-shell-fork-tree--fingerprint (plist-get turn :prompt) (plist-get turn :answer))))
                 (signal 'agent-shell-fork-tree-incremental-error
                         (list (format "%s: cached turn %d changed" id index))))))
    (cl-loop for record in path for turn in turns do
             (setf (map-elt record 'ids) (plist-get turn :ids))
             (when (plist-get turn :ids)
               (let ((node (agent-shell-fork-tree--node store (map-elt record 'node))))
                 (puthash (cons (agent-shell-fork-tree--node-parent node) (plist-get turn :ids))
                          node (agent-shell-fork-tree--store-by-id store)))))
    (let ((parent (or (map-elt (car (last path)) 'node) 0))
          (tail (nthcdr (length path) turns)))
      (when (and (null path) tail)
        (setq parent (agent-shell-fork-tree--attach store 0 (car tail))
              path (list (list (cons 'node parent) (cons 'ids (plist-get (pop tail) :ids))))))
      (if (and (not (equal id focus))
               (not (equal (map-elt (car path) 'node) focus-first)))
          (setf (agent-shell-fork-tree--session-coverage session) 'prefix)
        (let ((end (last path)))
          (dolist (turn tail)
            (setq parent (agent-shell-fork-tree--attach store parent turn))
            (let ((cell (list (list (cons 'node parent) (cons 'ids (plist-get turn :ids))))))
              (if end (setcdr end cell) (setq path cell))
              (setq end cell))))
        (setf (agent-shell-fork-tree--session-coverage session) (if complete 'full 'partial)))
      (setf (agent-shell-fork-tree--session-path session) path
            (agent-shell-fork-tree--session-title session) (or (map-elt info 'title) id)
            (agent-shell-fork-tree--session-dirty session) (not complete))
      (when complete (setf (agent-shell-fork-tree--session-updated session) (map-elt info 'updatedAt)))
      (puthash id session (agent-shell-fork-tree--store-sessions store)))
    session))

(defun agent-shell-fork-tree--needs-read (store info focus force)
  "Whether INFO needs reading from the backend for STORE and FOCUS."
  (let* ((old (gethash (map-elt info 'sessionId) (agent-shell-fork-tree--store-sessions store)))
         (current (gethash focus (agent-shell-fork-tree--store-sessions store))))
    (or force (null old) (agent-shell-fork-tree--session-dirty old)
        (eq (agent-shell-fork-tree--session-coverage old) 'partial)
        (not (map-elt info 'updatedAt))
        (not (equal (map-elt info 'updatedAt) (agent-shell-fork-tree--session-updated old)))
        (and (eq (agent-shell-fork-tree--session-coverage old) 'prefix)
             (or (equal focus (map-elt info 'sessionId))
                 (and current (agent-shell-fork-tree--first current)
                      (equal (agent-shell-fork-tree--first current) (agent-shell-fork-tree--first old))))))))

(defun agent-shell-fork-tree--save (store)
  "Atomically persist STORE as private JSON, without executable configuration."
  (when agent-shell-fork-tree-cache-directory
    (let* ((dir (expand-file-name agent-shell-fork-tree-cache-directory))
           (coding-system-for-write 'utf-8-unix) temporary)
      (make-directory dir t)
      (setq temporary (make-temp-file (expand-file-name ".cache-" dir)))
      (set-file-modes temporary #o600)
      (unwind-protect
          (progn
            (with-temp-file temporary
              (insert (json-serialize
                       `((version . 1) (agent . ,(agent-shell-fork-tree--store-agent store))
                         (cwd . ,(agent-shell-fork-tree--store-cwd store))
                         (nodes . ,(vconcat
                                    (mapcar (lambda (node)
                                              `((id . ,(agent-shell-fork-tree--node-id node))
                                                (parent . ,(agent-shell-fork-tree--node-parent node))
                                                (prompt . ,(agent-shell-fork-tree--node-prompt node))
                                                (answer . ,(agent-shell-fork-tree--node-answer node))
                                                (fingerprint . ,(agent-shell-fork-tree--node-fingerprint node))
                                                (label . ,(agent-shell-fork-tree--node-label node))
                                                (tools . ,(vconcat
                                                           (mapcar (lambda (tool)
                                                                     `((title . ,(map-elt (cdr tool) 'title))
                                                                       (status . ,(map-elt (cdr tool) 'status))
                                                                       (diffs . ,(vconcat
                                                                                  (mapcar (lambda (d) `((file . ,(map-elt d :file)) (old . ,(map-elt d :old)) (new . ,(map-elt d :new))))
                                                                                          (map-elt (cdr tool) 'diffs))))))
                                                                   (agent-shell-fork-tree--node-tools node))))))
                                            (hash-table-values (agent-shell-fork-tree--store-nodes store)))))
                         (sessions . ,(vconcat
                                       (mapcar (lambda (session)
                                                 `((id . ,(agent-shell-fork-tree--session-id session))
                                                   (title . ,(agent-shell-fork-tree--session-title session))
                                                   (updated . ,(agent-shell-fork-tree--session-updated session))
                                                   (coverage . ,(symbol-name (agent-shell-fork-tree--session-coverage session)))
                                                   (dirty . ,(agent-shell-fork-tree--session-dirty session))
                                                   (path . ,(vconcat (mapcar (lambda (record) `((node . ,(map-elt record 'node)) (ids . ,(vconcat (map-elt record 'ids)))))
                                                                            (agent-shell-fork-tree--session-path session))))))
                                               (hash-table-values (agent-shell-fork-tree--store-sessions store)))))))))
            (rename-file temporary (expand-file-name (concat (agent-shell-fork-tree--store-key store) ".json") dir) t))
        (when (file-exists-p temporary) (delete-file temporary))))))

(defun agent-shell-fork-tree--load (agent cwd)
  "Load AGENT/CWD's cache or make a new store."
  (let* ((store (agent-shell-fork-tree--new-store agent cwd))
         (file (and agent-shell-fork-tree-cache-directory
                    (expand-file-name (concat (agent-shell-fork-tree--store-key store) ".json") agent-shell-fork-tree-cache-directory))))
    (when (and file (file-exists-p file))
      (let ((data (with-temp-buffer (insert-file-contents file)
                                   (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object nil))))
        (unless (and (equal 1 (map-elt data 'version)) (equal agent (map-elt data 'agent)) (equal cwd (map-elt data 'cwd)))
          (error "Fork-tree cache identity/version mismatch: %s" file))
        (dolist (row (map-elt data 'nodes))
          (let ((node (agent-shell-fork-tree--node-create
                       :id (map-elt row 'id) :parent (map-elt row 'parent)
                       :prompt (map-elt row 'prompt) :answer (map-elt row 'answer)
                       :fingerprint (map-elt row 'fingerprint) :label (map-elt row 'label)
                       :tools (mapcar (lambda (tool) (cons nil `((title . ,(map-elt tool 'title)) (status . ,(map-elt tool 'status))
                                                               (diffs . ,(mapcar (lambda (d) `((:file . ,(map-elt d 'file)) (:old . ,(map-elt d 'old)) (:new . ,(map-elt d 'new)))) (map-elt tool 'diffs)))))) (map-elt row 'tools)))))
            (puthash (agent-shell-fork-tree--node-id node) node (agent-shell-fork-tree--store-nodes store))
            (setf (agent-shell-fork-tree--store-next store) (max (agent-shell-fork-tree--store-next store) (1+ (agent-shell-fork-tree--node-id node))))))
        (dolist (node (sort (hash-table-values (agent-shell-fork-tree--store-nodes store)) (lambda (a b) (< (agent-shell-fork-tree--node-id a) (agent-shell-fork-tree--node-id b)))))
          (when-let* ((parent (agent-shell-fork-tree--node-parent node)))
            (let ((ancestor (agent-shell-fork-tree--node store parent)))
              (setf (agent-shell-fork-tree--node-children ancestor) (append (agent-shell-fork-tree--node-children ancestor) (list (agent-shell-fork-tree--node-id node)))))
            (puthash (cons parent (agent-shell-fork-tree--node-fingerprint node)) node (agent-shell-fork-tree--store-by-text store))))
        (dolist (row (map-elt data 'sessions))
          (let ((session (agent-shell-fork-tree--session-create
                          :id (map-elt row 'id) :title (map-elt row 'title) :updated (map-elt row 'updated)
                          :coverage (intern (map-elt row 'coverage)) :dirty (map-elt row 'dirty) :path (map-elt row 'path))))
            (puthash (agent-shell-fork-tree--session-id session) session (agent-shell-fork-tree--store-sessions store))
            (dolist (record (agent-shell-fork-tree--session-path session))
              (when (map-elt record 'ids)
                (let ((node (agent-shell-fork-tree--node store (map-elt record 'node))))
                  (puthash (cons (agent-shell-fork-tree--node-parent node) (map-elt record 'ids)) node (agent-shell-fork-tree--store-by-id store)))))))))
    store))

(provide 'agent-shell-fork-tree-store)
;;; agent-shell-fork-tree-store.el ends here
