;;; benchmark.el --- Reproducible local rebuild ablations -*- lexical-binding: t; -*-
;;; Commentary:
;; Run with the package and its dependencies on load-path:
;; emacs -Q --batch ... -L . -l tests/benchmark.el -f aft-bench-run
;; AFT_BENCH_REPEATS (default 3), AFT_BENCH_SESSIONS (32 per group),
;; AFT_BENCH_TURNS (80) and AFT_BENCH_OUTPUT optionally customize the run.
;; No real agent, user history or network is accessed.
;;; Code:
(require 'agent-shell-fork-tree)
(require 'benchmark)
(require 'macroexp)

(defconst aft-bench-root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defconst aft-bench-baseline "a4f37acc4a72735a33cec6eadc01eb46f09d2a96")
(defvar aft-bench-originals nil)

(defun aft-bench-old-function (file name)
  "Read NAME's original definition from baseline FILE, without loading the file."
  (with-temp-buffer
    (let ((default-directory aft-bench-root))
      (unless (zerop (call-process "git" nil t nil "show" (concat aft-bench-baseline ":" file)))
        (error "Cannot read benchmark baseline %s" aft-bench-baseline)))
    (goto-char (point-min))
    (let (form found)
      (while (and (not found) (< (point) (point-max)))
        (setq form (read (current-buffer)))
        (when (and (eq (car-safe form) 'defun) (eq (cadr form) name))
          (setq found
                (eval (macroexpand-all
                       `(lambda ,(nth 2 form) ,@(nthcdr 3 form)))
                      t))))
      (or found (error "Missing baseline function %s" name)))))

(defun aft-bench-fixture (count turns)
  "Build COUNT related and COUNT unrelated histories with TURNS turns each."
  (let ((histories (make-hash-table :test #'equal)) rows)
    (dotimes (s (* count 2))
      (let ((id (if (zerop s) "main" (format "s%03d" s)))
            updates)
        (dotimes (i turns)
          (let* ((shared (and (< s count) (< i (- turns 10))))
                 (prompt (format "%s turn %03d" (if shared "Shared" (format "Session %03d" s)) i))
                 (answer (concat "Reply: " prompt "\n\n" (make-string 128 ?x))))
            (push `((sessionUpdate . "user_message_chunk")
                    (messageId . ,(format "s%d-u%d" s i)) (content (type . "text") (text . ,prompt))) updates)
            (push `((sessionUpdate . "agent_message_chunk")
                    (messageId . ,(format "s%d-a%d" s i)) (content (type . "text") (text . ,answer))) updates)))
        (puthash id (nreverse updates) histories)
        (push `((sessionId . ,id) (title . ,id) (updatedAt . "1")) rows)))
    (list (nreverse rows) histories)))

(defun aft-bench-once (variant fixture)
  "Measure VARIANT using FIXTURE and return timing, operation counts and hashes."
  (let* ((directory (make-temp-file "aft-bench-" t))
         (agent-shell-fork-tree-cache-directory directory)
         (agent-shell-fork-tree--stores (make-hash-table :test #'equal))
         (agent-shell-fork-tree-auto-rebuild nil)
         (agent-shell-fork-tree-checkpoint-batch-size
          (if (memq variant '(baseline no-checkpoint-batching)) 1 16))
         (store (agent-shell-fork-tree--new-store "benchmark" "/tmp"))
         (source (generate-new-buffer " *aft-bench-source*"))
         (view (generate-new-buffer " *aft-bench-view*"))
         (rows (car fixture)) (histories (cadr fixture))
         (renders 0) (previews 0) (saves 0) (loads 0) (erases 0)
         (bytes 0) (changes 0) (render-seconds 0.0) (save-seconds 0.0)
         callback pending transport-client worker measurements tree-hash cache-hash
         (original-render (alist-get 'agent-shell-fork-tree--render aft-bench-originals))
         (original-preview (alist-get 'agent-shell-fork-tree--preview aft-bench-originals))
         (original-scan (alist-get 'agent-shell-fork-tree--scan aft-bench-originals))
         (original-rebuild (alist-get 'agent-shell-fork-tree-rebuild aft-bench-originals))
         (render (symbol-function 'agent-shell-fork-tree--render))
         (preview (symbol-function 'agent-shell-fork-tree--preview))
         (progress (symbol-function 'agent-shell-fork-tree--progress))
         (save (symbol-function 'agent-shell-fork-tree--save))
         (erase (symbol-function 'erase-buffer))
         (scan (symbol-function 'agent-shell-fork-tree--scan))
         (rebuild (symbol-function 'agent-shell-fork-tree-rebuild)))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer source
            (setq-local agent-shell--state
                        (agent-shell--make-state
                         :buffer source
                         :agent-config
                         `((:identifier . benchmark)
                           (:client-maker . ,(lambda (b)
                                               (acp-make-client :command "fixture" :context-buffer b)))))))
          (switch-to-buffer view)
          (agent-shell-fork-tree-view-mode)
          (setq agent-shell-fork-tree--store store agent-shell-fork-tree--focus "main"
                agent-shell-fork-tree--source source)
          (agent-shell-fork-tree--render)
          (add-hook 'after-change-functions (lambda (&rest _) (cl-incf changes)) nil t)
          (cl-letf
              (((symbol-function 'shell-maker-busy) (lambda () nil))
               ((symbol-function 'acp-subscribe-to-notifications)
                (lambda (&rest args) (setq callback (plist-get args :on-notification))))
               ((symbol-function 'acp-send-request)
                (lambda (&rest args)
                  (setq transport-client (plist-get args :client) worker (plist-get args :buffer))
                  (push args pending)))
               ((symbol-function 'agent-shell-fork-tree--scan)
                (if (eq variant 'baseline) original-scan scan))
               ((symbol-function 'agent-shell-fork-tree-rebuild)
                (if (eq variant 'baseline) original-rebuild rebuild))
               ((symbol-function 'agent-shell-fork-tree--render)
                (lambda ()
                  (cl-incf renders)
                  (let ((start (float-time)))
                    (funcall (if (memq variant '(baseline no-indexed-render)) original-render render))
                    (cl-incf render-seconds (- (float-time) start)))
                  (when (eq variant 'no-indexed-render)
                    (setq agent-shell-fork-tree--rendered-state (agent-shell-fork-tree--view-state)
                          agent-shell-fork-tree--rendered-revision (agent-shell-fork-tree--store-revision store)))))
               ((symbol-function 'agent-shell-fork-tree--preview)
                (lambda ()
                  (let* ((before agent-shell-fork-tree--preview)
                         (tick (and (buffer-live-p before) (buffer-modified-tick before))))
                    (funcall (if (memq variant '(baseline no-semantic-refresh)) original-preview preview))
                    (unless (and (eq before agent-shell-fork-tree--preview)
                                 (equal tick (buffer-modified-tick agent-shell-fork-tree--preview)))
                      (cl-incf previews)))))
               ((symbol-function 'agent-shell-fork-tree--progress)
                (if (eq variant 'no-semantic-refresh)
                    (lambda (_store status)
                      (setq agent-shell-fork-tree--status status)
                      (agent-shell-fork-tree--render))
                  progress))
               ((symbol-function 'agent-shell-fork-tree--save)
                (lambda (value)
                  (cl-incf saves)
                  (let ((start (float-time)))
                    (funcall save value)
                    (cl-incf save-seconds (- (float-time) start)))
                  (cl-incf bytes (file-attribute-size
                                  (file-attributes
                                   (expand-file-name (concat (agent-shell-fork-tree--store-key value) ".json") directory))))))
               ((symbol-function 'erase-buffer)
                (lambda ()
                  (when (eq (current-buffer) view) (cl-incf erases))
                  (funcall erase))))
            (garbage-collect)
            (setq measurements
                  (benchmark-run 1
                    (agent-shell-fork-tree-rebuild)
                    ;; Drain an explicit callback queue so every variant receives
                    ;; identical notifications, without recursive synchronous RPCs.
                    (while pending
                      (let* ((args (pop pending))
                             (request (plist-get args :request))
                             (method (map-elt request :method))
                             (sid (map-nested-elt request '(:params sessionId)))
                             (cursor (or (map-nested-elt request '(:params cursor)) 0))
                             response)
                        (with-current-buffer (plist-get args :buffer)
                          (pcase method
                            ("initialize"
                             (setq response '((agentCapabilities (loadSession . t) (sessionCapabilities (list))))))
                            ("session/list"
                             (setq response `((sessions . ,(vconcat (seq-subseq rows cursor (min (length rows) (+ cursor 16)))))
                                              ,@(when (< (+ cursor 16) (length rows)) `((nextCursor . ,(+ cursor 16)))))))
                            ("session/load"
                             (cl-incf loads)
                             (dolist (update (gethash sid histories))
                               (funcall callback `((method . "session/update") (params (sessionId . ,sid) (update . ,update))))))
                            (_ (error "Unexpected benchmark request: %s" method)))
                          (funcall (plist-get args :on-success) response))))
                    (when agent-shell-fork-tree--loading (error "Benchmark did not finish")))))
          (unless (equal agent-shell-fork-tree--status "Up to date")
            (error "Benchmark rebuild failed: %s" agent-shell-fork-tree--status))
          (setq tree-hash (secure-hash 'sha256 (current-buffer))
                cache-hash (with-temp-buffer
                             (insert-file-contents (expand-file-name (concat (agent-shell-fork-tree--store-key store) ".json") directory))
                             (secure-hash 'sha256 (current-buffer))))
          `((variant . ,(symbol-name variant)) (seconds . ,(car measurements))
            (gc-count . ,(nth 1 measurements)) (gc-seconds . ,(nth 2 measurements))
            (render-seconds . ,render-seconds) (save-seconds . ,save-seconds)
            (renders . ,renders) (preview-writes . ,previews) (tree-erases . ,erases)
            (tree-changes . ,changes) (cache-writes . ,saves) (cache-bytes . ,bytes)
            (history-loads . ,loads) (tree-hash . ,tree-hash) (cache-hash . ,cache-hash)))
      (when transport-client (acp-shutdown :client transport-client))
      (when (buffer-live-p worker) (kill-buffer worker))
      (when (buffer-live-p view)
        (let ((preview-buffer (buffer-local-value 'agent-shell-fork-tree--preview view)))
          (when (buffer-live-p preview-buffer) (kill-buffer preview-buffer)))
        (kill-buffer view))
      (kill-buffer source)
      (delete-directory directory t))))

(defun aft-bench-run ()
  "Run baseline, all optimizations and each leave-one-out ablation."
  (setq aft-bench-originals
        (mapcar (lambda (name)
                  (cons name (aft-bench-old-function
                              (if (eq name 'agent-shell-fork-tree--scan) "agent-shell-fork-tree-acp.el"
                                "agent-shell-fork-tree.el") name)))
                '(agent-shell-fork-tree--render agent-shell-fork-tree--preview
                                                agent-shell-fork-tree--scan agent-shell-fork-tree-rebuild)))
  (let* ((repeats (string-to-number (or (getenv "AFT_BENCH_REPEATS") "3")))
         (sessions (string-to-number (or (getenv "AFT_BENCH_SESSIONS") "32")))
         (turns (string-to-number (or (getenv "AFT_BENCH_TURNS") "80")))
         (fixture (aft-bench-fixture sessions turns))
         (variants '(baseline all no-indexed-render no-semantic-refresh no-checkpoint-batching))
         tree-hash cache-hash results)
    (dotimes (round repeats)
      ;; Rotate execution order to avoid always giving one variant a warm cache.
      (dolist (variant (append (nthcdr (mod round (length variants)) variants)
                               (seq-take variants (mod round (length variants)))))
        (let ((result (aft-bench-once variant fixture)))
          (unless tree-hash (setq tree-hash (map-elt result 'tree-hash) cache-hash (map-elt result 'cache-hash)))
          (unless (and (equal tree-hash (map-elt result 'tree-hash))
                       (equal cache-hash (map-elt result 'cache-hash)))
            (error "Ablation changed final tree/cache: %s" variant))
          (push result results)
          (princ (format "%s round=%d seconds=%.4f renders=%d preview-writes=%d erases=%d saves=%d bytes=%d\n"
                         variant (1+ round) (map-elt result 'seconds) (map-elt result 'renders)
                         (map-elt result 'preview-writes) (map-elt result 'tree-erases)
                         (map-elt result 'cache-writes) (map-elt result 'cache-bytes))))))
    (when-let* ((output (getenv "AFT_BENCH_OUTPUT")))
      (with-temp-file output
        (insert (json-serialize
                 `((emacs . ,emacs-version) (system . ,system-configuration) (baseline . ,aft-bench-baseline)
                   (sessions-per-group . ,sessions) (turns . ,turns) (repeats . ,repeats)
                   (results . ,(vconcat (nreverse results))))))))))
