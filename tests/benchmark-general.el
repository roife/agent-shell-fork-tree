;;; benchmark-general.el --- General-case performance ablations -*- lexical-binding: t; -*-
;;; Commentary:
;; Compare each retained optimization with an isolated ablation.  The suite
;; also records rejected candidates and an end-to-end historical comparison.
;; Workloads cover wide trees, deep replay and append, sparse/dense conversation
;; groups, paginated cached discovery, view-state checks and navigation.
;;
;; Run with the package and dependencies on load-path:
;;   AFT_PERF_REPEATS=7 AFT_PERF_OUTPUT=/tmp/results.json \
;;     emacs -Q --batch ... -L . -l tests/benchmark-general.el -f aft-perf-run
;;; Code:
(require 'agent-shell-fork-tree)
(require 'benchmark)
(require 'macroexp)

(defconst aft-perf-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(defconst aft-perf-baseline "0271ad9cd590319c64227a1ce59608bee9d406e9")
(defvar aft-perf-old-functions nil)

(defun aft-perf-old-function (file name)
  "Read NAME's definition from baseline FILE without loading that revision."
  (with-temp-buffer
    (let ((default-directory aft-perf-root))
      (unless (zerop (call-process "git" nil t nil "show"
                                   (concat aft-perf-baseline ":" file)))
        (error "Cannot read benchmark baseline %s" aft-perf-baseline)))
    (goto-char (point-min))
    (let (form found)
      (while (and (not found) (< (point) (point-max)))
        (setq form (read (current-buffer)))
        (when (and (eq (car-safe form) 'defun) (eq (cadr form) name))
          ;; Loading a file macroexpands its definitions before installing them.
          ;; Match that behavior so the ablation measures only the code change.
          (setq found
                (eval (macroexpand-all
                       `(lambda ,(nth 2 form) ,@(nthcdr 3 form)))
                      t))))
      (or found (error "Missing baseline function %s" name)))))

(defun aft-perf-old (name)
  "Return baseline definition of NAME."
  (or (alist-get name aft-perf-old-functions)
      (error "No baseline definition for %s" name)))

(defun aft-perf-turn (session index)
  "Build one normalized turn for SESSION at INDEX."
  (let ((text (format "%s turn %d" session index)))
    (list :prompt text :answer (concat "Reply: " text (make-string 64 ?x))
          :ids (list (format "%s-u%d" session index)
                     (format "%s-a%d" session index))
          :tools nil)))

(defun aft-perf-store-digest (store)
  "Return a stable digest of STORE's externally meaningful graph."
  (secure-hash
   'sha256
   (prin1-to-string
    (list
     (mapcar (lambda (node)
               (list (agent-shell-fork-tree--node-id node)
                     (agent-shell-fork-tree--node-parent node)
                     (agent-shell-fork-tree--node-children node)
                     (agent-shell-fork-tree--node-prompt node)
                     (agent-shell-fork-tree--node-answer node)
                     (agent-shell-fork-tree--node-fingerprint node)))
             (sort (hash-table-values (agent-shell-fork-tree--store-nodes store))
                   (lambda (a b) (< (agent-shell-fork-tree--node-id a)
                                    (agent-shell-fork-tree--node-id b)))))
     (mapcar (lambda (session)
               (list (agent-shell-fork-tree--session-id session)
                     (agent-shell-fork-tree--session-title session)
                     (agent-shell-fork-tree--session-updated session)
                     (agent-shell-fork-tree--session-coverage session)
                     (agent-shell-fork-tree--session-dirty session)
                     (agent-shell-fork-tree--session-path session)))
             (sort (hash-table-values (agent-shell-fork-tree--store-sessions store))
                   (lambda (a b) (string< (agent-shell-fork-tree--session-id a)
                                          (agent-shell-fork-tree--session-id b)))))))))

(defun aft-perf-result (scenario variant measurement digest &optional operations)
  "Format a benchmark result for SCENARIO and VARIANT."
  `((scenario . ,scenario) (variant . ,variant)
    (seconds . ,(car measurement)) (gc-count . ,(nth 1 measurement))
    (gc-seconds . ,(nth 2 measurement)) (operations . ,(or operations 1))
    (digest . ,digest)))

(defun aft-perf-wide-children (variant count)
  "Measure adding COUNT root children for VARIANT."
  (let ((store (agent-shell-fork-tree--new-store "benchmark" "/tmp")) measurement)
    (garbage-collect)
    (cl-letf (((symbol-function 'agent-shell-fork-tree--attach)
               (if (equal variant "no-child-tail")
                   (aft-perf-old 'agent-shell-fork-tree--attach)
                 (symbol-function 'agent-shell-fork-tree--attach))))
      (setq measurement
            (benchmark-run 1
              (dotimes (i count)
                (let ((id (format "wide-%d" i)))
                  (agent-shell-fork-tree--ingest
                   store `((sessionId . ,id) (title . ,id) (updatedAt . "1"))
                   (list (aft-perf-turn id 0)) t "main"))))))
    (aft-perf-result "wide-children" variant measurement
                     (aft-perf-store-digest store) count)))

(defun aft-perf-copy-path-before-ingest (current store info turns complete focus)
  "Call CURRENT after copying INFO's cached path in STORE.
This is the path-spine reuse ablation: all validation, indexing and commit logic
continues to use the current implementation."
  (when-let* ((session (gethash (map-elt info 'sessionId)
                                (agent-shell-fork-tree--store-sessions store))))
    (let ((path (copy-tree (agent-shell-fork-tree--session-path session))))
      (setf (agent-shell-fork-tree--session-path session) path
            (agent-shell-fork-tree--session-path-tail session) (last path))))
  (funcall current store info turns complete focus))

(defun aft-perf-deep-replay (variant turns repeats)
  "Measure REPEATS unchanged full replays of a TURNS-long session."
  (let* ((store (agent-shell-fork-tree--new-store "benchmark" "/tmp"))
         (history (cl-loop for i below turns collect (aft-perf-turn "main" i)))
         (current (symbol-function 'agent-shell-fork-tree--ingest)) measurement)
    (agent-shell-fork-tree--ingest
     store '((sessionId . "main") (title . "main") (updatedAt . "1"))
     history t "main")
    (garbage-collect)
    (cl-letf (((symbol-function 'agent-shell-fork-tree--ingest)
               (if (equal variant "no-path-reuse")
                   (lambda (&rest args)
                     (apply #'aft-perf-copy-path-before-ingest current args))
                 current)))
      (setq measurement
            (benchmark-run 1
              (dotimes (_ repeats)
                (agent-shell-fork-tree--ingest
                 store '((sessionId . "main") (title . "main") (updatedAt . "2"))
                 history t "main")))))
    (aft-perf-result "deep-replay" variant measurement
                     (aft-perf-store-digest store) repeats)))

(defun aft-perf-deep-append (variant turns suffix)
  "Measure appending SUFFIX turns to a cached TURNS-long session."
  (let* ((store (agent-shell-fork-tree--new-store "benchmark" "/tmp"))
         (history (cl-loop for i below turns collect (aft-perf-turn "main" i)))
         (extended (append history
                           (cl-loop for i from turns below (+ turns suffix)
                                    collect (aft-perf-turn "main" i))))
         (current (symbol-function 'agent-shell-fork-tree--ingest)) measurement)
    (agent-shell-fork-tree--ingest
     store '((sessionId . "main") (title . "main") (updatedAt . "1"))
     history t "main")
    (garbage-collect)
    (cl-letf (((symbol-function 'agent-shell-fork-tree--ingest)
               (if (equal variant "no-path-reuse")
                   (lambda (&rest args)
                     (apply #'aft-perf-copy-path-before-ingest current args))
                 current)))
      (setq measurement
            (benchmark-run 1
              (agent-shell-fork-tree--ingest
               store '((sessionId . "main") (title . "main") (updatedAt . "2"))
               extended t "main"))))
    (aft-perf-result "deep-append" variant measurement
                     (aft-perf-store-digest store) suffix)))

(defun aft-perf-related-store (count related-count)
  "Build COUNT cached sessions, RELATED-COUNT of which share the first turn."
  (let ((store (agent-shell-fork-tree--new-store "benchmark" "/tmp")))
    (dotimes (i count)
      (let* ((id (if (zerop i) "main" (format "s%d" i)))
             (first (if (< i related-count) 1 (1+ i)))
             (session (agent-shell-fork-tree--session-create
                       :id id :title id :updated "1" :coverage 'full
                       :path (list `((node . ,first))))))
        (setf (agent-shell-fork-tree--session-path-tail session)
              (last (agent-shell-fork-tree--session-path session)))
        (puthash id session (agent-shell-fork-tree--store-sessions store))))
    store))

(defun aft-perf-first-index-put (index session)
  "Insert SESSION into candidate first-turn INDEX."
  (when-let* ((first (agent-shell-fork-tree--first session)))
    (let ((bucket (or (gethash first index)
                      (let ((table (make-hash-table :test #'equal)))
                        (puthash first table index)
                        table))))
      (puthash (agent-shell-fork-tree--session-id session) session bucket))))

(defun aft-perf-first-index-related (store index focus)
  "Return FOCUS's related sessions using candidate first-turn INDEX."
  (let* ((current (gethash focus (agent-shell-fork-tree--store-sessions store)))
         (first (and current (agent-shell-fork-tree--first current)))
         (bucket (and first (gethash first index))))
    (if bucket (hash-table-values bucket)
      (and current (list current)))))

(defun aft-perf-related (scenario variant count related-count repeats)
  "Measure repeated related-session queries for VARIANT."
  (let ((store (aft-perf-related-store count related-count))
        (index (make-hash-table :test #'eql)) measurement ids)
    (when (equal variant "first-index-candidate")
      (dolist (session
               (hash-table-values
                (agent-shell-fork-tree--store-sessions store)))
        (aft-perf-first-index-put index session)))
    (garbage-collect)
    (setq measurement
          (benchmark-run 1
            (dotimes (_ repeats)
              (setq ids
                    (mapcar
                     #'agent-shell-fork-tree--session-id
                     (if (equal variant "first-index-candidate")
                         (aft-perf-first-index-related store index "main")
                       (agent-shell-fork-tree--related store "main")))))))
    (aft-perf-result scenario variant measurement
                     (secure-hash 'sha256
                                  (prin1-to-string (sort ids #'string<)))
                     repeats)))

(defun aft-perf-related-lifecycle
    (scenario variant count queries iterations)
  "Measure ITERATIONS index lifecycles for SCENARIO and VARIANT."
  (let (store index ids measurement)
    (garbage-collect)
    (setq measurement
          (benchmark-run 1
            (dotimes (_ iterations)
              (setq store (agent-shell-fork-tree--new-store "benchmark" "/tmp")
                    index (make-hash-table :test #'eql))
              (dotimes (i count)
                (let* ((id (if (zerop i) "main" (format "s%d" i)))
                       (session (agent-shell-fork-tree--session-create
                                 :id id :title id :updated "1" :coverage 'full
                                 :path (list '((node . 1))))))
                  (setf (agent-shell-fork-tree--session-path-tail session)
                        (last (agent-shell-fork-tree--session-path session)))
                  (puthash id session
                           (agent-shell-fork-tree--store-sessions store))
                  (when (equal variant "first-index-candidate")
                    (aft-perf-first-index-put index session))))
              (dotimes (_ queries)
                (setq ids
                      (mapcar
                       #'agent-shell-fork-tree--session-id
                       (if (equal variant "first-index-candidate")
                           (aft-perf-first-index-related store index "main")
                         (agent-shell-fork-tree--related store "main")))))
              (dotimes (i count)
                (let ((id (if (zerop i) "main" (format "s%d" i))))
                  (when (equal variant "first-index-candidate")
                    (when-let* ((session
                                 (gethash id
                                          (agent-shell-fork-tree--store-sessions store)))
                                (first (agent-shell-fork-tree--first session))
                                (bucket (gethash first index)))
                      (remhash id bucket)))
                  (remhash id
                           (agent-shell-fork-tree--store-sessions store)))))))
    (aft-perf-result scenario variant measurement
                     (secure-hash 'sha256
                                  (prin1-to-string
                                   (list (sort ids #'string<)
                                         (hash-table-count
                                          (agent-shell-fork-tree--store-sessions store)))))
                     (* iterations (+ (* count 2) queries)))))

(defun aft-perf-tool-updates (tools updates-per-tool)
  "Build one turn containing TOOLS, each with UPDATES-PER-TOOL updates."
  (append
   '(((sessionUpdate . "user_message_chunk")
      (messageId . "user") (content (text . "prompt"))))
   (cl-loop for i below tools
            collect `((sessionUpdate . "tool_call")
                      (toolCallId . ,(format "tool-%d" i))
                      (title . ,(format "Tool %d" i))))
   (cl-loop for round below updates-per-tool append
            (cl-loop for i below tools
                     collect `((sessionUpdate . "tool_call_update")
                               (toolCallId . ,(format "tool-%d" i))
                               (status . ,(format "round-%d" round)))))
   '(((sessionUpdate . "agent_message_chunk")
      (messageId . "agent") (content (text . "answer"))))))

(defun aft-perf-turns-with-tool-index (updates complete)
  "Candidate UPDATES normalizer using a tool hash after sixteen entries."
  (let (turns prompt answer ids tools tool-index (tool-count 0)
              last-user last-kind have-user)
    (cl-labels ((seal ()
                  (when have-user
                    (push (list :prompt (apply #'concat (nreverse prompt))
                                :answer (apply #'concat (nreverse answer))
                                :ids (and ids (not (memq nil ids)) (nreverse ids))
                                :tools (nreverse tools)) turns))
                  (setq prompt nil answer nil ids nil tools nil tool-index nil
                        tool-count 0 have-user nil)))
      (dolist (update updates)
        (let* ((kind (map-elt update 'sessionUpdate))
               (id (map-elt update 'messageId))
               (content (map-elt update 'content)))
          (pcase kind
            ("user_message_chunk"
             (when (and have-user
                        (or (not (equal last-kind kind))
                            (not (equal last-user id))))
               (seal))
             (unless have-user (push id ids))
             (setq have-user t last-user id last-kind kind)
             (push (or (map-elt content 'text) "") prompt))
            ("agent_message_chunk"
             (when have-user
               (unless (and (equal kind last-kind) (equal id (car ids)))
                 (push id ids))
               (push (or (map-elt content 'text) "") answer)
               (setq last-kind kind)))
            ((or "tool_call" "tool_call_update")
             (when have-user
               (let* ((tool-id (map-elt update 'toolCallId))
                      (entry (if tool-index (gethash tool-id tool-index)
                               (assoc tool-id tools))))
                 (unless entry
                   (setq entry
                         (cons tool-id
                               (list (cons 'title nil) (cons 'status nil)
                                     (cons 'diffs nil))))
                   (push entry tools)
                   (cl-incf tool-count)
                   (cond (tool-index (puthash tool-id entry tool-index))
                         ((>= tool-count 16)
                          (setq tool-index
                                (make-hash-table :test #'equal
                                                 :size (* 2 tool-count)))
                          (dolist (tool tools)
                            (puthash (car tool) tool tool-index)))))
                 (dolist (field '(title status))
                   (when (assq field update)
                     (setf (alist-get field (cdr entry))
                           (map-elt update field))))
                 (when-let* ((diffs
                              (agent-shell--make-diff-infos
                               :acp-tool-call update)))
                   (setf (alist-get 'diffs (cdr entry)) diffs)))
               (setq last-kind kind))))))
      (when (and complete answer) (seal)))
    (nreverse turns)))

(defun aft-perf-tools (scenario variant tools updates-per-tool repeats)
  "Measure the rejected tool-index candidate for SCENARIO and VARIANT."
  (let ((updates (aft-perf-tool-updates tools updates-per-tool)) turns measurement)
    (garbage-collect)
    (cl-letf (((symbol-function 'agent-shell-fork-tree--turns)
               (if (equal variant "tool-hash-candidate")
                   #'aft-perf-turns-with-tool-index
                 (symbol-function 'agent-shell-fork-tree--turns))))
      (setq measurement
            (benchmark-run 1
              (dotimes (_ repeats)
                (setq turns (agent-shell-fork-tree--turns updates t))))))
    (aft-perf-result scenario variant measurement
                     (secure-hash 'sha256 (prin1-to-string turns)) repeats)))

(defun aft-perf-pages (scenario variant count page-size repeats)
  "Measure one paginated-list optimization for SCENARIO and VARIANT."
  (let ((pages (cl-loop for start from 0 below count by page-size
                        collect (cl-loop for i from start below (min count (+ start page-size))
                                         collect i)))
        listed tail listed-count measurement)
    (garbage-collect)
    (setq measurement
          (benchmark-run 1
            (dotimes (_ repeats)
              (setq listed nil tail nil listed-count 0)
              (dolist (page pages)
                (pcase variant
                  ("no-page-tail"
                   (setq listed (append listed (copy-sequence page))
                         listed-count (+ listed-count (length page))))
                  ("no-page-count"
                   (pcase-let ((`(,head ,new-tail ,_new-count)
                                (agent-shell-fork-tree--append-page
                                 listed tail listed-count (copy-sequence page))))
                     (setq listed head tail new-tail
                           listed-count (length listed))))
                  (_
                   (pcase-let ((`(,head ,new-tail ,new-count)
                                (agent-shell-fork-tree--append-page
                                 listed tail listed-count (copy-sequence page))))
                     (setq listed head tail new-tail
                           listed-count new-count))))))))
    (aft-perf-result scenario variant measurement
                     (secure-hash 'sha256
                                  (prin1-to-string
                                   (list listed listed-count)))
                     (* repeats count))))

(defun aft-perf-progress-total (variant count repeats)
  "Measure repeated progress totals for COUNT discovered sessions."
  (let ((listed (number-sequence 1 count))
        (total count) text measurement)
    (garbage-collect)
    (setq measurement
          (benchmark-run 1
            (dotimes (_ repeats)
              (dotimes (done count)
                (setq text
                      (format "Read %d/%d"
                              (1+ done)
                              (if (equal variant "no-progress-total")
                                  (length listed)
                                total)))))))
    (aft-perf-result "progress-total" variant measurement
                     (secure-hash 'sha256 text) (* repeats count))))

(defun aft-perf-endpoint (variant turns repeats)
  "Measure repeated endpoint lookup on a TURNS-long session path."
  (let* ((store (agent-shell-fork-tree--new-store "benchmark" "/tmp"))
         (history (cl-loop for i below turns collect (aft-perf-turn "main" i)))
         endpoint measurement)
    (agent-shell-fork-tree--ingest
     store '((sessionId . "main") (title . "main") (updatedAt . "1"))
     history t "main")
    (let ((session (gethash "main" (agent-shell-fork-tree--store-sessions store))))
      (garbage-collect)
      (setq measurement
            (benchmark-run 1
              (dotimes (_ repeats)
                (setq endpoint
                      (if (equal variant "no-path-tail")
                          (map-elt
                           (car (last (agent-shell-fork-tree--session-path session)))
                           'node)
                        (agent-shell-fork-tree--last session)))))))
    (aft-perf-result "endpoint-lookup" variant measurement
                     (secure-hash 'sha256 (prin1-to-string endpoint)) repeats)))

(defun aft-perf-priority (variant count related-count repeats)
  "Measure stable priority partitioning for VARIANT."
  (let* ((rows (cl-loop for i below count
                        collect (list (cons 'sessionId
                                            (if (zerop i) "main" (format "s%d" i))))))
         (related (cl-loop for i from 1 below related-count
                           collect (format "s%d" i)))
         ordered measurement)
    (garbage-collect)
    (setq measurement
          (benchmark-run 1
            (dotimes (_ repeats)
              (setq ordered
                    (if (equal variant "no-priority-hash")
                        (append
                         (seq-filter (lambda (s) (equal "main" (map-elt s 'sessionId))) rows)
                         (seq-filter (lambda (s) (and (not (equal "main" (map-elt s 'sessionId)))
                                                      (member (map-elt s 'sessionId) related))) rows)
                         (seq-remove (lambda (s) (member (map-elt s 'sessionId) related))
                                     (seq-remove (lambda (s) (equal "main" (map-elt s 'sessionId))) rows)))
                      (agent-shell-fork-tree--prioritize rows "main" related))))))
    (aft-perf-result "priority-partition" variant measurement
                     (secure-hash 'sha256 (prin1-to-string ordered)) repeats)))

(defun aft-perf-cached-scan (variant count related-count page-size)
  "Measure a no-history-load paginated scan for VARIANT."
  (let* ((store (aft-perf-related-store count related-count))
         (rows (cl-loop for i below count
                        for id = (if (zerop i) "main" (format "s%d" i))
                        collect `((sessionId . ,id) (title . ,id) (updatedAt . "1"))))
         (source (generate-new-buffer " *aft-perf-source*"))
         pending callback worker client finished error-text measurement)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq-local agent-shell--state
                        (agent-shell--make-state
                         :buffer source
                         :agent-config
                         `((:identifier . benchmark)
                           (:client-maker . ,(lambda (buffer)
                                               (acp-make-client :command "fixture"
                                                                :context-buffer buffer)))))))
          (garbage-collect)
              (cl-letf (((symbol-function 'agent-shell-fork-tree--scan)
                     (if (equal variant "historical-scan")
                         (aft-perf-old 'agent-shell-fork-tree--scan)
                       (symbol-function 'agent-shell-fork-tree--scan)))
                    ((symbol-function 'acp-subscribe-to-notifications)
                     (lambda (&rest args) (setq callback (plist-get args :on-notification))))
                    ((symbol-function 'acp-send-request)
                     (lambda (&rest args)
                       (setq client (plist-get args :client)
                             worker (plist-get args :buffer))
                       (push args pending))))
            (setq measurement
                  (benchmark-run 1
                    (agent-shell-fork-tree--scan
                     source store "main" nil #'ignore
                     (lambda (_store error _cancelled)
                       (setq error-text error finished t)))
                    (while pending
                      (let* ((args (pop pending))
                             (request (plist-get args :request))
                             (method (map-elt request :method))
                             (cursor (or (map-nested-elt request '(:params cursor)) 0))
                             response)
                        (pcase method
                          ("initialize"
                           (setq response
                                 '((agentCapabilities
                                    (loadSession . t)
                                    (sessionCapabilities (list))))))
                          ("session/list"
                           (setq response
                                 `((sessions . ,(vconcat
                                                 (seq-subseq rows cursor
                                                             (min count (+ cursor page-size)))))
                                   ,@(when (< (+ cursor page-size) count)
                                       `((nextCursor . ,(+ cursor page-size)))))))
                          ("session/load" (error "Cached scan unexpectedly loaded history"))
                          (_ (error "Unexpected benchmark request: %s" method)))
                        (with-current-buffer (plist-get args :buffer)
                          (funcall (plist-get args :on-success) response))))
                    (unless finished (error "Cached scan did not finish"))
                    (when error-text (error "Cached scan failed: %s" error-text)))))
          (aft-perf-result "cached-discovery" variant measurement
                           (aft-perf-store-digest store) count))
      (when client (acp-shutdown :client client))
      (when (buffer-live-p worker) (kill-buffer worker))
      (when (buffer-live-p source) (kill-buffer source)))))

(defun aft-perf-visible (variant turns repeats)
  "Measure repeated visible-node queries for VARIANT."
  (let* ((store (agent-shell-fork-tree--new-store "benchmark" "/tmp"))
         (history (cl-loop for i below turns collect (aft-perf-turn "main" i)))
         visible measurement)
    (agent-shell-fork-tree--ingest
     store '((sessionId . "main") (title . "main") (updatedAt . "1"))
     history t "main")
    (with-temp-buffer
      (agent-shell-fork-tree-view-mode)
      (setq agent-shell-fork-tree--store store
            agent-shell-fork-tree--focus "main"
            agent-shell-fork-tree--selected turns)
      (agent-shell-fork-tree--render)
      (garbage-collect)
      (cl-letf (((symbol-function 'agent-shell-fork-tree--visible)
                 (if (equal variant "no-visible-cache")
                     (aft-perf-old 'agent-shell-fork-tree--visible)
                   (symbol-function 'agent-shell-fork-tree--visible))))
        (setq measurement
              (benchmark-run 1
                (dotimes (_ repeats)
                  (setq visible (agent-shell-fork-tree--visible)))))))
    (aft-perf-result "visible-set" variant measurement
                     (secure-hash 'sha256
                                  (prin1-to-string
                                   (sort (hash-table-keys visible) #'<)))
                     repeats)))

(defun aft-perf-select-with-render (id &optional session)
  "Select ID or SESSION by rebuilding, for the position-index ablation."
  (setq agent-shell-fork-tree--selected id
        agent-shell-fork-tree--selected-session session)
  (agent-shell-fork-tree--render))

(defun aft-perf-navigation (variant turns repeats)
  "Measure parent/child navigation in a TURNS-long rendered tree."
  (let* ((store (agent-shell-fork-tree--new-store "benchmark" "/tmp"))
         (history (cl-loop for i below turns collect (aft-perf-turn "main" i)))
         measurement digest)
    (agent-shell-fork-tree--ingest
     store '((sessionId . "main") (title . "main") (updatedAt . "1"))
     history t "main")
    (with-temp-buffer
      (agent-shell-fork-tree-view-mode)
      (setq agent-shell-fork-tree--store store
            agent-shell-fork-tree--focus "main"
            agent-shell-fork-tree--selected turns)
      (agent-shell-fork-tree--render)
      (garbage-collect)
      (cl-letf (((symbol-function 'agent-shell-fork-tree--select)
                 (if (equal variant "no-position-index")
                     #'aft-perf-select-with-render
                   (symbol-function 'agent-shell-fork-tree--select))))
        (setq measurement
              (benchmark-run 1
                (dotimes (_ repeats)
                  (agent-shell-fork-tree-parent)
                  (agent-shell-fork-tree-child)))))
      (setq digest
            (secure-hash 'sha256
                         (prin1-to-string
                          (list (buffer-string) agent-shell-fork-tree--selected
                                agent-shell-fork-tree--selected-session
                                (get-text-property (point) 'fork-tree-node))))))
    (aft-perf-result "navigation" variant measurement digest (* repeats 2))))

(defun aft-perf-run ()
  "Run general-case paired ablations and optionally write JSON results."
  (setq aft-perf-old-functions
        (mapcar
         (lambda (entry)
           (cons (cdr entry) (aft-perf-old-function (car entry) (cdr entry))))
         '(("agent-shell-fork-tree-store.el" . agent-shell-fork-tree--attach)
           ("agent-shell-fork-tree-acp.el" . agent-shell-fork-tree--scan)
           ("agent-shell-fork-tree.el" . agent-shell-fork-tree--visible))))
  (let* ((repeats (string-to-number (or (getenv "AFT_PERF_REPEATS") "5")))
         (cases
          `(("wide-children" . ,(lambda (variant) (aft-perf-wide-children variant 6000)))
            ("deep-replay" . ,(lambda (variant) (aft-perf-deep-replay variant 5000 10)))
            ("deep-append" . ,(lambda (variant) (aft-perf-deep-append variant 5000 10)))
            ("related-sparse" . ,(lambda (variant) (aft-perf-related "related-sparse" variant 10000 1000 50)))
            ("related-dense" . ,(lambda (variant) (aft-perf-related "related-dense" variant 10000 10000 50)))
            ("related-cold-lifecycle" . ,(lambda (variant) (aft-perf-related-lifecycle "related-cold-lifecycle" variant 1000 1 20)))
            ("related-warm-lifecycle" . ,(lambda (variant) (aft-perf-related-lifecycle "related-warm-lifecycle" variant 1000 10 10)))
            ("tools-small" . ,(lambda (variant) (aft-perf-tools "tools-small" variant 4 2 2000)))
            ("tools-large" . ,(lambda (variant) (aft-perf-tools "tools-large" variant 300 2 10)))
            ("paginated-tail" . ,(lambda (variant) (aft-perf-pages "paginated-tail" variant 6000 32 20)))
            ("paginated-count" . ,(lambda (variant) (aft-perf-pages "paginated-count" variant 6000 32 20)))
            ("progress-total" . ,(lambda (variant) (aft-perf-progress-total variant 6000 10)))
            ("priority-partition" . ,(lambda (variant) (aft-perf-priority variant 6000 600 20)))
            ("cached-discovery" . ,(lambda (variant) (aft-perf-cached-scan variant 6000 600 32)))
            ("endpoint-lookup" . ,(lambda (variant) (aft-perf-endpoint variant 5000 1000)))
            ("visible-set" . ,(lambda (variant) (aft-perf-visible variant 1500 60)))
            ("navigation" . ,(lambda (variant) (aft-perf-navigation variant 1500 30)))))
         (ablations
          '(("wide-children" . "no-child-tail")
            ("deep-replay" . "no-path-reuse")
            ("deep-append" . "no-path-reuse")
            ("related-sparse" . "first-index-candidate")
            ("related-dense" . "first-index-candidate")
            ("related-cold-lifecycle" . "first-index-candidate")
            ("related-warm-lifecycle" . "first-index-candidate")
            ("tools-small" . "tool-hash-candidate")
            ("tools-large" . "tool-hash-candidate")
            ("paginated-tail" . "no-page-tail")
            ("paginated-count" . "no-page-count")
            ("progress-total" . "no-progress-total")
            ("priority-partition" . "no-priority-hash")
            ("cached-discovery" . "historical-scan")
            ("endpoint-lookup" . "no-path-tail")
            ("visible-set" . "no-visible-cache")
            ("navigation" . "no-position-index")))
         expected-digests results)
    (dotimes (round repeats)
      (dolist (case (if (zerop (% round 2)) cases (reverse cases)))
        (let* ((scenario (car case)) (runner (cdr case))
               (ablation (cdr (assoc scenario ablations)))
               (variants (if (zerop (% round 2)) (list "all" ablation)
                           (list ablation "all"))))
          (dolist (variant variants)
            (let ((result (funcall runner variant)))
              (unless (assoc scenario expected-digests)
                (push (cons scenario (map-elt result 'digest)) expected-digests))
              (unless (equal (cdr (assoc scenario expected-digests))
                             (map-elt result 'digest))
                (error "%s ablation changed output: %s" scenario variant))
              (push result results)
              (princ (format "%s %-21s round=%d seconds=%.6f gc=%d\n"
                             scenario variant (1+ round)
                             (map-elt result 'seconds) (map-elt result 'gc-count))))))))
    (when-let* ((output (getenv "AFT_PERF_OUTPUT")))
      (with-temp-file output
        (insert
         (json-serialize
          `((emacs . ,emacs-version) (system . ,system-configuration)
            (baseline . ,aft-perf-baseline) (repeats . ,repeats)
            (parameters . ((wide-children . 6000) (deep-turns . 5000)
                           (deep-replays . 10) (deep-append-turns . 10)
                           (related-sessions . 10000) (related-queries . 50)
                           (lifecycle-sessions . 1000)
                           (cold-lifecycle-queries . 1) (cold-lifecycle-iterations . 20)
                           (warm-lifecycle-queries . 10) (warm-lifecycle-iterations . 10)
                           (small-tools . 4) (small-tool-repeats . 2000)
                           (large-tools . 300) (large-tool-repeats . 10)
                           (page-sessions . 6000) (page-repeats . 20)
                           (progress-repeats . 10) (discovery-sessions . 6000)
                           (discovery-page-size . 32) (navigation-turns . 1500)
                           (endpoint-lookups . 1000) (navigation-operations . 60)))
            (results . ,(vconcat (nreverse results))))))))))

;;; benchmark-general.el ends here
