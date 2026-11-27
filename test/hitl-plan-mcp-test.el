;;; hitl-plan-mcp-test.el --- ERT tests for hitl-plan-mcp.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mcpkit)
(require 'hitl-mcp)
(require 'hitl-plan-mcp)

(defvar hitl-plan-mcp-test--applied nil)

(defmacro hitl-plan-mcp-test--with-env (&rest body)
  "Run BODY with isolated plan store, registries and a stub domain."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "hitl-plan-mcp-test" t))
          (hitl-plan-directory dir)
          (hitl-plan--store (make-hash-table :test #'equal))
          (hitl-plan--op-registry (make-hash-table :test #'equal))
          (hitl-plan--domain-registry (make-hash-table :test #'eq))
          (hitl-plan-agent-may-apply nil)
          (hitl-plan-after-apply-functions nil)
          (hitl-plan-mcp-test--applied nil))
     (hitl-plan-register-domain 'stub
       :label "Stub"
       :resolve-target (lambda (tgt) tgt)
       :format-target (lambda (tgt) (cons (plist-get tgt :file) (plist-get tgt :name))))
     (hitl-plan-register-op 'stub 'mark :key ?m :queueable t
       :schema '((:name "tag" :type "string"))
       :from-json (lambda (json) (list (plist-get json :tag)))
       :apply (lambda (&rest a) (push (cons 'mark a) hitl-plan-mcp-test--applied)))
     (hitl-plan-register-op 'stub 'manual :key ?x
       :apply (lambda (&rest _) (push 'manual hitl-plan-mcp-test--applied)))
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (string-prefix-p "*hitl-plan: " (buffer-name b)) (kill-buffer b)))
       (delete-directory dir t))))

(defun hitl-plan-mcp-test--call (tool args)
  "Call hitl TOOL handler with ARGS and return its result."
  (let ((svc (hitl-mcp-register)))
    (funcall (mcpkit-tool-handler (gethash tool (mcpkit-service-tools svc)))
             args (lambda (_s r) r))))

(defun hitl-plan-mcp-test--create (&rest extra)
  (hitl-plan-mcp-test--call
   "plan_create"
   (append (list :domain "stub"
                 :targets [(:file "a.org" :name "A") (:file "b.org" :name "B")]
                 :ops [(:op "mark" :args (:tag "x"))]
                 :show :json-false)
           extra)))

(ert-deftest hitl-plan-mcp-test/registration ()
  (let ((tools (mcpkit-service-tools (hitl-mcp-register))))
    (dolist (n '("plan_domains" "plan_ops" "plan_create" "plan_list"
                 "plan_status" "plan_discard" "plan_execute"))
      (should (gethash n tools)))))

(ert-deftest hitl-plan-mcp-test/domains-and-ops ()
  (hitl-plan-mcp-test--with-env
    (let ((doms (plist-get (hitl-plan-mcp-test--call "plan_domains" nil) :domains)))
      (should (equal (plist-get (aref doms 0) :name) "stub")))
    (let* ((res (hitl-plan-mcp-test--call "plan_ops" '(:domain "stub")))
           (ops (append (plist-get res :ops) nil))
           (mark (seq-find (lambda (o) (equal (plist-get o :name) "mark")) ops))
           (manual (seq-find (lambda (o) (equal (plist-get o :name) "manual")) ops)))
      (should (eq (plist-get mark :queueable) t))
      (should (eq (plist-get manual :queueable) :json-false))
      (should (equal (plist-get mark :key) "m")))
    (should (plist-get (hitl-plan-mcp-test--call "plan_ops" '(:domain "nope")) :error))))

(ert-deftest hitl-plan-mcp-test/create-list-status-discard ()
  (hitl-plan-mcp-test--with-env
    (let* ((res (hitl-plan-mcp-test--create :description "demo"))
           (id (plist-get res :plan_id)))
      (should id)
      (should (equal (plist-get res :status) "draft"))
      (should (= (plist-get (plist-get res :counts) :total) 2))
      (should (equal (aref (plist-get (aref (plist-get res :ops) 0) :args) 0) "x"))
      (should (equal (plist-get res :description) "demo"))
      (let ((lst (hitl-plan-mcp-test--call "plan_list" nil)))
        (should (= (length (plist-get lst :plans)) 1)))
      (should (= (length (plist-get (hitl-plan-mcp-test--call "plan_list" '(:status "complete"))
                                    :plans))
                 0))
      (should (equal (plist-get (hitl-plan-mcp-test--call "plan_status" (list :plan_id id))
                                :plan_id)
                     id))
      (should (plist-get (hitl-plan-mcp-test--call "plan_status" '(:plan_id "bogus")) :error))
      ;; show defaults to opening the review buffer
      (hitl-plan-mcp-test--call
       "plan_create" '(:domain "stub" :targets [(:file "a")] :ops [(:op "mark")]))
      (should (cl-some (lambda (b) (string-prefix-p "*hitl-plan: " (buffer-name b)))
                       (buffer-list)))
      (let ((d (hitl-plan-mcp-test--call "plan_discard" (list :plan_id id))))
        (should (equal (plist-get d :status) "discarded"))
        (should-not (get-buffer (format "*hitl-plan: %s*" id)))))))

(ert-deftest hitl-plan-mcp-test/execute-awaiting-confirmation ()
  (hitl-plan-mcp-test--with-env
    (let* ((id (plist-get (hitl-plan-mcp-test--create) :plan_id))
           (res (hitl-plan-mcp-test--call "plan_execute" (list :plan_id id))))
      (should (equal (plist-get res :status) "awaiting_user_confirmation"))
      (should (equal (plist-get res :plan_id) id))
      (should (equal (plist-get res :plan_buffer) (format "*hitl-plan: %s*" id)))
      (should (get-buffer (plist-get res :plan_buffer)))
      (should-not hitl-plan-mcp-test--applied)
      (should (eq (hitl-plan-status (hitl-plan-get id)) 'draft)))))

(ert-deftest hitl-plan-mcp-test/execute-may-apply ()
  (hitl-plan-mcp-test--with-env
    (let* ((hitl-plan-agent-may-apply t)
           (id (plist-get (hitl-plan-mcp-test--create) :plan_id))
           (res (hitl-plan-mcp-test--call "plan_execute" (list :plan_id id :exclude [2]))))
      (should (equal (plist-get res :status) "complete"))
      (should (= (plist-get (plist-get res :counts) :done) 1))
      (should (= (plist-get (plist-get res :counts) :skipped) 1))
      (should (equal hitl-plan-mcp-test--applied '((mark "x")))))))

(ert-deftest hitl-plan-mcp-test/rejections ()
  (hitl-plan-mcp-test--with-env
    (let ((err (plist-get (hitl-plan-mcp-test--create :log_notes "each") :error)))
      (should (string-match-p "each" err)))
    (let ((err (plist-get (hitl-plan-mcp-test--call
                           "plan_create"
                           '(:domain "stub" :targets [(:file "a")] :ops [(:op "manual")]
                             :show :json-false))
                          :error)))
      (should (string-match-p "not queueable" err)))
    (should (plist-get (hitl-plan-mcp-test--create :log_notes "one") :error))
    (should (plist-get (hitl-plan-mcp-test--call
                        "plan_create" '(:domain "stub" :targets [(:file "a")] :ops [(:op "zzz")]))
                       :error))
    ;; a plan with a non-queueable op or `each' notes is not executable via MCP
    (let* ((hitl-plan-agent-may-apply t)
           (p1 (hitl-plan-create :domain 'stub :targets '((:file "a")) :ops '(manual)))
           (p2 (hitl-plan-create :domain 'stub :targets '((:file "a")) :ops '(mark)
                                 :log-notes 'each)))
      (should (string-match-p "not queueable"
                              (plist-get (hitl-plan-mcp-test--call
                                          "plan_execute" (list :plan_id (hitl-plan-id p1)))
                                         :error)))
      (should (string-match-p "each"
                              (plist-get (hitl-plan-mcp-test--call
                                          "plan_execute" (list :plan_id (hitl-plan-id p2)))
                                         :error)))
      (should-not hitl-plan-mcp-test--applied))))


(ert-deftest hitl-plan-mcp-test/create-with-directory ()
  ;; An explicit directory is honoured only when it is a registered
  ;; domain's store (here a second domain's).
  (hitl-plan-mcp-test--with-env
    (let ((other (make-temp-file "hitl-plan-mcp-other" t)))
      (hitl-plan-register-domain 'stub2 :resolve-target #'identity :directory other)
      (let* ((res (hitl-plan-mcp-test--create :directory other))
             (id (plist-get res :plan_id)))
        (unwind-protect
            (progn
              (should (file-exists-p (hitl-plan-file id other)))
              (should-not (file-exists-p (hitl-plan-file id dir)))
              (clrhash hitl-plan--store)
              (should-not (hitl-plan-get id))
              (should (equal id (plist-get (hitl-plan-mcp-test--call
                                            "plan_status" (list :plan_id id :directory other))
                                           :plan_id))))
          (delete-directory other t))))))

(ert-deftest hitl-plan-mcp-test/directory-outside-stores-rejected ()
  (hitl-plan-mcp-test--with-env
    (let* ((evil (make-temp-file "hitl-plan-mcp-evil" t))
           (plan (hitl-plan-create :domain 'stub :targets '((:file "a")) :ops '(mark)
                                   :directory evil))
           (id (hitl-plan-id plan)))
      (unwind-protect
          (progn
            (clrhash hitl-plan--store)
            (dolist (call `(("plan_create" (:domain "stub" :targets [(:file "a")]
                                            :ops [(:op "mark")] :show :json-false
                                            :directory ,evil))
                            ("plan_list" (:directory ,evil))
                            ("plan_status" (:plan_id ,id :directory ,evil))
                            ("plan_discard" (:plan_id ,id :directory ,evil))
                            ("plan_execute" (:plan_id ,id :directory ,evil))))
              (should (string-match-p "not allowed"
                                      (or (plist-get (hitl-plan-mcp-test--call (car call) (cadr call))
                                                     :error)
                                          ""))))
            (should (equal (list (concat id ".eld"))
                           (directory-files evil nil "\\`plan-.*\\.eld\\'" t))))
        (delete-directory evil t)))))

(ert-deftest hitl-plan-mcp-test/path-like-plan-id-rejected ()
  (hitl-plan-mcp-test--with-env
    (let* ((plan (hitl-plan-create :domain 'stub :targets '((:file "a")) :ops '(mark)))
           (file (hitl-plan-file (hitl-plan-id plan))))
      (clrhash hitl-plan--store)
      (dolist (bad (list file "x.eld" "../x" (concat (hitl-plan-id plan) ".eld")))
        (dolist (tool '("plan_status" "plan_discard" "plan_execute"))
          (should (string-match-p "Invalid plan_id"
                                  (or (plist-get (hitl-plan-mcp-test--call tool (list :plan_id bad))
                                                 :error)
                                      "")))))
      (should (plist-get (hitl-plan-mcp-test--call "plan_status" (list :plan_id (hitl-plan-id plan)))
                         :plan_id)))))

(ert-deftest hitl-plan-mcp-test/show-false-suppresses-review ()
  (hitl-plan-mcp-test--with-env
    (let (reviewed)
      (cl-letf (((symbol-function 'hitl-plan-review) (lambda (&rest _) (push t reviewed))))
        (let ((base (list :domain "stub" :targets [(:file "a")] :ops [(:op "mark")])))
          ;; mcpkit parses JSON false as nil; explicit false must not show.
          (hitl-plan-mcp-test--call "plan_create" (append base '(:show nil)))
          (should-not reviewed)
          (hitl-plan-mcp-test--call "plan_create" (append base '(:show :json-false)))
          (should-not reviewed)
          ;; absent or true shows
          (hitl-plan-mcp-test--call "plan_create" base)
          (should (= 1 (length reviewed)))
          (hitl-plan-mcp-test--call "plan_create" (append base '(:show t)))
          (should (= 2 (length reviewed))))))))

(ert-deftest hitl-plan-mcp-test/null-values-normalised ()
  (hitl-plan-mcp-test--with-env
    (let ((res (hitl-plan-mcp-test--call
                "plan_create"
                '(:domain "stub" :show :json-false
                  :targets [(:file "a" :name :null)]
                  :ops [(:op "mark" :args (:tag :null))]))))
      (should-not (plist-get res :error))
      (let ((plan (hitl-plan-get (plist-get res :plan_id))))
        (should (equal '(:file "a" :name nil)
                       (hitl-plan-step-target (car (hitl-plan-steps plan)))))
        (should (equal '(nil) (hitl-plan-op-args (car (hitl-plan-ops plan)))))))))

(ert-deftest hitl-plan-mcp-test/create-uses-domain-directory ()
  (hitl-plan-mcp-test--with-env
    (let ((other (make-temp-file "hitl-plan-mcp-other" t)))
      (setf (hitl-plan-domain-directory (hitl-plan-get-domain 'stub)) other)
      (unwind-protect
          (let ((id (plist-get (hitl-plan-mcp-test--create) :plan_id)))
            (should (file-exists-p (hitl-plan-file id other)))
            (clrhash hitl-plan--store)
            (should (equal (list id)
                           (mapcar (lambda (p) (plist-get p :plan_id))
                                   (plist-get (hitl-plan-mcp-test--call
                                               "plan_list" '(:domain "stub"))
                                              :plans)))))
        (delete-directory other t)))))

;;; Review follow-ups

(ert-deftest hitl-plan-mcp-test/exclude-sets-message-and-refreshes-buffer ()
  (hitl-plan-mcp-test--with-env
    (let* ((hitl-plan-agent-may-apply t)
           (id (plist-get (hitl-plan-mcp-test--create) :plan_id))
           (plan (hitl-plan-get id))
           (buf (hitl-plan-review plan nil)))
      (should (buffer-live-p buf))
      (should-not (string-match-p "excluded over MCP" (with-current-buffer buf (buffer-string))))
      (hitl-plan-mcp-test--call "plan_execute" (list :plan_id id :exclude [2]))
      (should (equal "excluded over MCP"
                     (hitl-plan-step-message (nth 1 (hitl-plan-steps plan)))))
      ;; The live review buffer was re-rendered with the new state.
      (with-current-buffer buf
        (should (string-match-p "excluded over MCP" (buffer-string)))
        (should (string-match-p "\\[complete\\]" (buffer-string)))))))

(ert-deftest hitl-plan-mcp-test/execute-without-review-buffer ()
  "Refreshing is a no-op when no review buffer exists."
  (hitl-plan-mcp-test--with-env
    (let* ((hitl-plan-agent-may-apply t)
           (id (plist-get (hitl-plan-mcp-test--create) :plan_id)))
      (should-not (get-buffer (format "*hitl-plan: %s*" id)))
      (should (equal "complete"
                     (plist-get (hitl-plan-mcp-test--call "plan_execute" (list :plan_id id))
                                :status)))
      (should-not (get-buffer (format "*hitl-plan: %s*" id))))))

(ert-deftest hitl-plan-mcp-test/null-top-level-args ()
  "JSON null for domain, status, plan_id and directory counts as absent."
  (hitl-plan-mcp-test--with-env
    (let ((id (plist-get (hitl-plan-mcp-test--create) :plan_id)))
      (should (= 1 (length (plist-get (hitl-plan-mcp-test--call
                                       "plan_list" '(:domain :null :status :null :directory :null))
                                      :plans))))
      (should (string-match-p "Unknown plan domain: file-target"
                              (plist-get (hitl-plan-mcp-test--call "plan_ops" '(:domain :null))
                                         :error)))
      (should (equal id (plist-get (hitl-plan-mcp-test--call
                                    "plan_status" (list :plan_id id :domain :null :directory :null))
                                   :plan_id)))
      (should (string-match-p "Invalid plan_id"
                              (plist-get (hitl-plan-mcp-test--call "plan_status" '(:plan_id :null))
                                         :error)))
      (let ((res (hitl-plan-mcp-test--call
                  "plan_create" '(:domain :null :show :json-false :directory :null
                                  :description :null :note :null :log_notes :null
                                  :targets [(:file "a")] :ops [(:op "mark")]))))
        ;; A null domain means the default, file-target, which this env lacks.
        (should (string-match-p "Unknown plan domain: file-target" (plist-get res :error)))))))

(ert-deftest hitl-plan-mcp-test/null-status-in-function-api ()
  (hitl-plan-mcp-test--with-env
    (hitl-plan-mcp-test--create)
    (should (= 1 (length (plist-get (hitl-plan-mcp-list :status :null :domain "stub") :plans))))))

(ert-deftest hitl-plan-mcp-test/function-api ()
  "The plain functions return the tool results without going through mcpkit."
  (hitl-plan-mcp-test--with-env
    (let* ((hitl-plan-agent-may-apply t)
           (res (hitl-plan-mcp-create :domain "stub" :show :json-false
                                      :targets [(:file "a.org" :name "A")]
                                      :ops [(:op "mark" :args (:tag "t"))]))
           (id (plist-get res :plan_id)))
      (should id)
      (should (equal "stub" (plist-get res :domain)))
      (should (equal id (plist-get (hitl-plan-mcp-status :plan_id id) :plan_id)))
      (should (= 1 (length (plist-get (hitl-plan-mcp-list :domain "stub" :status ["draft"])
                                      :plans))))
      (should (equal "complete" (plist-get (hitl-plan-mcp-execute :plan_id id) :status)))
      (should (equal '((mark "t")) hitl-plan-mcp-test--applied))
      (should (equal "discarded" (plist-get (hitl-plan-mcp-discard :plan_id id) :status)))
      (should (plist-get (hitl-plan-mcp-status :plan_id "nope") :error))
      (should (= 1 (length (plist-get (hitl-plan-mcp-domains) :domains))))
      (should (equal "stub" (plist-get (hitl-plan-mcp-ops :domain "stub") :domain))))))

(ert-deftest hitl-plan-mcp-test/show-p ()
  (should (hitl-plan-mcp-show-p nil))
  (should (hitl-plan-mcp-show-p '(:show t)))
  (should-not (hitl-plan-mcp-show-p '(:show nil)))
  (should-not (hitl-plan-mcp-show-p '(:show :json-false))))

(ert-deftest hitl-plan-mcp-test/autostart-registered ()
  (should (assq 'hitl mcpkit-autostart-alist)))

(ert-deftest hitl-plan-mcp-test/allowed-directories ()
  (hitl-plan-mcp-test--with-env
    (let ((other (make-temp-file "hitl-plan-mcp-other" t)))
      (unwind-protect
          (progn
            (hitl-plan-register-domain 'stub2 :directory (lambda () other))
            (hitl-plan-register-domain 'nodir)
            (let ((dirs (hitl-plan-mcp--allowed-directories)))
              (should (member (file-name-as-directory (expand-file-name dir)) dirs))
              (should (member (file-name-as-directory (expand-file-name other)) dirs))
              (should (cl-every (lambda (d) (string-suffix-p "/" d)) dirs))
              (should (= 2 (length (delete-dups (copy-sequence dirs)))))))
        (delete-directory other t)))))

(ert-deftest hitl-plan-mcp-test/parse-log-notes ()
  (let ((hitl-plan-log-notes 'ignore))
    (should (eq 'ignore (hitl-plan-mcp--parse-log-notes nil)))
    (should (eq 'one (hitl-plan-mcp--parse-log-notes "one")))
    (should (eq 'one (hitl-plan-mcp--parse-log-notes "ONE")))
    (should (eq 'one (hitl-plan-mcp--parse-log-notes 'one)))
    (should-error (hitl-plan-mcp--parse-log-notes "each"))
    (should-error (hitl-plan-mcp--parse-log-notes 'each))
    (should-error (hitl-plan-mcp--parse-log-notes "bogus"))
    (should-error (hitl-plan-mcp--parse-log-notes 5)))
  (let ((hitl-plan-log-notes 'one))
    (should (eq 'one (hitl-plan-mcp--parse-log-notes nil)))))

(ert-deftest hitl-plan-mcp-test/parse-op ()
  (hitl-plan-mcp-test--with-env
    (hitl-plan-register-op 'stub 'raw :queueable t :apply #'ignore)
    (hitl-plan-register-op 'stub 'checked :queueable t :apply #'ignore
                           :validate (lambda (x) (when (equal x "bad") "bad arg")))
    ;; Plist args go through :from-json; name may be `op' or `name'.
    (let ((op (hitl-plan-mcp--parse-op 'stub '(:op "mark" :args (:tag "z")))))
      (should (eq 'mark (hitl-plan-op-name op)))
      (should (equal '("z") (hitl-plan-op-args op))))
    (should (equal '("z") (hitl-plan-op-args
                           (hitl-plan-mcp--parse-op 'stub '(:name "mark" :args (:tag "z"))))))
    ;; Alist entries and hash tables work too.
    (should (equal '("q") (hitl-plan-op-args
                           (hitl-plan-mcp--parse-op 'stub '(("op" . "mark") ("args" . (:tag "q")))))))
    (let ((h (make-hash-table :test #'equal)))
      (puthash "op" "mark" h)
      (puthash "args" '(:tag "h") h)
      (should (equal '("h") (hitl-plan-op-args (hitl-plan-mcp--parse-op 'stub h)))))
    ;; Without :from-json: vectors and lists are the arg list; scalars wrap.
    (should (equal '(1 2) (hitl-plan-op-args (hitl-plan-mcp--parse-op 'stub '(:op "raw" :args [1 2])))))
    (should (equal '(1 2) (hitl-plan-op-args (hitl-plan-mcp--parse-op 'stub '(:op "raw" :args (1 2))))))
    (should (equal '("s") (hitl-plan-op-args (hitl-plan-mcp--parse-op 'stub '(:op "raw" :args "s")))))
    (should-not (hitl-plan-op-args (hitl-plan-mcp--parse-op 'stub '(:op "raw"))))
    ;; Errors.
    (should-error (hitl-plan-mcp--parse-op 'stub '(:args [1])))
    (should-error (hitl-plan-mcp--parse-op 'stub '(:op "manual")) :type 'user-error)
    (should-error (hitl-plan-mcp--parse-op 'stub '(:op "unknown")) :type 'user-error)
    (should (string-match-p "bad arg"
                            (error-message-string
                             (should-error (hitl-plan-mcp--parse-op
                                            'stub '(:op "checked" :args ["bad"]))))))))

(ert-deftest hitl-plan-mcp-test/targets ()
  (hitl-plan-mcp-test--with-env
    ;; Explicit targets (vector or list) win over :build-batch.
    (should (equal '((:file "a")) (hitl-plan-mcp--targets 'stub nil '((:file "a")))))
    ;; No targets and no :build-batch is an error.
    (should-error (hitl-plan-mcp--targets 'stub '(:scope (:x 1)) nil))
    (let ((shape nil) (scopes nil))
      (hitl-plan-register-domain 'built
        :build-batch (lambda (scope) (push scope scopes) (funcall shape)))
      (dolist (fn (list (lambda () '((:file "a") (:file "b")))            ; bare list
                        (lambda () '(nil (:file "a") (:file "b")))        ; (nil . targets)
                        (lambda () (cons (generate-new-buffer " *h*")     ; (buffer . targets)
                                         '((:file "a") (:file "b"))))))
        (setq shape fn)
        (should (equal '((:file "a") (:file "b"))
                       (hitl-plan-mcp--targets 'built '(:scope (:s 1)) nil))))
      (should (equal '((:s 1) (:s 1) (:s 1)) scopes)))))

(ert-deftest hitl-plan-mcp-test/targets-batch-buffer-not-leaked ()
  "A buffer created by :build-batch is killed; a pre-existing one is kept."
  (hitl-plan-mcp-test--with-env
    (let ((existing (generate-new-buffer " *hitl-existing*"))
          created)
      (unwind-protect
          (progn
            (hitl-plan-register-domain 'fresh
              :resolve-target #'identity
              :build-batch (lambda (_scope)
                             (setq created (generate-new-buffer " *hitl-batch*"))
                             (cons created '((:file "a")))))
            (hitl-plan-register-domain 'reuse
              :resolve-target #'identity
              :build-batch (lambda (_scope) (cons existing '((:file "a")))))
            (hitl-plan-register-op 'fresh 'mark :queueable t :apply #'ignore)
            (hitl-plan-register-op 'reuse 'mark :queueable t :apply #'ignore)
            (let ((before (length (buffer-list))))
              (let ((res (hitl-plan-mcp-create :domain "fresh" :show :json-false
                                               :ops [(:op "mark")])))
                (should-not (plist-get res :error))
                (should (= 1 (plist-get (plist-get res :counts) :total))))
              (should-not (buffer-live-p created))
              (should (= before (length (buffer-list)))))
            (should-not (plist-get (hitl-plan-mcp-create :domain "reuse" :show :json-false
                                                         :ops [(:op "mark")])
                                   :error))
            (should (buffer-live-p existing)))
        (kill-buffer existing)))))


;;; Security regression tests

(ert-deftest hitl-plan-mcp-test/sec-origin-marked-and-persisted ()
  (hitl-plan-mcp-test--with-env
    (let* ((res (hitl-plan-mcp-test--create))
           (id (plist-get res :plan_id)))
      (should (hitl-plan-origin-mcp-p (hitl-plan-get id)))
      (clrhash hitl-plan--store)
      (should (hitl-plan-origin-mcp-p (hitl-plan-load id)))
      ;; Interactive creation is not mcp origin.
      (should-not (hitl-plan-origin-mcp-p
                   (hitl-plan-create :domain 'stub :targets '((:file "a")) :ops '(mark)))))))

(ert-deftest hitl-plan-mcp-test/sec-smuggled-args-rejected ()
  (hitl-plan-mcp-test--with-env
    (dolist (extra '((:origin "user") (:status "complete") (:metadata (:approved t))
                     (:id "plan-evil")))
      (let ((res (apply #'hitl-plan-mcp-test--create extra)))
        (should (plist-get res :error))
        (should-not (plist-get res :plan_id))))
    (should (null (hitl-plan-list)))))

(ert-deftest hitl-plan-mcp-test/sec-create-does-not-steal-focus ()
  (hitl-plan-mcp-test--with-env
    (let ((buf (current-buffer))
          (win (selected-window))
          (shown (window-buffer (selected-window))))
      (hitl-plan-mcp-create :domain "stub" :targets [(:file "a")] :ops [(:op "mark")])
      (should (eq (current-buffer) buf))
      (should (eq (selected-window) win))
      (should (eq (window-buffer win) shown))
      (let ((id (hitl-plan-id (car (hitl-plan-list)))))
        (hitl-plan-mcp-execute :plan_id id)
        (should (eq (current-buffer) buf))
        (should (eq (window-buffer (selected-window)) shown))))))

(ert-deftest hitl-plan-mcp-test/sec-caps ()
  (hitl-plan-mcp-test--with-env
    (let ((hitl-plan-max-steps 1))
      (should (string-match-p "steps" (plist-get (hitl-plan-mcp-test--create) :error)))
      (should (null (hitl-plan-list))))
    (let ((hitl-plan-max-string-length 10))
      (should (string-match-p "too long" (plist-get (hitl-plan-mcp-test--create
                                                     :description (make-string 11 ?x))
                                                    :error)))
      (should (string-match-p "too long"
                              (plist-get (hitl-plan-mcp-create
                                          :domain "stub" :targets [(:file "a")]
                                          :ops [(:op "mark" :args (:tag "xxxxxxxxxxxxxxxxxxxx"))]
                                          :show :json-false)
                                         :error)))
      (should (string-match-p "too long"
                              (plist-get (hitl-plan-mcp-test--create
                                          :log_notes "one" :note (make-string 11 ?x))
                                         :error)))
      (should (string-match-p "too long"
                              (plist-get (hitl-plan-mcp-create
                                          :domain "stub"
                                          :targets (list (list :file (make-string 11 ?x)))
                                          :ops [(:op "mark")] :show :json-false)
                                         :error))))
    (should (null (hitl-plan-list)))))

(ert-deftest hitl-plan-mcp-test/sec-denull-long-list ()
  (let ((l (make-list 50000 '(:a :null))))
    (should (= 50000 (length (hitl-plan-mcp-denull l))))
    (should (equal (car (hitl-plan-mcp-denull l)) '(:a nil))))
  (should (equal (hitl-plan-mcp-denull '(1 :null (2 . :null) [:null]))
                 '(1 nil (2) [nil]))))

(ert-deftest hitl-plan-mcp-test/sec-many-targets ()
  (hitl-plan-mcp-test--with-env
    (let ((hitl-plan-max-steps 6000)
          (hitl-plan-save-buffers nil))
      (let ((res (hitl-plan-mcp-create
                  :domain "stub"
                  :targets (make-list 5000 '(:file "a" :name "n"))
                  :ops [(:op "mark")] :show :json-false)))
        (should-not (plist-get res :error))
        (should (= 5000 (plist-get (plist-get res :counts) :total)))))))

(ert-deftest hitl-plan-mcp-test/sec-type-validation ()
  (hitl-plan-mcp-test--with-env
    (dolist (args '((:targets "abc" :ops [(:op "mark")])
                    (:targets [7] :ops [(:op "mark")])
                    (:targets [(:file "a")] :ops "mark")
                    (:targets [(:file "a")] :ops [7])
                    (:targets [(:file "a")] :ops [(:op "mark")] :description 5)
                    (:targets [(:file "a")] :ops [(:op "mark")] :description (:x 1))
                    (:targets [(:file "a")] :ops [(:op "mark")] :log_notes "one" :note (:x 1))
                    (:targets [(:file "a")] :ops [(:op "mark")] :scope "x")
                    (:targets [(:file "a")] :ops [(:op "mark" :args 7)])))
      (let ((res (apply #'hitl-plan-mcp-create :domain "stub" :show :json-false args)))
        (should (stringp (plist-get res :error)))))
    (should (null (hitl-plan-list)))))

(ert-deftest hitl-plan-mcp-test/sec-list-and-status-restricted-to-allowed-dirs ()
  (hitl-plan-mcp-test--with-env
    (let* ((evil (make-temp-file "hitl-plan-mcp-evil" t))
           (plan (hitl-plan-create :domain 'stub :targets '((:file "a")) :ops '(mark)
                                   :directory evil))
           (id (hitl-plan-id plan)))
      (unwind-protect
          (progn
            (should (gethash id hitl-plan--store))
            (should-not (cl-some (lambda (p) (equal (plist-get p :plan_id) id))
                                 (plist-get (hitl-plan-mcp-list) :plans)))
            (should (plist-get (hitl-plan-mcp-status :plan_id id) :error))
            (should (plist-get (hitl-plan-mcp-discard :plan_id id) :error))
            (should (plist-get (hitl-plan-mcp-execute :plan_id id) :error)))
        (delete-directory evil t)))))

(ert-deftest hitl-plan-mcp-test/sec-discard-only-mcp-origin ()
  (hitl-plan-mcp-test--with-env
    (let* ((user (hitl-plan-create :domain 'stub :targets '((:file "a")) :ops '(mark)))
           (res (hitl-plan-mcp-discard :plan_id (hitl-plan-id user))))
      (should (plist-get res :error))
      (should (eq (hitl-plan-status user) 'draft)))
    (let* ((id (plist-get (hitl-plan-mcp-test--create) :plan_id))
           (res (hitl-plan-mcp-discard :plan_id id)))
      (should-not (plist-get res :error))
      (should (equal (plist-get res :status) "discarded")))))

(ert-deftest hitl-plan-mcp-test/sec-sanitize-target-hook ()
  (hitl-plan-mcp-test--with-env
    (hitl-plan-register-domain 'chk
      :resolve-target #'identity
      :sanitize-target (lambda (tgt)
                         (unless (equal (plist-get tgt :file) "ok")
                           (error "Bad target"))
                         (list :file "ok" :fresh t)))
    (hitl-plan-register-op 'chk 'mark :queueable t :apply #'ignore)
    (let ((res (hitl-plan-mcp-create :domain "chk" :ops [(:op "mark")] :show :json-false
                                     :targets [(:file "ok" :fresh nil)])))
      (should-not (plist-get res :error))
      (should (eq t (plist-get (hitl-plan-step-target
                                (car (hitl-plan-steps (hitl-plan-get (plist-get res :plan_id)))))
                               :fresh))))
    (let ((res (hitl-plan-mcp-create :domain "chk" :ops [(:op "mark")] :show :json-false
                                     :targets [(:file "ok") (:file "bad")])))
      (should (string-match-p "Bad target" (plist-get res :error))))
    (should (= 1 (length (hitl-plan-list))))))

(ert-deftest hitl-plan-mcp-test/sec-plan-id-charset ()
  (dolist (bad '("~" "~root" "a\nb" "x\0y" " " "ssh:h:x" "" "-x" ".x" "a/b" "x.eld"))
    (should-error (hitl-plan-mcp--plan-id (list :plan_id bad))))
  (should (equal "plan-20260101-120000-0abc"
                 (hitl-plan-mcp--plan-id '(:plan_id "plan-20260101-120000-0abc")))))

(ert-deftest hitl-plan-mcp-test/sec-approved-p ()
  (hitl-plan-mcp-test--with-env
    (let ((user (hitl-plan-create :domain 'stub :targets '((:file "a")) :ops '(mark)))
          (agent (hitl-plan-create :domain 'stub :targets '((:file "a")) :ops '(mark)
                                   :metadata '(:origin mcp))))
      (should (hitl-plan-approved-p user))
      (should-not (hitl-plan-approved-p agent))
      (let ((hitl-plan-agent-may-apply t)) (should (hitl-plan-approved-p agent)))
      (setf (hitl-plan-metadata agent) '(:origin mcp :approved t))
      (should (hitl-plan-approved-p agent)))))

(ert-deftest hitl-plan-mcp-test/apply-skip-summary ()
  "MCP status and execute report skipped steps triggered by :apply."
  (hitl-plan-mcp-test--with-env
    (hitl-plan-register-op 'stub 'maybe :queueable t
      :apply (lambda ()
               (when (equal (plist-get hitl-plan-current-target :file) "b.org")
                 (hitl-plan-skip "unpushed commits"))))
    (let* ((hitl-plan-agent-may-apply t)
           (created (hitl-plan-mcp-create
                     :domain "stub"
                     :targets [(:file "a.org" :name "A") (:file "b.org" :name "B")]
                     :ops [(:op "maybe")]
                     :show :json-false))
           (id (plist-get created :plan_id))
           (res (hitl-plan-mcp-execute :plan_id id))
           (status-res (hitl-plan-mcp-status :plan_id id))
           (counts (plist-get res :counts))
           (steps (plist-get res :steps)))
      (should (equal "complete" (plist-get res :status)))
      (should (= 2 (plist-get counts :total)))
      (should (= 1 (plist-get counts :done)))
      (should (= 1 (plist-get counts :skipped)))
      (should (= 0 (plist-get counts :failed)))
      (should (equal "skipped" (plist-get (aref steps 1) :status)))
      (should (equal "unpushed commits" (plist-get (aref steps 1) :message)))
      (should (equal counts (plist-get status-res :counts))))))

(provide 'hitl-plan-mcp-test)
;;; hitl-plan-mcp-test.el ends here
