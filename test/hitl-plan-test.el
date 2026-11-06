;;; hitl-plan-test.el --- Tests for hitl-plan -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -L /home/tychoish/src/hitl -l ert \
;;     -l /home/tychoish/src/hitl/test/hitl-plan-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'hitl-plan)

(defvar hitl-plan-test--log nil "Names of applied things, in order.")

(defmacro hitl-plan-test--with-env (&rest body)
  "Run BODY with isolated directory, store, registries, and hooks."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "hitl-plan-test" t))
          (hitl-plan-directory dir)
          (hitl-plan--store (make-hash-table :test #'equal))
          (hitl-plan--op-registry (make-hash-table :test #'equal))
          (hitl-plan--domain-registry (copy-hash-table hitl-plan--domain-registry))
          (hitl-plan-step-functions nil)
          (hitl-plan-after-apply-functions nil)
          (hitl-plan-save-buffers nil)
          (hitl-plan-test--log nil))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun hitl-plan-test--file (text)
  "Return a new temp file containing TEXT."
  (let ((f (make-temp-file "hitl-plan-target" nil ".txt")))
    (with-temp-file f (insert text))
    f))

(defun hitl-plan-test--targets (file &rest lines)
  "Return file-target targets for LINES (strings) of FILE."
  (mapcar (lambda (l) (list :file file :snippet l)) lines))

(defun hitl-plan-test--register-upcase ()
  "Register an upcase op in file-target that appends to the log."
  (hitl-plan-register-op 'file-target 'mark
    :key ?m :queueable t
    :apply (lambda (&optional tag)
             (push (format "%s@%s" (or tag "x")
                           (buffer-substring (line-beginning-position)
                                             (line-end-position)))
                   hitl-plan-test--log))))

;;; Structs and persistence

(ert-deftest hitl-plan-test-roundtrip ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'file-target 'a :apply #'ignore)
    (hitl-plan-register-op 'file-target 'b :apply #'ignore :order-last t)
    (let* ((plan (hitl-plan-create
                  :scope '(:buffer "x") :description "d" :note "n"
                  :log-notes 'one :metadata '(:k 1)
                  :targets (list '(:file "/x" :line 1) '(:file "/y" :snippet "s"))
                  :ops '((b) (a 1 2)))))
      (setf (hitl-plan-step-status (car (hitl-plan-steps plan))) 'done
            (hitl-plan-step-message (car (hitl-plan-steps plan))) "ok")
      (hitl-plan-save plan)
      (clrhash hitl-plan--store)
      (let ((l (hitl-plan-load (hitl-plan-id plan))))
        (should (hitl-plan-p l))
        (should (equal (hitl-plan-id l) (hitl-plan-id plan)))
        (should (eq (hitl-plan-domain l) 'file-target))
        (should (equal (hitl-plan-description l) "d"))
        (should (eq (hitl-plan-log-notes l) 'one))
        (should (equal (hitl-plan-metadata l) '(:k 1)))
        (should (equal (mapcar #'hitl-plan-op-name (hitl-plan-ops l)) '(a b)))
        (should (equal (hitl-plan-op-args (car (hitl-plan-ops l))) '(1 2)))
        (should (= 4 (length (hitl-plan-steps l))))
        (let ((s (car (hitl-plan-steps l))))
          (should (eq (hitl-plan-step-status s) 'done))
          (should (equal (hitl-plan-step-message s) "ok"))
          (should (eq (hitl-plan-step-op s) (car (hitl-plan-ops l)))))
        (should (eq l (hitl-plan-get (hitl-plan-id plan))))))))

(ert-deftest hitl-plan-test-corrupt-file ()
  (hitl-plan-test--with-env
    (let ((good (hitl-plan-create :targets '((:file "/x")) :ops '(a))))
      (with-temp-file (hitl-plan-file "bad") (insert "(:version 1 :id \"bad\" "))
      (with-temp-file (hitl-plan-file "wrong") (insert "(:version 99 :id \"wrong\")"))
      (clrhash hitl-plan--store)
      (should-not (hitl-plan-load "bad"))
      (should-not (hitl-plan-load "wrong"))
      (should-not (hitl-plan-load "missing"))
      (should-not (hitl-plan-get "bad"))
      (should (equal (mapcar #'hitl-plan-id (hitl-plan-list))
                     (list (hitl-plan-id good)))))))

(ert-deftest hitl-plan-test-list-discard-summary ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'file-target 'a :apply #'ignore :describe (lambda (&rest _) "desc-a"))
    (let ((p1 (hitl-plan-create :targets '((:file "/x" :line 1)) :ops '(a)))
          (p2 (hitl-plan-create :targets '((:file "/y" :line 2)) :ops '(a))))
      (hitl-plan-discard p2)
      (should (eq 'discarded (hitl-plan-status (hitl-plan-get (hitl-plan-id p2)))))
      (should (equal (list p1) (hitl-plan-list '(draft))))
      (should (= 2 (length (hitl-plan-list))))
      (let ((sum (hitl-plan-summary p1)))
        (should (equal (plist-get sum :status) "draft"))
        (should (equal (plist-get sum :domain) "file-target"))
        (should (= 1 (plist-get (plist-get sum :counts) :pending)))
        (should (equal (plist-get (aref (plist-get sum :ops) 0) :description) "desc-a")))
      (should-error (hitl-plan-execute p2) :type 'user-error))))

(ert-deftest hitl-plan-test-atomic-no-temp-leftover ()
  (hitl-plan-test--with-env
    (let ((p (hitl-plan-create :targets '((:file "/x")) :ops '(a))))
      (hitl-plan-save p)
      (should (equal (list (concat (hitl-plan-id p) ".eld"))
                     (directory-files hitl-plan-directory nil "\\`[^.]"))))))

;;; Registration and ordering

(ert-deftest hitl-plan-test-domain-registration ()
  (hitl-plan-test--with-env
    (should (hitl-plan-get-domain 'file-target))
    (hitl-plan-register-domain 'other :label "Other" :driver #'ignore)
    (should (equal "Other" (hitl-plan-domain-label (hitl-plan-get-domain 'other))))
    (should (eq #'ignore (hitl-plan-domain-driver (hitl-plan-get-domain "other"))))
    (should (memq 'other (mapcar #'hitl-plan-domain-name (hitl-plan-registered-domains))))
    (should-error (hitl-plan-create :domain 'nope :ops '(a)) :type 'user-error)))

(ert-deftest hitl-plan-test-op-registration ()
  (hitl-plan-test--with-env
    (hitl-plan-register-domain 'other)
    (hitl-plan-register-op 'file-target 'a :key ?a :apply #'ignore :label "Alpha")
    (hitl-plan-register-op 'other 'a :key ?a :apply #'identity)
    (hitl-plan-register-op t 'common :key ?c :apply #'ignore)
    (should (eq #'ignore (hitl-plan-op-spec-apply (hitl-plan-get-op 'file-target 'a))))
    (should (eq #'identity (hitl-plan-op-spec-apply (hitl-plan-get-op 'other "a"))))
    (should (equal "Alpha" (hitl-plan-op-spec-label (hitl-plan-get-op 'file-target 'a))))
    ;; Domain t fallback.
    (should (hitl-plan-get-op 'other 'common))
    (should (eq 'common (hitl-plan-op-spec-name (hitl-plan-op-for-key 'other ?c))))
    (should-not (hitl-plan-get-op 'other 'missing))
    (should (= 2 (length (hitl-plan-registered-ops 'other))))
    (should (= 3 (length (hitl-plan-registered-ops))))
    ;; Key collision inside one domain.
    (should-error (hitl-plan-register-op 'other 'z :key ?a :apply #'ignore))
    ;; Re-registering the same op is fine.
    (hitl-plan-register-op 'other 'a :key ?a :apply #'ignore)
    (should (equal "a -> x"
                   (progn (hitl-plan-register-op 'other 'd :describe (lambda (x) (format "a -> %s" x)))
                          (hitl-plan-describe-op 'other (hitl-plan-op--make :name 'd :args '("x"))))))
    (should (equal "zzz 1 2" (hitl-plan-describe-op 'other (hitl-plan-op--make :name 'zzz :args '(1 2)))))))

(ert-deftest hitl-plan-test-order-last ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'file-target 'archive :order-last t :apply #'ignore)
    (hitl-plan-register-op 'file-target 'x :apply #'ignore)
    (hitl-plan-register-op 'file-target 'y :apply #'ignore)
    (should (equal '(x y archive)
                   (hitl-plan-order-ops 'file-target '(archive x y))))
    (let ((plan (hitl-plan-create :targets '((:file "/a") (:file "/b"))
                                  :ops '(archive x))))
      (should (equal '(x archive) (mapcar #'hitl-plan-op-name (hitl-plan-ops plan))))
      (should (equal '(x x archive archive)
                     (mapcar (lambda (s) (hitl-plan-op-name (hitl-plan-step-op s)))
                             (hitl-plan-steps plan))))
      (hitl-plan-add-op plan '(y))
      (should (equal '(x y archive) (mapcar #'hitl-plan-op-name (hitl-plan-ops plan))))
      (should (= 6 (length (hitl-plan-steps plan))))
      (should (equal '(1 2 3 4 5 6)
                     (sort (mapcar #'hitl-plan-step-id (hitl-plan-steps plan)) #'<))))))

;;; file-target domain

(ert-deftest hitl-plan-test-file-target-resolve ()
  (let ((f (hitl-plan-test--file "one\ntwo\nthree\ntwo\n")))
    (unwind-protect
        (progn
          (let ((m (hitl-plan-file-target-resolve (list :file f :line 3))))
            (should (equal "three" (with-current-buffer (marker-buffer m)
                                     (goto-char m)
                                     (buffer-substring (line-beginning-position)
                                                       (line-end-position))))))
          ;; Duplicate snippet: pick closest to :pos.
          (let ((m (hitl-plan-file-target-resolve (list :file f :snippet "two" :pos 14))))
            (should (= 15 (marker-position m))))
          (should-not (hitl-plan-file-target-resolve (list :file f :snippet "nope")))
          (should-not (hitl-plan-file-target-resolve (list :file "/nonexistent/zz" :line 1)))
          (should (equal '("x" . "line 2")
                         (let ((r (hitl-plan-file-target-format (list :file "/a/x" :line 2))))
                           r))))
      (when-let* ((b (get-file-buffer f))) (kill-buffer b))
      (delete-file f))))

;;; Execution

(ert-deftest hitl-plan-test-default-iterator ()
  (hitl-plan-test--with-env
    (hitl-plan-test--register-upcase)
    (hitl-plan-register-op 'file-target 'boom :apply (lambda () (error "kaboom")))
    (let* ((f (hitl-plan-test--file "alpha\nbeta\ngamma\n"))
           (plan (hitl-plan-create
                  :targets (append (hitl-plan-test--targets f "alpha" "beta")
                                   (list (list :file f :snippet "gone")))
                  :ops '((mark "T") (boom) (nope)))))
      (unwind-protect
          (progn
            (hitl-plan-execute plan)
            (should (eq 'complete (hitl-plan-status plan)))
            (should (equal '("T@alpha" "T@beta") (reverse hitl-plan-test--log)))
            (let ((by-op (lambda (n) (seq-filter (lambda (s) (eq n (hitl-plan-op-name (hitl-plan-step-op s))))
                                                 (hitl-plan-steps plan)))))
              (should (equal '(done done failed)
                             (mapcar #'hitl-plan-step-status (funcall by-op 'mark))))
              (should (equal "target no longer resolves"
                             (hitl-plan-step-message (nth 2 (funcall by-op 'mark)))))
              (should (equal "kaboom" (hitl-plan-step-message (car (funcall by-op 'boom)))))
              (should (string-match-p "Unknown operation: nope"
                                      (hitl-plan-step-message (car (funcall by-op 'nope))))))
            ;; Persisted.
            (clrhash hitl-plan--store)
            (should (eq 'complete (hitl-plan-status (hitl-plan-get (hitl-plan-id plan)))))
            (should-error (hitl-plan-execute plan) :type 'user-error))
        (delete-file f)))))

(ert-deftest hitl-plan-test-validate-and-only-op ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'file-target 'v
      :validate (lambda (x) (when (equal x "bad") "bad arg"))
      :apply (lambda (x) (push x hitl-plan-test--log)))
    (let* ((f (hitl-plan-test--file "alpha\n"))
           (plan (hitl-plan-create :targets (hitl-plan-test--targets f "alpha")
                                   :ops '((v "ok") (v "bad")))))
      (unwind-protect
          (progn
            (hitl-plan-execute plan (car (hitl-plan-ops plan)))
            (should (equal '("ok") hitl-plan-test--log))
            (should (eq 'paused (hitl-plan-status plan)))
            (hitl-plan-execute plan)
            (should (eq 'complete (hitl-plan-status plan)))
            (should (equal "bad arg" (hitl-plan-step-message (cadr (hitl-plan-steps plan))))))
        (delete-file f)))))

(ert-deftest hitl-plan-test-skipped-steps-not-run ()
  (hitl-plan-test--with-env
    (hitl-plan-test--register-upcase)
    (let* ((f (hitl-plan-test--file "alpha\nbeta\n"))
           (plan (hitl-plan-create :targets (hitl-plan-test--targets f "alpha" "beta")
                                   :ops '(mark))))
      (unwind-protect
          (progn
            (setf (hitl-plan-step-status (car (hitl-plan-steps plan))) 'skipped)
            (hitl-plan-execute plan)
            (should (equal '("x@beta") hitl-plan-test--log))
            (should (eq 'complete (hitl-plan-status plan))))
        (delete-file f)))))

(ert-deftest hitl-plan-test-custom-driver ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply (lambda () (push 'applied hitl-plan-test--log)))
    (let (calls)
      (hitl-plan-register-domain 'drv
        :resolve-target (lambda (target) (and (not (eq target 'gone)) (list 'ctx target)))
        :driver (lambda (plan op pending resolved)
                  (push (list (hitl-plan-op-name op) (length pending) (length resolved)
                              (hitl-plan-batch-buffer-test-p))
                        calls)
                  (dolist (cell (reverse resolved))
                    (hitl-plan-run-step plan (car cell) op (cdr cell)))))
      (let ((plan (hitl-plan-create :domain 'drv :targets '(a b gone) :ops '(mark))))
        (hitl-plan-execute plan)
        (should (equal '((mark 2 2 t)) calls))
        (should (equal '(done done failed) (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
        (should (equal "target no longer resolves"
                       (hitl-plan-step-message (nth 2 (hitl-plan-steps plan)))))
        (should (eq 'complete (hitl-plan-status plan)))
        (should (= 2 (length hitl-plan-test--log)))))))

(defun hitl-plan-batch-buffer-test-p ()
  "Return t; confirms `hitl-plan-batch-buffer' is bound-able."
  (boundp 'hitl-plan-batch-buffer))

(ert-deftest hitl-plan-test-custom-rebuild-batch ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply #'ignore)
    (let (seen)
      (hitl-plan-register-domain 'drv
        :rebuild-batch (lambda (plan)
                         (cons 'the-buffer
                               (list (cons (car (hitl-plan-steps plan)) 'r1))))
        :driver (lambda (plan op _pending resolved)
                  (push hitl-plan-batch-buffer seen)
                  (dolist (cell resolved)
                    (hitl-plan-run-step plan (car cell) op (cdr cell)))))
      (let ((plan (hitl-plan-create :domain 'drv :targets '(a b) :ops '(mark))))
        (hitl-plan-execute plan)
        (should (equal '(the-buffer) seen))
        (should (equal '(done failed) (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
        (should (equal "target no longer resolves"
                       (hitl-plan-step-message (cadr (hitl-plan-steps plan)))))))))

(ert-deftest hitl-plan-test-quit-pause-and-resume ()
  (hitl-plan-test--with-env
    (hitl-plan-test--register-upcase)
    (let* ((f (hitl-plan-test--file "alpha\nbeta\ngamma\n"))
           (plan (hitl-plan-create :targets (hitl-plan-test--targets f "alpha" "beta" "gamma")
                                   :ops '(mark)))
           (quit-flag nil))
      (unwind-protect
          (progn
            ;; A C-g arriving while a step hook runs is deferred by
            ;; `inhibit-quit' and observed between steps.
            (add-hook 'hitl-plan-step-functions
                      (lambda (step _plan)
                        (when (= 2 (hitl-plan-step-id step))
                          (setq quit-flag t)))
                      nil nil)
            (hitl-plan-execute plan)
            (should-not quit-flag)
            (should (eq 'paused (hitl-plan-status plan)))
            (should (equal '(done done pending) (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
            ;; Persisted as paused.
            (clrhash hitl-plan--store)
            (let ((loaded (hitl-plan-get (hitl-plan-id plan))))
              (should (eq 'paused (hitl-plan-status loaded)))
              (setq hitl-plan-step-functions nil)
              (hitl-plan-execute loaded)
              (should (eq 'complete (hitl-plan-status loaded)))
              (should (equal '("x@alpha" "x@beta" "x@gamma") (reverse hitl-plan-test--log)))))
        (delete-file f)))))

(ert-deftest hitl-plan-test-quit-during-apply ()
  (hitl-plan-test--with-env
    (let ((interrupt t))
      (hitl-plan-register-op 'file-target 'flaky
        :apply (lambda ()
                 (when interrupt
                   (setq interrupt nil)
                   (signal 'quit nil))
                 (push 'ran hitl-plan-test--log)))
      (let* ((f (hitl-plan-test--file "alpha\nbeta\n"))
             (plan (hitl-plan-create :targets (hitl-plan-test--targets f "alpha" "beta")
                                     :ops '(flaky)))
             (quit-flag nil))
        (unwind-protect
            (progn
              (hitl-plan-execute plan)
              (should-not quit-flag)
              (should (eq 'paused (hitl-plan-status plan)))
              ;; Interrupted step is left pending, not marked done.
              (should (equal '(pending pending)
                             (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
              (hitl-plan-execute plan)
              (should (eq 'complete (hitl-plan-status plan)))
              (should (equal '(ran ran) hitl-plan-test--log)))
          (delete-file f))))))

(ert-deftest hitl-plan-test-driver-quit-pauses ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply #'ignore)
    (let ((interrupt t))
      (hitl-plan-register-domain 'drv
        :resolve-target (lambda (target) target)
        :driver (lambda (plan op _pending resolved)
                  (dolist (cell resolved)
                    (hitl-plan-run-step plan (car cell) op (cdr cell))
                    (when interrupt
                      (setq interrupt nil)
                      (signal 'quit nil)))))
      (let ((plan (hitl-plan-create :domain 'drv :targets '(a b) :ops '(mark)))
            (quit-flag nil))
        (hitl-plan-execute plan)
        (should (eq 'paused (hitl-plan-status plan)))
        (should (equal '(done pending) (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
        (hitl-plan-execute plan)
        (should (eq 'complete (hitl-plan-status plan)))))))

;;; Error handling

(ert-deftest hitl-plan-test-driver-error-pauses ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply #'ignore)
    (hitl-plan-register-domain 'drv
      :resolve-target (lambda (target) target)
      :driver (lambda (&rest _) (error "driver boom")))
    (let ((plan (hitl-plan-create :domain 'drv :targets '(a b) :ops '(mark))))
      (should-error (hitl-plan-execute plan))
      (should (eq 'paused (hitl-plan-status plan)))
      (clrhash hitl-plan--store)
      (should (eq 'paused (hitl-plan-status (hitl-plan-load (hitl-plan-id plan))))))))

(ert-deftest hitl-plan-test-rebuild-batch-error-pauses ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply #'ignore)
    (hitl-plan-register-domain 'drv
      :resolve-target (lambda (target) target)
      :rebuild-batch (lambda (&rest _) (error "rebuild boom"))
      :driver (lambda (&rest _) nil))
    (let ((plan (hitl-plan-create :domain 'drv :targets '(a) :ops '(mark))))
      (should-error (hitl-plan-execute plan))
      (clrhash hitl-plan--store)
      (should (eq 'paused (hitl-plan-status (hitl-plan-load (hitl-plan-id plan))))))))

;;; Load validation

(defun hitl-plan-test--write (id form)
  (with-temp-file (hitl-plan-file id) (prin1 form (current-buffer))))

(ert-deftest hitl-plan-test-bad-op-index-is-corrupt ()
  (hitl-plan-test--with-env
    (let ((ops '((:name a) (:name b))))
      (dolist (case '(("noidx" (:id 1 :target (:x 1)))
                      ("oob" (:id 1 :op-index 9 :target (:x 1)))
                      ("neg" (:id 1 :op-index -1 :target (:x 1)))
                      ("nilidx" (:id 1 :op-index nil :target (:x 1)))))
        (hitl-plan-test--write (car case)
                               `(:version 1 :id ,(car case) :ops ,ops :steps (,(cadr case))))
        (should-not (hitl-plan-load (car case))))
      (hitl-plan-test--write "ok" `(:version 1 :id "ok" :ops ,ops
                                    :steps ((:id 1 :op-index 1 :target (:x 1)))))
      (should (hitl-plan-load "ok")))))

(ert-deftest hitl-plan-test-malformed-lists-are-corrupt ()
  (hitl-plan-test--with-env
    (hitl-plan-test--write "s1" '(:version 1 :id "s1" :ops ((:name a)) :steps 5))
    (hitl-plan-test--write "s2" '(:version 1 :id "s2" :ops (5) :steps nil))
    (hitl-plan-test--write "s3" '(:version 1 :id "s3" :ops ((:name a)) :steps (7)))
    (hitl-plan-test--write "s4" '(:version 1 :id "s4" :ops ((:name a)) :steps ((:id 1 :op-index 0) . 3)))
    (dolist (id '("s1" "s2" "s3" "s4"))
      (should-not (hitl-plan-load id)))))

(ert-deftest hitl-plan-test-to-plist-rejects-orphan-step ()
  (hitl-plan-test--with-env
    (let ((plan (hitl-plan-create :targets '((:file "/x")) :ops '(a))))
      (setf (hitl-plan-step-op (car (hitl-plan-steps plan)))
            (hitl-plan-op--make :name 'stray))
      (should-error (hitl-plan-to-plist plan)))))

;;; Summary counts

(ert-deftest hitl-plan-test-summary-counts-other-statuses ()
  (hitl-plan-test--with-env
    (let ((plan (hitl-plan-create :targets '((:file "/x") (:file "/y") (:file "/z"))
                                  :ops '(a))))
      (setf (hitl-plan-step-status (nth 0 (hitl-plan-steps plan))) 'weird)
      (setf (hitl-plan-step-status (nth 1 (hitl-plan-steps plan))) 'weird)
      (setf (hitl-plan-step-status (nth 2 (hitl-plan-steps plan))) 'done)
      (let ((counts (plist-get (hitl-plan-summary plan) :counts)))
        (should (= 3 (plist-get counts :total)))
        (should (= 1 (plist-get counts :done)))
        (should (= 0 (plist-get counts :pending)))
        (should (equal '(:weird 2) (plist-get counts :other)))))))

(ert-deftest hitl-plan-test-file-target-non-string-snippet ()
  (hitl-plan-test--with-env
    (let ((f (hitl-plan-test--file "alpha\nbeta\n")))
      (unwind-protect
          (progn
            (should (markerp (hitl-plan-file-target-resolve (list :file f :line 2 :snippet :null))))
            (should (markerp (hitl-plan-file-target-resolve (list :file f :line 2 :snippet 5)))))
        (when-let* ((b (get-file-buffer f))) (kill-buffer b))
        (delete-file f)))))

;;; Hooks

(ert-deftest hitl-plan-test-step-functions-hook ()
  (hitl-plan-test--with-env
    (hitl-plan-test--register-upcase)
    (hitl-plan-register-op 'file-target 'boom :apply (lambda () (error "no")))
    (let* ((f (hitl-plan-test--file "alpha\nbeta\n"))
           (plan (hitl-plan-create :targets (hitl-plan-test--targets f "alpha" "beta")
                                   :ops '(mark boom)))
           seen)
      (unwind-protect
          (progn
            (add-hook 'hitl-plan-step-functions
                      (lambda (step p)
                        (push (list (hitl-plan-step-id step) (eq p plan)
                                    (hitl-plan-step-status step)
                                    (buffer-substring (line-beginning-position)
                                                      (line-end-position)))
                              seen)))
            (hitl-plan-execute plan)
            ;; Only successful steps, with point on the target line.
            (should (equal '((1 t done "alpha") (2 t done "beta")) (reverse seen))))
        (delete-file f)))))

(ert-deftest hitl-plan-test-after-apply-hook ()
  (hitl-plan-test--with-env
    (hitl-plan-test--register-upcase)
    (let* ((f1 (hitl-plan-test--file "alpha\nbeta\n"))
           (f2 (hitl-plan-test--file "gamma\n"))
           (plan (hitl-plan-create
                  :targets (append (hitl-plan-test--targets f1 "alpha" "beta")
                                   (hitl-plan-test--targets f2 "gamma"))
                  :ops '(mark)))
           calls)
      (unwind-protect
          (progn
            (add-hook 'hitl-plan-after-apply-functions
                      (lambda (file steps p)
                        (push (list file (length steps) (eq p plan)
                                    (equal (buffer-file-name) file))
                              calls)))
            (hitl-plan-execute plan)
            (should (equal (sort (list (list f1 2 t t) (list f2 1 t t))
                                 (lambda (a b) (string< (car a) (car b))))
                           (sort (copy-sequence calls)
                                 (lambda (a b) (string< (car a) (car b))))))
            (should (= 2 (length calls))))
        (dolist (f (list f1 f2))
          (when-let* ((b (get-file-buffer f))) (kill-buffer b))
          (delete-file f))))))

(ert-deftest hitl-plan-test-save-buffers ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'file-target 'edit
      :apply (lambda () (end-of-line) (insert "!")))
    (let* ((f (hitl-plan-test--file "alpha\n"))
           (plan (hitl-plan-create :targets (hitl-plan-test--targets f "alpha") :ops '(edit)))
           (hitl-plan-save-buffers t))
      (unwind-protect
          (progn
            (should-not hitl-plan-after-apply-functions)
            (hitl-plan-execute plan)
            (should (equal "alpha!\n" (with-temp-buffer (insert-file-contents f) (buffer-string)))))
        (when-let* ((b (get-file-buffer f))) (kill-buffer b))
        (delete-file f)))))

(ert-deftest hitl-plan-test-save-buffers-nil-leaves-modified ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'file-target 'edit
      :apply (lambda () (end-of-line) (insert "!")))
    (let* ((f (hitl-plan-test--file "alpha\n"))
           (plan (hitl-plan-create :targets (hitl-plan-test--targets f "alpha") :ops '(edit)))
           (hitl-plan-save-buffers nil))
      (unwind-protect
          (progn
            (hitl-plan-execute plan)
            (should (equal "alpha\n" (with-temp-buffer (insert-file-contents f) (buffer-string)))))
        (when-let* ((b (get-file-buffer f))) (set-buffer-modified-p nil) (kill-buffer b))
        (delete-file f)))))

(ert-deftest hitl-plan-test-create-id ()
  (hitl-plan-test--with-env
    (let ((plan (hitl-plan-create :id "my-id" :targets '((:file "/x")) :ops '(noop))))
      (should (equal "my-id" (hitl-plan-id plan)))
      (should (file-exists-p (hitl-plan-file "my-id"))))))

(ert-deftest hitl-plan-test-public-plist ()
  (hitl-plan-test--with-env
    (let* ((plan (hitl-plan-create :id "p1" :targets '((:file "/x")) :ops '((noop 1))))
           (copy (hitl-plan-from-plist (hitl-plan-to-plist plan))))
      (should (equal "p1" (hitl-plan-id copy)))
      (should (= 1 (length (hitl-plan-steps copy)))))))

(ert-deftest hitl-plan-test-unresolved-status ()
  (hitl-plan-test--with-env
    (hitl-plan-register-domain 'dflt :resolve-target (lambda (_) nil))
    (hitl-plan-register-domain 'miss :resolve-target (lambda (_) nil)
      :unresolved-status 'missing
      :unresolved-message (lambda (_p s) (format "gone %S" (hitl-plan-step-target s))))
    (hitl-plan-register-op t 'noop :apply #'ignore)
    (let ((p1 (hitl-plan-create :domain 'dflt :targets '(a) :ops '(noop)))
          (p2 (hitl-plan-create :domain 'miss :targets '(a) :ops '(noop))))
      (hitl-plan-execute p1)
      (hitl-plan-execute p2)
      (let ((s1 (car (hitl-plan-steps p1))) (s2 (car (hitl-plan-steps p2))))
        (should (eq 'failed (hitl-plan-step-status s1)))
        (should (equal "target no longer resolves" (hitl-plan-step-message s1)))
        (should (eq 'missing (hitl-plan-step-status s2)))
        (should (equal "gone a" (hitl-plan-step-message s2))))
      (should (eq 'complete (hitl-plan-status p2)))
      (should (= 1 (plist-get (plist-get (hitl-plan-summary p2) :counts) :missing)))
      (should (= 0 (plist-get (plist-get (hitl-plan-summary p2) :counts) :failed))))))

(ert-deftest hitl-plan-test-per-call-directory ()
  (hitl-plan-test--with-env
    (hitl-plan-register-domain 'other)
    (let* ((d2 (make-temp-file "hitl-plan-test2" t))
           (p1 (hitl-plan-create :id "a1" :targets '(x) :ops '(noop)))
           (p2 (hitl-plan-create :id "b1" :domain 'other :targets '(x) :ops '(noop)
                                 :directory d2)))
      (unwind-protect
          (progn
            (should (equal '("a1") (mapcar #'hitl-plan-id (hitl-plan-list nil hitl-plan-directory))))
            (should (equal '("b1") (mapcar #'hitl-plan-id (hitl-plan-list nil d2))))
            (should (equal '("b1") (mapcar #'hitl-plan-id (hitl-plan-list nil nil 'other))))
            (should (hitl-plan-get "b1" d2))
            (should-not (hitl-plan-get "a1" d2))
            (should-not (hitl-plan-get "b1" hitl-plan-directory))
            ;; cold cache: loaded from the given directory
            (clrhash hitl-plan--store)
            (should-not (hitl-plan-get "b1"))
            (should (hitl-plan-get "b1" d2))
            (should (hitl-plan-load "b1" d2))
            (clrhash hitl-plan--store)
            (should (equal '("b1") (mapcar #'hitl-plan-id (hitl-plan-list nil d2 'other)))))
        (delete-directory d2 t)))))

(ert-deftest hitl-plan-test-domain-p ()
  (hitl-plan-test--with-env
    (let ((plan (hitl-plan-create :targets '(x) :ops '(noop))))
      (should (hitl-plan-in-domain-p plan 'file-target))
      (should (hitl-plan-in-domain-p plan "file-target"))
      (should-not (hitl-plan-in-domain-p plan 'other)))))

(ert-deftest hitl-plan-test-op-extra ()
  (hitl-plan-test--with-env
    (let ((spec (hitl-plan-register-op 'file-target 'ex :apply #'ignore :extra '(:a 1))))
      (should (equal '(:a 1) (hitl-plan-op-spec-extra spec)))
      (should (equal '(:a 1) (hitl-plan-op-spec-extra (hitl-plan-get-op 'file-target 'ex)))))))

(ert-deftest hitl-plan-test-accept-buffer-slot ()
  (hitl-plan-test--with-env
    (let ((d (hitl-plan-register-domain 'acc :accept-buffer #'bufferp)))
      (should (funcall (hitl-plan-domain-accept-buffer d) (current-buffer))))))


;;; Domain directory, migration and after-steps-added

(ert-deftest hitl-plan-test-domain-directory-default ()
  (hitl-plan-test--with-env
    (let ((other (make-temp-file "hitl-plan-dom" t)))
      (unwind-protect
          (progn
            (hitl-plan-register-domain 'dd :resolve-target #'identity
                                       :directory (lambda () other))
            (hitl-plan-register-op 'dd 'o :apply #'ignore)
            (let* ((plan (hitl-plan-create :domain 'dd :targets '(a) :ops '(o)))
                   (id (hitl-plan-id plan))
                   (explicit (hitl-plan-create :domain 'dd :targets '(a) :ops '(o)
                                               :directory dir)))
              (should (equal other (hitl-plan-directory plan)))
              (should (file-exists-p (hitl-plan-file id other)))
              (should-not (file-exists-p (hitl-plan-file id dir)))
              (should (equal dir (hitl-plan-directory explicit)))
              (clrhash hitl-plan--store)
              ;; Domain given, no directory: the domain store is used.
              (should (hitl-plan-get id nil 'dd))
              (clrhash hitl-plan--store)
              (should (equal (list id)
                             (mapcar #'hitl-plan-id (hitl-plan-list nil nil 'dd))))))
        (delete-directory other t)))))

(ert-deftest hitl-plan-test-domain-store-string ()
  (hitl-plan-test--with-env
    (hitl-plan-register-domain 'ds :directory "/tmp/x/")
    (should (equal "/tmp/x/" (hitl-plan-domain-store 'ds)))
    (should-not (hitl-plan-domain-store 'file-target))
    (should-not (hitl-plan-domain-store nil))))

(ert-deftest hitl-plan-test-after-steps-added-create ()
  (hitl-plan-test--with-env
    (let (calls)
      (hitl-plan-register-domain
       'ac :resolve-target #'identity
       :after-steps-added
       (lambda (plan steps)
         (push (length steps) calls)
         (should-not (file-exists-p (hitl-plan-file (hitl-plan-id plan))))
         (should (equal steps (hitl-plan-steps plan)))
         (setf (hitl-plan-step-status (car steps)) 'skipped)))
      (hitl-plan-register-op 'ac 'o :apply #'ignore)
      (let* ((plan (hitl-plan-create :domain 'ac :targets '(a b) :ops '(o)))
             (loaded (progn (clrhash hitl-plan--store)
                            (hitl-plan-load (hitl-plan-id plan)))))
        (should (equal '(2) calls))
        (should (equal '(skipped pending)
                       (mapcar #'hitl-plan-step-status (hitl-plan-steps loaded))))))))

(ert-deftest hitl-plan-test-after-steps-added-create-no-steps ()
  (hitl-plan-test--with-env
    (let (calls)
      (hitl-plan-register-domain 'ac :resolve-target #'identity
                                 :after-steps-added (lambda (_p s) (push s calls)))
      (hitl-plan-register-op 'ac 'o :apply #'ignore)
      (hitl-plan-create :domain 'ac :targets nil :ops '(o))
      (should-not calls))))

(ert-deftest hitl-plan-test-after-steps-added-add-op ()
  (hitl-plan-test--with-env
    (let (calls)
      (hitl-plan-register-domain
       'ac :resolve-target #'identity
       :after-steps-added
       (lambda (plan steps)
         (push (mapcar #'hitl-plan-step-id steps) calls)
         (should (< (length steps) (1+ (length (hitl-plan-steps plan)))))
         (dolist (s steps)
           (when (eq (hitl-plan-op-name (hitl-plan-step-op s)) 'p)
             (setf (hitl-plan-step-status s) 'skipped)))))
      (hitl-plan-register-op 'ac 'o :apply #'ignore)
      (hitl-plan-register-op 'ac 'p :apply #'ignore)
      (let ((plan (hitl-plan-create :domain 'ac :targets '(a b) :ops '(o))))
        (setq calls nil)
        (hitl-plan-add-op plan '(p))
        (should (equal '((3 4)) calls)) ; only the new steps
        (should (equal '(pending pending skipped skipped)
                       (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
        ;; Saved after the hook ran.
        (clrhash hitl-plan--store)
        (should (equal '(skipped skipped)
                       (mapcar #'hitl-plan-step-status
                               (cddr (hitl-plan-steps
                                      (hitl-plan-load (hitl-plan-id plan)))))))
        ;; No targets: hook not called.
        (setq calls nil)
        (hitl-plan-add-op (hitl-plan-get (hitl-plan-id plan)) '(o) nil)
        (should (= 1 (length calls)))
        (setq calls nil)
        (let ((empty (hitl-plan-create :domain 'ac :targets nil :ops '(o))))
          (hitl-plan-add-op empty '(p))
          (should-not calls))))))

(ert-deftest hitl-plan-test-after-steps-added-add-op-reopen ()
  (hitl-plan-test--with-env
    (hitl-plan-register-domain
     'ac :resolve-target #'identity
     :after-steps-added
     (lambda (_p steps)
       (dolist (s steps)
         (when (eq (hitl-plan-op-name (hitl-plan-step-op s)) 'p)
           (setf (hitl-plan-step-status s) 'skipped)))))
    (hitl-plan-register-op 'ac 'o :apply #'ignore)
    (hitl-plan-register-op 'ac 'p :apply #'ignore)
    (let ((plan (hitl-plan-create :domain 'ac :targets '(a) :ops '(o))))
      (setf (hitl-plan-step-status (car (hitl-plan-steps plan))) 'done
            (hitl-plan-status plan) 'complete)
      ;; Everything new is skipped: stays complete.
      (hitl-plan-add-op plan '(p))
      (should (eq 'complete (hitl-plan-status plan)))
      ;; Pending new step reopens.
      (hitl-plan-add-op plan '(o))
      (should (eq 'paused (hitl-plan-status plan))))))

(ert-deftest hitl-plan-test-after-steps-added-other-domains-unaffected ()
  (hitl-plan-test--with-env
    (let (calls)
      (hitl-plan-register-domain 'ac :resolve-target #'identity
                                 :after-steps-added (lambda (_p s) (push s calls)))
      (hitl-plan-register-domain 'plain :resolve-target #'identity)
      (hitl-plan-register-op 'plain 'o :apply #'ignore)
      (hitl-plan-register-op 'file-target 'o :apply #'ignore)
      (dolist (d '(plain file-target))
        (let ((plan (hitl-plan-create :domain d :targets '(a) :ops '(o))))
          (hitl-plan-add-op plan '(o))
          (should (equal '(pending pending)
                         (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))))
      (should-not calls))))

(ert-deftest hitl-plan-test-migrate-plist ()
  (hitl-plan-test--with-env
    (hitl-plan-register-domain
     'mg :resolve-target #'identity
     :migrate-plist (lambda (data) (plist-put (copy-sequence data) :description "migrated")))
    (let ((data (list :version hitl-plan-format-version :id "old-1"
                      :ops nil :steps nil)))
      ;; Domain named in the data.
      (should (equal "migrated"
                     (hitl-plan-description
                      (hitl-plan-from-plist (plist-put (copy-sequence data) :domain 'mg)))))
      ;; No domain, no hint: not migrated, defaults to file-target.
      (let ((plan (hitl-plan-from-plist data)))
        (should-not (hitl-plan-description plan))
        (should (eq 'file-target (hitl-plan-domain plan))))
      ;; Explicit legacy hint.  The migrator is expected to supply :domain
      ;; itself when it wants one; otherwise file-target applies.
      (should (equal "migrated"
                     (hitl-plan-description (hitl-plan-from-plist data 'mg)))))))

(defun hitl-plan-test--write-legacy (file id)
  "Write a legacy plan (no :domain) to FILE with ID."
  (with-temp-file file
    (prin1 (list :version hitl-plan-format-version :id id :ops nil :steps nil)
           (current-buffer))))

(ert-deftest hitl-plan-test-load-legacy-domain ()
  (hitl-plan-test--with-env
    (let ((store (make-temp-file "hitl-plan-store" t)))
      (unwind-protect
          (progn
            (hitl-plan-register-domain
             'mg :resolve-target #'identity :directory store
             :migrate-plist (lambda (data) (plist-put (copy-sequence data) :domain 'mg)))
            (let ((file (expand-file-name "legacy-1.eld" dir))
                  (sfile (expand-file-name "legacy-2.eld" store)))
              (hitl-plan-test--write-legacy file "legacy-1")
              (hitl-plan-test--write-legacy sfile "legacy-2")
              ;; No matching store: file-target.
              (should (eq 'file-target (hitl-plan-domain (hitl-plan-load file))))
              (clrhash hitl-plan--store)
              ;; Explicit argument overrides.
              (should (eq 'mg (hitl-plan-domain (hitl-plan-load file nil 'mg))))
              (clrhash hitl-plan--store)
              ;; Derived from the store; scans and get derive too.
              (should (eq 'mg (hitl-plan-domain (hitl-plan-load sfile))))
              (clrhash hitl-plan--store)
              (should (eq 'mg (hitl-plan-domain (hitl-plan-get "legacy-2" nil 'mg))))
              (clrhash hitl-plan--store)
              (should (equal '(mg) (mapcar #'hitl-plan-domain
                                           (hitl-plan-list nil store))))
              ;; Explicit override wins over the store.
              (hitl-plan-register-domain 'other :resolve-target #'identity)
              (clrhash hitl-plan--store)
              (should (eq 'other (hitl-plan-domain (hitl-plan-load sfile nil 'other))))))
        (delete-directory store t)))))

(ert-deftest hitl-plan-test-load-legacy-domain-stores ()
  (hitl-plan-test--with-env
    (let ((a (make-temp-file "hitl-plan-a" t))
          (b (make-temp-file "hitl-plan-b" t)))
      (unwind-protect
          (progn
            ;; A with trailing slash and a function; B without trailing slash.
            (hitl-plan-register-domain 'da :resolve-target #'identity
                                       :directory (lambda () (file-name-as-directory a)))
            (hitl-plan-register-domain 'db :resolve-target #'identity
                                       :directory (directory-file-name b))
            (hitl-plan-register-domain 'dn :resolve-target #'identity)
            (let ((fa (expand-file-name "x.eld" a))
                  (fb (expand-file-name "y.eld" b)))
              (hitl-plan-test--write-legacy fa "x")
              (hitl-plan-test--write-legacy fb "y")
              (should (eq 'da (hitl-plan-domain (hitl-plan-load fa))))
              (should (eq 'db (hitl-plan-domain (hitl-plan-load fb))))
              (should (eq 'da (hitl-plan--domain-for-directory (directory-file-name a))))
              (should (eq 'db (hitl-plan--domain-for-directory
                               (concat (directory-file-name b) "/./"))))
              (should-not (hitl-plan--domain-for-directory dir))))
        (delete-directory a t)
        (delete-directory b t)))))

;;; Review follow-ups

(ert-deftest hitl-plan-test-rebuild-batch-quit-pauses-and-resignals ()
  "A quit inside the domain's :rebuild-batch pauses and saves the plan."
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply #'ignore)
    (hitl-plan-register-domain 'drv
      :resolve-target #'identity
      :rebuild-batch (lambda (&rest _) (signal 'quit nil))
      :driver (lambda (&rest _) nil))
    (let ((plan (hitl-plan-create :domain 'drv :targets '(a) :ops '(mark)))
          (quit-flag nil))
      (should (eq :quit (condition-case nil
                            (progn (hitl-plan-execute plan) :no-quit)
                          (quit :quit))))
      (should (eq 'paused (hitl-plan-status plan)))
      (clrhash hitl-plan--store)
      (should (eq 'paused (hitl-plan-status (hitl-plan-load (hitl-plan-id plan))))))))

(ert-deftest hitl-plan-test-after-apply-skipped-on-driver-error ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply #'ignore)
    (hitl-plan-register-domain 'drv
      :resolve-target #'identity
      :driver (lambda (plan op _pending resolved)
                (hitl-plan-run-step plan (caar resolved) op (cdar resolved))
                (error "driver boom")))
    (let ((plan (hitl-plan-create :domain 'drv :targets '(a b) :ops '(mark)))
          (hitl-plan-save-buffers t)
          calls)
      (add-hook 'hitl-plan-after-apply-functions (lambda (&rest a) (push a calls)))
      (should-error (hitl-plan-execute plan))
      (should-not calls)
      (should (eq 'paused (hitl-plan-status plan))))))

(ert-deftest hitl-plan-test-run-step-unknown-op-does-not-resolve ()
  "An unregistered operation fails the step without resolving its target."
  (hitl-plan-test--with-env
    (let (resolved)
      (hitl-plan-register-domain 'drv
        :resolve-target (lambda (tgt) (push tgt resolved) tgt))
      (let* ((plan (hitl-plan-create :domain 'drv :targets '(a) :ops '(nope)))
             (step (car (hitl-plan-steps plan))))
        (should (eq 'failed (hitl-plan-run-step plan step (hitl-plan-step-op step))))
        (should-not resolved)
        (should (string-match-p "Unknown operation: nope" (hitl-plan-step-message step)))))))

(ert-deftest hitl-plan-test-queueable-spec ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'file-target 'safe :queueable t :apply #'ignore)
    (hitl-plan-register-op 'file-target 'unsafe :apply #'ignore)
    (hitl-plan-register-op t 'anywhere :queueable t :apply #'ignore)
    (should (eq 'safe (hitl-plan-op-spec-name (hitl-plan-queueable-spec 'file-target 'safe))))
    ;; Domain-t fallback.
    (should (hitl-plan-queueable-spec 'file-target 'anywhere))
    (let ((err (should-error (hitl-plan-queueable-spec 'file-target 'unsafe)
                             :type 'user-error)))
      (should (string-match-p "not queueable" (error-message-string err))))
    (let ((err (should-error (hitl-plan-queueable-spec 'file-target 'missing)
                             :type 'user-error)))
      (should (string-match-p "Unknown operation" (error-message-string err))))))

(ert-deftest hitl-plan-test-check-unattended ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'file-target 'safe :queueable t :apply #'ignore)
    (hitl-plan-register-op 'file-target 'unsafe :apply #'ignore)
    (let ((mk (lambda (ops notes)
                (hitl-plan-create :targets '((:file "/x")) :ops ops :log-notes notes))))
      (should-not (hitl-plan-check-unattended (funcall mk '(safe) 'ignore)))
      (should-not (hitl-plan-check-unattended (funcall mk '(safe) 'one)))
      (should-error (hitl-plan-check-unattended (funcall mk '(safe) 'each))
                    :type 'user-error)
      (should-error (hitl-plan-check-unattended (funcall mk '(safe unsafe) 'ignore))
                    :type 'user-error)
      (should-error (hitl-plan-check-unattended (funcall mk '(missing) 'ignore))
                    :type 'user-error))))

(ert-deftest hitl-plan-test-file-target-capture ()
  (let ((f (hitl-plan-test--file "alpha\n  beta  \n\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect f)
          (goto-char (point-min))
          (forward-line 1)
          (should (equal (list :file f :line 2 :pos 7 :snippet "beta")
                         (hitl-plan-file-target-capture)))
          ;; A blank line has no snippet.
          (forward-line 1)
          (let ((tgt (hitl-plan-file-target-capture)))
            (should (= 3 (plist-get tgt :line)))
            (should-not (plist-get tgt :snippet))))
      (when-let* ((b (get-file-buffer f))) (kill-buffer b))
      (delete-file f))))

(ert-deftest hitl-plan-test-file-target-visit ()
  (let ((f (hitl-plan-test--file "one\ntwo\nthree\n")))
    (unwind-protect
        (save-window-excursion
          (hitl-plan-file-target-visit (list :file f :line 3))
          (should (equal f (buffer-file-name)))
          (should (= 3 (line-number-at-pos)))
          (hitl-plan-file-target-visit (list :file f :snippet "two") t)
          (should (= 2 (line-number-at-pos)))
          ;; Unresolvable target: nil, no window changes.
          (should-not (hitl-plan-file-target-visit (list :file f :snippet "zzz"))))
      (when-let* ((b (get-file-buffer f))) (kill-buffer b))
      (delete-file f))))

(ert-deftest hitl-plan-test-rebuild-batch-default-path ()
  "Without :rebuild-batch each pending step resolves on its own, buffer nil."
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply #'ignore)
    (hitl-plan-register-domain 'drv
      :resolve-target (lambda (tgt) (and (not (eq tgt 'gone)) (list 'ctx tgt))))
    (let* ((plan (hitl-plan-create :domain 'drv :targets '(a gone b) :ops '(mark)))
           (steps (hitl-plan-steps plan)))
      (setf (hitl-plan-step-status (nth 2 steps)) 'done)
      (let ((batch (hitl-plan-rebuild-batch plan (car (hitl-plan-ops plan)))))
        (should-not (car batch))
        (should (equal (list (cons (nth 0 steps) '(ctx a))) (cdr batch)))
        ;; The unresolvable pending step is marked; the done one is untouched.
        (should (eq 'failed (hitl-plan-step-status (nth 1 steps))))
        (should (equal "target no longer resolves" (hitl-plan-step-message (nth 1 steps))))
        (should (eq 'done (hitl-plan-step-status (nth 2 steps))))))))

(ert-deftest hitl-plan-test-after-apply-fileless-targets ()
  "Targets without :file are reported once with a nil FILE and no buffer."
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply #'ignore)
    (hitl-plan-register-domain 'drv :resolve-target #'identity)
    (let ((plan (hitl-plan-create :domain 'drv :targets '(a b) :ops '(mark)))
          (hitl-plan-save-buffers t)
          calls)
      (add-hook 'hitl-plan-after-apply-functions
                (lambda (file steps _plan) (push (list file (length steps)) calls)))
      (hitl-plan-execute plan)
      (should (equal '((nil 2)) calls))
      (should (eq 'complete (hitl-plan-status plan))))))

(ert-deftest hitl-plan-test-after-apply-hook-error-is-reported ()
  "A failing after-apply hook is logged and does not abort the execution."
  (hitl-plan-test--with-env
    (hitl-plan-test--register-upcase)
    (let* ((f (hitl-plan-test--file "alpha\nbeta\n"))
           (plan (hitl-plan-create :targets (hitl-plan-test--targets f "alpha" "beta")
                                   :ops '(mark)))
           (msgs nil))
      (unwind-protect
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
            (add-hook 'hitl-plan-after-apply-functions
                      (lambda (&rest _) (error "hook kaboom")))
            (hitl-plan-execute plan)
            (should (eq 'complete (hitl-plan-status plan)))
            (should (seq-some (lambda (m) (string-match-p "after-apply hook failed.*hook kaboom" m))
                              msgs)))
        (when-let* ((b (get-file-buffer f))) (kill-buffer b))
        (delete-file f)))))

(ert-deftest hitl-plan-test-other-counts ()
  (hitl-plan-test--with-env
    (let ((plan (hitl-plan-create :targets '(a b c d e) :ops '(noop))))
      (cl-mapc (lambda (s st) (setf (hitl-plan-step-status s) st))
               (hitl-plan-steps plan) '(pending zeta alpha zeta done))
      (should (equal '((alpha . 1) (zeta . 2)) (hitl-plan-other-counts plan)))
      (dolist (s (hitl-plan-steps plan)) (setf (hitl-plan-step-status s) 'done))
      (should-not (hitl-plan-other-counts plan)))))

(ert-deftest hitl-plan-test-load-shared-store-first-registered-wins ()
  "Two domains sharing a store: legacy files belong to the first registered."
  (hitl-plan-test--with-env
    (let ((store (make-temp-file "hitl-plan-shared" t)))
      (unwind-protect
          (progn
            (hitl-plan-register-domain 'first :resolve-target #'identity :directory store)
            (hitl-plan-register-domain 'second :resolve-target #'identity :directory store)
            (should (eq 'first (hitl-plan--domain-for-directory store)))
            (let ((file (expand-file-name "legacy.eld" store)))
              (hitl-plan-test--write-legacy file "legacy")
              (should (eq 'first (hitl-plan-domain (hitl-plan-load file))))))
        (delete-directory store t)))))

(ert-deftest hitl-plan-test-id-across-directories ()
  "The cache is keyed by ID; lookups still respect the requested directory."
  (hitl-plan-test--with-env
    (let ((d2 (make-temp-file "hitl-plan-test2" t)))
      (unwind-protect
          (progn
            (hitl-plan-create :id "dup" :description "one" :targets '(x) :ops '(noop))
            ;; An ID already cached is refused, even for another directory.
            (should-error (hitl-plan-create :id "dup" :targets '(x) :ops '(noop)
                                            :directory d2)
                          :type 'user-error)
            (clrhash hitl-plan--store)
            ;; Saved in the target directory: refused there, fine elsewhere.
            (should-error (hitl-plan-create :id "dup" :targets '(x) :ops '(noop))
                          :type 'user-error)
            (clrhash hitl-plan--store)
            (let ((other (hitl-plan-create :id "dup" :description "two" :targets '(x)
                                           :ops '(noop) :directory d2)))
              (should (equal "two" (hitl-plan-description other))))
            ;; Each directory yields its own plan; the cache follows the last load.
            (should (equal "one" (hitl-plan-description (hitl-plan-get "dup" hitl-plan-directory))))
            (should (equal "two" (hitl-plan-description (hitl-plan-get "dup" d2))))
            (should (equal "one" (hitl-plan-description (hitl-plan-get "dup" hitl-plan-directory)))))
        (delete-directory d2 t)))))

(ert-deftest hitl-plan-test-generated-id-avoids-target-directory-files ()
  (hitl-plan-test--with-env
    (let ((d2 (make-temp-file "hitl-plan-test2" t)))
      (unwind-protect
          (cl-letf (((symbol-function 'random) (lambda (&rest _) 1)))
            (let ((p1 (hitl-plan-create :targets '(x) :ops '(noop) :directory d2)))
              (clrhash hitl-plan--store)
              (let ((p2 (hitl-plan-create :targets '(x) :ops '(noop) :directory d2)))
                (should-not (equal (hitl-plan-id p1) (hitl-plan-id p2))))))
        (delete-directory d2 t)))))

;;; Coverage-driven additions

(ert-deftest hitl-plan-test-registration-rejects-bad-names ()
  (hitl-plan-test--with-env
    (should-error (hitl-plan-register-domain nil) :type 'user-error)
    (should-error (hitl-plan-register-domain "str") :type 'user-error)
    (should-error (hitl-plan-register-op "d" 'x) :type 'user-error)
    (should-error (hitl-plan-register-op 'file-target nil) :type 'user-error)
    (should-error (hitl-plan-register-op 'file-target "x") :type 'user-error)
    (should-error (hitl-plan-create :domain 'nope :targets '(x) :ops '(noop))
                  :type 'user-error)))

(ert-deftest hitl-plan-test-normalize-op-forms ()
  (hitl-plan-test--with-env
    (let ((struct (hitl-plan-op--make :name 'a :args '(1))))
      (should (eq struct (hitl-plan--normalize-op struct))))
    (let ((op (hitl-plan--normalize-op '(:name b :args (2 3)))))
      (should (eq 'b (hitl-plan-op-name op)))
      (should (equal '(2 3) (hitl-plan-op-args op))))
    (let ((op (hitl-plan--normalize-op '(c 4))))
      (should (eq 'c (hitl-plan-op-name op)))
      (should (equal '(4) (hitl-plan-op-args op))))
    (should (eq 'd (hitl-plan-op-name (hitl-plan--normalize-op 'd))))
    (should-error (hitl-plan--normalize-op nil))
    (should-error (hitl-plan--normalize-op 42))))

(ert-deftest hitl-plan-test-describe-op-fallbacks ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'file-target 'good :apply #'ignore
                           :describe (lambda (x) (format "good<%s>" x)))
    (hitl-plan-register-op 'file-target 'broken :apply #'ignore
                           :describe (lambda (_x) (error "no")))
    (hitl-plan-register-op 'file-target 'plain :apply #'ignore)
    (let ((mk (lambda (name &rest args) (hitl-plan-op--make :name name :args args))))
      (should (equal "good<1>" (hitl-plan-describe-op 'file-target (funcall mk 'good 1))))
      (should (equal "broken (1)" (hitl-plan-describe-op 'file-target (funcall mk 'broken 1))))
      (should (equal "plain \"a\" \"b\"" (hitl-plan-describe-op 'file-target (funcall mk 'plain "a" "b"))))
      (should (equal "plain" (hitl-plan-describe-op 'file-target 'plain)))
      (should (equal "unregistered 1" (hitl-plan-describe-op 'file-target
                                                             (funcall mk 'unregistered 1)))))))

(ert-deftest hitl-plan-test-file-target-resolve-by-pos-and-line-hint ()
  (let ((f (hitl-plan-test--file "one\ntwo\nthree\ntwo\n")))
    (unwind-protect
        (progn
          ;; :pos alone lands on the start of its line.
          (let ((m (hitl-plan-file-target-resolve (list :file f :pos 10))))
            (should (= 9 (marker-position m))))
          ;; A snippet with only :line as the hint picks the closer duplicate.
          (let ((m (hitl-plan-file-target-resolve (list :file f :snippet "two" :line 4))))
            (should (= 15 (marker-position m))))
          (let ((m (hitl-plan-file-target-resolve (list :file f :snippet "two" :line 1))))
            (should (= 5 (marker-position m))))
          ;; Out of range :pos and :line do not resolve.
          (should-not (hitl-plan-file-target-resolve (list :file f :pos 9999)))
          (should-not (hitl-plan-file-target-resolve (list :file f :line 99)))
          (should-not (hitl-plan-file-target-resolve (list :file f))))
      (when-let* ((b (get-file-buffer f))) (kill-buffer b))
      (delete-file f))))

(ert-deftest hitl-plan-test-unresolved-message-forms ()
  (hitl-plan-test--with-env
    (hitl-plan-register-op t 'mark :apply #'ignore)
    (hitl-plan-register-domain 'drv :resolve-target (lambda (_) nil)
                               :unresolved-message "custom gone")
    (hitl-plan-register-domain 'drvf :resolve-target (lambda (_) nil)
                               :unresolved-message
                               (lambda (_plan step) (format "no %s" (hitl-plan-step-target step))))
    (let ((p1 (hitl-plan-create :domain 'drv :targets '(a) :ops '(mark)))
          (p2 (hitl-plan-create :domain 'drvf :targets '(b) :ops '(mark))))
      (hitl-plan-execute p1)
      (hitl-plan-execute p2)
      (should (equal "custom gone" (hitl-plan-step-message (car (hitl-plan-steps p1)))))
      (should (equal "no b" (hitl-plan-step-message (car (hitl-plan-steps p2))))))))

(ert-deftest hitl-plan-test-inject-note ()
  (hitl-plan-test--with-env
    (let (injected)
      (hitl-plan-register-op 'drv 'mark :apply #'ignore)
      (hitl-plan-register-domain 'drv
        :resolve-target #'identity
        :inject-note (lambda (text step _plan) (push (cons (hitl-plan-step-id step) text) injected)))
      ;; `one': the plan note; empty notes are not injected.
      (let ((plan (hitl-plan-create :domain 'drv :targets '(a b) :ops '(mark)
                                    :log-notes 'one :note "shared")))
        (hitl-plan-execute plan)
        (should (equal '((1 . "shared") (2 . "shared")) (reverse injected))))
      (setq injected nil)
      ;; `each': the step's note.
      (let ((plan (hitl-plan-create :domain 'drv :targets '(a b) :ops '(mark)
                                    :log-notes 'each)))
        (setf (hitl-plan-step-note (car (hitl-plan-steps plan))) "mine"
              (hitl-plan-step-note (cadr (hitl-plan-steps plan))) "")
        (hitl-plan-execute plan)
        (should (equal '((1 . "mine")) injected)))
      (setq injected nil)
      ;; `ignore': nothing.
      (hitl-plan-execute (hitl-plan-create :domain 'drv :targets '(a) :ops '(mark)
                                           :log-notes 'ignore :note "x"))
      (should-not injected))))

(ert-deftest hitl-plan-test-step-hook-error-recorded ()
  "A failing step hook keeps the step `done' and records the hook error."
  (hitl-plan-test--with-env
    (hitl-plan-register-op 'drv 'mark :apply #'ignore)
    (hitl-plan-register-domain 'drv :resolve-target #'identity)
    (add-hook 'hitl-plan-step-functions (lambda (&rest _) (error "hook broke")))
    (let ((plan (hitl-plan-create :domain 'drv :targets '(a) :ops '(mark))))
      (hitl-plan-execute plan)
      (let ((step (car (hitl-plan-steps plan))))
        (should (eq 'done (hitl-plan-step-status step)))
        (should (equal "step hook error: hook broke" (hitl-plan-step-message step)))))))

(ert-deftest hitl-plan-test-execute-rejects-finished-plans ()
  (hitl-plan-test--with-env
    (dolist (st '(complete discarded))
      (let ((plan (hitl-plan-create :targets '(x) :ops '(noop) :status st)))
        (should-error (hitl-plan-execute plan) :type 'user-error)))))

(provide 'hitl-plan-test)
;;; hitl-plan-test.el ends here
