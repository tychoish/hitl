;;; hitl-plan-ui-test.el --- Tests for hitl-plan-ui -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -L /home/tychoish/src/hitl -L /home/tychoish/src/annotated-completing-read \
;;     -l ert -l /home/tychoish/src/hitl/test/hitl-plan-ui-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'hitl-plan-ui)

(defvar hitl-plan-ui-test--applied nil)
(defvar hitl-plan-ui-test--visited nil)

(defmacro hitl-plan-ui-test--with-env (&rest body)
  "Run BODY with an isolated plan directory, registries and a stub domain."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "hitl-plan-ui-test" t))
          (hitl-plan-directory dir)
          (hitl-plan--store (make-hash-table :test #'equal))
          (hitl-plan--op-registry (make-hash-table :test #'equal))
          (hitl-plan--domain-registry (make-hash-table :test #'eq))
          (hitl-plan-ui-test--applied nil)
          (hitl-plan-ui-test--visited nil)
          (hitl-plan-after-apply-functions nil))
     (hitl-plan-register-domain 'stub
       :resolve-target (lambda (tgt) (and (not (plist-get tgt :gone)) tgt))
       :format-target (lambda (tgt) (cons (plist-get tgt :file) (plist-get tgt :name)))
       :visit-target (lambda (tgt &optional _o)
                       (push tgt hitl-plan-ui-test--visited)
                       (not (plist-get tgt :gone))))
     (hitl-plan-register-op 'stub 'mark :key ?m :label "mark"
       :read-args (lambda () (list "arg"))
       :apply (lambda (&rest a) (push (cons 'mark a) hitl-plan-ui-test--applied)))
     (hitl-plan-register-op 'stub 'other :key ?o :label "other"
       :apply (lambda (&rest _) (push 'other hitl-plan-ui-test--applied)))
     (unwind-protect (progn ,@body)
       (dolist (b (buffer-list))
         (when (string-prefix-p "*hitl-plan: " (buffer-name b)) (kill-buffer b)))
       (delete-directory dir t))))

(defun hitl-plan-ui-test--plan (&optional status)
  (hitl-plan-create :domain 'stub :status (or status 'draft)
                    :ops '(mark)
                    :targets (list (list :file "a.org" :name "Alpha")
                                   (list :file "b.org" :name "Beta" :gone t)
                                   (list :file "c.org" :name "Gamma"))))

(defun hitl-plan-ui-test--goto-step (id)
  (goto-char (point-min))
  (while (not (eql id (let ((s (get-text-property (point) 'hitl-plan-step)))
                        (and s (hitl-plan-step-id s)))))
    (forward-line 1)))

(ert-deftest hitl-plan-ui-test-progress-counts-other-statuses ()
  (hitl-plan-ui-test--with-env
    (let ((plan (hitl-plan-ui-test--plan)))
      (setf (hitl-plan-step-status (car (hitl-plan-steps plan))) 'weird)
      (with-current-buffer (hitl-plan-review plan)
        (should (string-match-p "2 pending.*, 1 weird" (buffer-string)))))))

(ert-deftest hitl-plan-ui-test-render ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (buf (hitl-plan-review plan)))
      (should (equal (buffer-name buf) (format "*hitl-plan: %s*" (hitl-plan-id plan))))
      (with-current-buffer buf
        (should (derived-mode-p 'hitl-plan-mode))
        (let ((s (buffer-string)))
          (should (string-match-p "Progress: +0/3 done, 3 pending, 0 failed, 0 skipped" s))
          (should (string-match-p "Alpha" s))
          (should (string-match-p "\\[pending\\]" s)))
        (should (eq 'hitl-plan-pending-face
                    (get-text-property
                     (+ 4 (line-beginning-position)) 'face)))))
    (should (face-attribute 'hitl-plan-done-face :strike-through))))

(ert-deftest hitl-plan-ui-test-review-by-id ()
  (hitl-plan-ui-test--with-env
    (let ((plan (hitl-plan-ui-test--plan)))
      (should (buffer-live-p (hitl-plan-review (hitl-plan-id plan))))
      (should-error (hitl-plan-review "nope") :type 'user-error))))

(ert-deftest hitl-plan-ui-test-toggle-skip ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (buf (hitl-plan-review plan))
           (step (car (hitl-plan-steps plan))))
      (with-current-buffer buf
        (hitl-plan-ui-test--goto-step 1)
        (hitl-plan-ui-toggle-skip)
        (should (eq 'skipped (hitl-plan-step-status step)))
        (should (string-match-p "\\[skipped\\]" (buffer-string)))
        (hitl-plan-ui-test--goto-step 1)
        (hitl-plan-ui-toggle-skip)
        (should (eq 'pending (hitl-plan-step-status step)))
        ;; persisted
        (should (eq 'pending (hitl-plan-step-status
                              (car (hitl-plan-steps
                                    (hitl-plan-load (hitl-plan-id plan)))))))))))

(ert-deftest hitl-plan-ui-test-bulk-selection ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (steps (hitl-plan-steps plan)))
      (setf (hitl-plan-step-status (nth 2 steps)) 'done)
      (with-current-buffer (hitl-plan-review plan nil)
        (should (string-match-p "\\[x\\]" (buffer-string)))
        (hitl-plan-ui-skip-all)
        (should (equal (mapcar #'hitl-plan-step-status steps) '(skipped skipped done)))
        (should (string-match-p "\\[ \\]" (buffer-string)))
        (hitl-plan-ui-test--goto-step 1)
        (hitl-plan-ui-toggle-skip)
        (should (equal (mapcar #'hitl-plan-step-status steps) '(pending skipped done)))
        (hitl-plan-ui-select-all)
        (should (equal (mapcar #'hitl-plan-step-status steps) '(pending pending done)))
        (should (equal (mapcar #'hitl-plan-step-status
                               (hitl-plan-steps (hitl-plan-load (hitl-plan-id plan))))
                       '(pending pending done)))))))

(ert-deftest hitl-plan-ui-test-reselect-completed-plan ()
  (hitl-plan-ui-test--with-env
    (dolist (command '(hitl-plan-ui-toggle-skip hitl-plan-ui-select-all))
      (let* ((plan (hitl-plan-ui-test--plan 'complete))
             (steps (hitl-plan-steps plan)))
        (dolist (step steps)
          (setf (hitl-plan-step-status step) 'done))
        (setf (hitl-plan-step-status (car steps)) 'skipped)
        (with-current-buffer (hitl-plan-review plan nil)
          (hitl-plan-ui-test--goto-step 1)
          (funcall command)
          (should (eq 'paused (hitl-plan-status plan)))
          (should (eq 'paused (hitl-plan-status (hitl-plan-load (hitl-plan-id plan)))))
          (hitl-plan-ui-execute)
          (should (eq 'done (hitl-plan-step-status (car steps))))
          (should (eq 'complete (hitl-plan-status plan))))))))

(ert-deftest hitl-plan-ui-test-add-op ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (buf (hitl-plan-review plan)))
      (with-current-buffer buf
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "other"))
                  ((symbol-function 'annotated-completing-read) nil))
          (hitl-plan-ui-add-op))
        (should (= 2 (length (hitl-plan-ops plan))))
        (should (= 6 (length (hitl-plan-steps plan))))
        (should (string-match-p "other" (buffer-string)))))))

(ert-deftest hitl-plan-ui-test-edit-note ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (buf (hitl-plan-review plan)))
      (with-current-buffer buf
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "hello")))
          (hitl-plan-ui-edit-note))
        (should (equal "hello" (hitl-plan-note plan)))
        (should (eq 'one (hitl-plan-log-notes plan)))
        (should (string-match-p "Plan note: +hello" (buffer-string)))
        (setf (hitl-plan-log-notes plan) 'each)
        (hitl-plan-ui-test--goto-step 2)
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "step note")))
          (hitl-plan-ui-edit-note))
        (should (equal "step note" (hitl-plan-step-note (nth 1 (hitl-plan-steps plan)))))))))

(ert-deftest hitl-plan-ui-test-cycle-log-notes ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (buf (hitl-plan-review plan))
           (n 0))
      (setf (hitl-plan-log-notes plan) 'ignore)
      (with-current-buffer buf
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (format "n%d" (cl-incf n)))))
          (hitl-plan-ui-cycle-log-notes)
          (should (eq 'one (hitl-plan-log-notes plan)))
          (should (equal "n1" (hitl-plan-note plan)))
          (hitl-plan-ui-cycle-log-notes)
          (should (eq 'each (hitl-plan-log-notes plan)))
          (should (equal '("n2" "n3" "n4")
                         (mapcar #'hitl-plan-step-note (hitl-plan-steps plan))))
          (hitl-plan-ui-cycle-log-notes)
          (should (eq 'ignore (hitl-plan-log-notes plan))))))))

(ert-deftest hitl-plan-ui-test-visit ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (buf (hitl-plan-review plan)))
      (with-current-buffer buf
        (hitl-plan-ui-test--goto-step 1)
        (hitl-plan-ui-visit-target)
        (should (equal "Alpha" (plist-get (car hitl-plan-ui-test--visited) :name)))
        (hitl-plan-ui-test--goto-step 2)
        (should-error (hitl-plan-ui-visit-target) :type 'user-error)))))

(ert-deftest hitl-plan-ui-test-discard ()
  (hitl-plan-ui-test--with-env
    (dolist (cmd '(hitl-plan-ui-discard))
      (let* ((plan (hitl-plan-ui-test--plan))
             (buf (hitl-plan-review plan)))
        (with-current-buffer buf
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (funcall cmd))
          (should (eq 'draft (hitl-plan-status plan)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (funcall cmd)))
        (should (eq 'discarded (hitl-plan-status plan)))
        (should-not (buffer-live-p buf))))
    (should (eq (lookup-key hitl-plan-mode-map "k") 'hitl-plan-ui-discard))))

(ert-deftest hitl-plan-ui-test-execute ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (buf (hitl-plan-review plan)))
      (with-current-buffer buf
        (hitl-plan-ui-test--goto-step 3)
        (hitl-plan-ui-toggle-skip)
        (hitl-plan-ui-execute)
        (should (equal '(done failed skipped)
                       (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
        (should (eq 'complete (hitl-plan-status plan)))
        (should (string-match-p "\\[done +\\]" (buffer-string)))
        (should (string-match-p "target no longer resolves" (buffer-string)))
        (should (eq 'hitl-plan-done-face
                    (get-text-property (+ 4 (save-excursion
                                              (hitl-plan-ui-test--goto-step 1)
                                              (line-beginning-position)))
                                       'face)))))))

(ert-deftest hitl-plan-ui-test-execute-each-prompts ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (buf (hitl-plan-review plan)))
      (setf (hitl-plan-log-notes plan) 'each)
      (with-current-buffer buf
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "why")))
          (hitl-plan-ui-execute))
        (should (equal "why" (hitl-plan-step-note (car (hitl-plan-steps plan)))))))))

(ert-deftest hitl-plan-ui-test-resume ()
  (hitl-plan-ui-test--with-env
    (let* ((p1 (hitl-plan-ui-test--plan 'paused))
           (_p2 (hitl-plan-ui-test--plan 'complete))
           (seen nil))
      (cl-letf (((symbol-function 'annotated-completing-read) nil)
                ((symbol-function 'completing-read)
                 (lambda (_p table &rest _)
                   (setq seen (all-completions "" table))
                   (hitl-plan-id p1))))
        (let ((buf (call-interactively #'hitl-plan-resume)))
          (should (equal (list (hitl-plan-id p1)) seen))
          (should (equal (buffer-name buf)
                         (format "*hitl-plan: %s*" (hitl-plan-id p1)))))))))

(ert-deftest hitl-plan-ui-test-indicator-plans ()
  (hitl-plan-ui-test--with-env
    (require 'hitl)
    (hitl-plan-ui-test--plan 'paused)
    (cl-letf (((symbol-function 'hitl-list-pending) (lambda () nil)))
      (let ((hitl-indicator-include-plans nil))
        (hitl--update-indicator)
        (should (equal "" hitl--indicator-string)))
      (let ((hitl-indicator-include-plans t))
        (hitl--update-indicator)
        (should (equal " HITL[P:1]" (substring-no-properties hitl--indicator-string)))))))

(ert-deftest hitl-plan-ui-test-resume-none ()
  (hitl-plan-ui-test--with-env
    (hitl-plan-ui-test--plan 'complete)
    (should-error (hitl-plan-resume) :type 'user-error)))

(ert-deftest hitl-plan-ui-test-missing-status ()
  (hitl-plan-ui-test--with-env
    (should (eq 'hitl-plan-missing-face (hitl-plan-ui--step-face 'missing)))
    (should (eq 'hitl-plan-failed-face (hitl-plan-ui--step-face 'weird)))
    (should (eq 'hitl-plan-pending-face (hitl-plan-ui--step-face 'pending)))
    (hitl-plan-register-op t 'any :apply #'ignore)
    (hitl-plan-register-domain 'miss :resolve-target #'ignore
      :unresolved-status 'missing)
    (let ((plan (hitl-plan-create :domain 'miss :targets '(a) :ops '(any))))
      (hitl-plan-execute plan)
      (with-temp-buffer
        (hitl-plan-mode)
        (setq hitl-plan-ui--plan plan)
        (hitl-plan-ui--render plan)
        (should (string-match-p "1 missing" (buffer-string)))
        (should (string-match-p "\\[missing\\]" (buffer-string)))))))


(ert-deftest hitl-plan-ui-test-execute-one-key-and-command ()
  (hitl-plan-ui-test--with-env
    (should (eq 'hitl-plan-ui-execute-one (lookup-key hitl-plan-mode-map "o")))
    (hitl-plan-register-op t 'any :apply #'ignore)
    (hitl-plan-register-domain 'one :resolve-target #'identity)
    (let ((plan (hitl-plan-create :domain 'one :targets '(a b) :ops '(any))))
      (with-temp-buffer
        (hitl-plan-mode)
        (setq hitl-plan-ui--plan plan)
        (hitl-plan-ui-execute-one)
        (should (equal '(done pending)
                       (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
        (should (eq 'paused (hitl-plan-status plan)))))))

;;; Directory carrying, filters, display

(ert-deftest hitl-plan-ui-test/refresh-reloads-from-plan-directory ()
  (hitl-plan-ui-test--with-env
    (let* ((other (make-temp-file "hitl-plan-ui-other" t))
           (plan (hitl-plan-create :domain 'stub :ops '(mark) :directory other
                                   :targets (list (list :file "a.org" :name "A")))))
      (unwind-protect
          (let ((buf (hitl-plan-review plan nil)))
            (with-current-buffer buf
              (clrhash hitl-plan--store)
              (hitl-plan-ui-refresh)
              (should-not (eq hitl-plan-ui--plan plan))
              (should (equal (file-name-as-directory other)
                             (file-name-as-directory (hitl-plan-directory hitl-plan-ui--plan))))
              (should (equal (hitl-plan-id plan) (hitl-plan-id hitl-plan-ui--plan)))))
        (delete-directory other t)))))

(ert-deftest hitl-plan-ui-test/resolve-plan-uses-directory ()
  (hitl-plan-ui-test--with-env
    (let* ((other (make-temp-file "hitl-plan-ui-other" t))
           (plan (hitl-plan-create :domain 'stub :ops '(mark) :directory other
                                   :targets (list (list :file "a.org")))))
      (unwind-protect
          (progn
            (clrhash hitl-plan--store)
            (should (hitl-plan-ui--resolve-plan (hitl-plan-id plan) other))
            (clrhash hitl-plan--store)
            (should-error (hitl-plan-ui--resolve-plan (hitl-plan-id plan))
                          :type 'user-error))
        (delete-directory other t)))))

(ert-deftest hitl-plan-ui-test/review-display-nil-does-not-pop ()
  (hitl-plan-ui-test--with-env
    (let ((plan (hitl-plan-ui-test--plan)) popped)
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq popped b))))
        (should (buffer-live-p (hitl-plan-review plan nil)))
        (should-not popped)
        (let ((b (hitl-plan-review plan)))
          (should (eq popped b)))))))

(ert-deftest hitl-plan-ui-test/read-plan-and-resume-filters ()
  (hitl-plan-ui-test--with-env
    (hitl-plan-register-domain 'other :resolve-target #'identity)
    (hitl-plan-register-op 'other 'o :apply #'ignore)
    (let* ((a (hitl-plan-ui-test--plan))
           (b (hitl-plan-create :domain 'other :ops '(o) :targets '(x)))
           seen)
      (cl-letf (((symbol-function 'hitl-plan-ui--complete)
                 (lambda (_p cands &rest _)
                   (setq seen (mapcar #'car cands))
                   (car seen))))
        (should (equal (hitl-plan-id b)
                       (hitl-plan-ui-read-plan '(draft) "p: " nil 'other)))
        (should (equal (list (hitl-plan-id b)) seen))
        (should (equal (hitl-plan-id a)
                       (hitl-plan-ui-read-plan '(draft) "p: " nil 'stub)))
        (should (equal (list (hitl-plan-id a)) seen)))
      (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
        (should (eq 'other (hitl-plan-domain
                            (with-current-buffer
                                (progn (hitl-plan-resume (hitl-plan-id b) nil 'other)
                                       (get-buffer (hitl-plan-ui-buffer-name b)))
                              hitl-plan-ui--plan))))))))

;;; Review follow-ups

(ert-deftest hitl-plan-ui-test-age ()
  (let ((ago (lambda (secs)
               (hitl-plan-ui--age (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                                      (time-subtract (current-time) secs))))))
    (should (string-match-p "\\`[0-9]+s ago\\'" (funcall ago 5)))
    (should (equal "5m ago" (funcall ago (* 5 60 1.0 1.0001))))
    (should (equal "3h ago" (funcall ago (+ 5 (* 3 3600)))))
    (should (equal "2d ago" (funcall ago (+ 5 (* 2 86400)))))
    ;; A future time clamps to 0s.
    (should (equal "0s ago" (funcall ago -100)))
    (should (equal "unknown age" (hitl-plan-ui--age nil)))
    (should (equal "unknown age" (hitl-plan-ui--age "not a date")))))

(ert-deftest hitl-plan-ui-test-annotate ()
  (hitl-plan-ui-test--with-env
    (let ((plan (hitl-plan-ui-test--plan 'paused)))
      (setf (hitl-plan-step-status (car (hitl-plan-steps plan))) 'done)
      ;; Without a description the ops are described.
      (should (string-match-p "\\`\\[paused\\] 1/3 steps (.* ago) — mark\\'"
                              (hitl-plan-ui--annotate plan)))
      (setf (hitl-plan-description plan) "my desc")
      (should (string-suffix-p "— my desc" (hitl-plan-ui--annotate plan))))))

(ert-deftest hitl-plan-ui-test-prompt-step-notes ()
  "Only pending steps are asked; empty answers clear; the plan is saved."
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (steps (hitl-plan-steps plan))
           (answers (list "first" ""))
           prompts defaults)
      (setf (hitl-plan-step-status (nth 1 steps)) 'done
            (hitl-plan-step-note (nth 2 steps)) "old")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional initial &rest _)
                   (push prompt prompts) (push initial defaults)
                   (pop answers))))
        (hitl-plan-ui--prompt-step-notes plan))
      (should (= 2 (length prompts)))
      (should (string-match-p "Note for step 1 (a.org Alpha)" (car (last prompts))))
      (should (equal '("old" nil) defaults))
      (should (equal "first" (hitl-plan-step-note (nth 0 steps))))
      (should-not (hitl-plan-step-note (nth 1 steps)))
      (should-not (hitl-plan-step-note (nth 2 steps)))
      (clrhash hitl-plan--store)
      (should (equal "first" (hitl-plan-step-note
                              (car (hitl-plan-steps (hitl-plan-load (hitl-plan-id plan))))))))))

(ert-deftest hitl-plan-ui-test-format-step ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-ui-test--plan))
           (step (car (hitl-plan-steps plan))))
      ;; Single-op: no op column.
      (let ((line (hitl-plan-ui--format-step plan step nil)))
        (should (string-match-p "\\`\\[x\\]  +1 \\[pending\\] a.org +Alpha\n\\'" line))
        (should (eq step (get-text-property 0 'hitl-plan-step line))))
      ;; Multi-op: a 14 column op description, truncated.
      (hitl-plan-register-op 'stub 'a-very-long-operation-name :apply #'ignore)
      (setf (hitl-plan-step-op step)
            (hitl-plan-op--make :name 'a-very-long-operation-name))
      (should (string-match-p "\\[pending\\] a-very-long-o… +a.org"
                              (hitl-plan-ui--format-step plan step t)))
      ;; Message and note.
      (setf (hitl-plan-step-status step) 'failed
            (hitl-plan-step-message step) "went wrong"
            (hitl-plan-step-note step) "my note")
      (let ((line (hitl-plan-ui--format-step plan step nil)))
        (should (string-match-p " — went wrong" line))
        (should (string-match-p "{my note}" line))
        (let ((pos (string-search "went wrong" line)))
          (should (eq 'hitl-plan-failed-face (get-text-property pos 'face line)))))
      ;; A non-failure message is shaded, an empty note omitted.
      (setf (hitl-plan-step-status step) 'skipped
            (hitl-plan-step-message step) "skipped by user"
            (hitl-plan-step-note step) "")
      (let ((line (hitl-plan-ui--format-step plan step nil)))
        (should (eq 'shadow (get-text-property (string-search "skipped by user" line) 'face line)))
        (should-not (string-match-p "{" line))))))

(ert-deftest hitl-plan-ui-test-refresh-plan ()
  (hitl-plan-ui-test--with-env
    (let ((plan (hitl-plan-ui-test--plan)))
      ;; No buffer: nil, and no buffer is created.
      (should-not (hitl-plan-ui-refresh-plan plan))
      (should-not (get-buffer (hitl-plan-ui-buffer-name plan)))
      (let ((buf (hitl-plan-review plan nil)))
        (setf (hitl-plan-step-status (car (hitl-plan-steps plan))) 'skipped
              (hitl-plan-step-message (car (hitl-plan-steps plan))) "mm")
        (should (eq buf (hitl-plan-ui-refresh-plan plan)))
        (with-current-buffer buf
          (should (string-match-p "\\[skipped\\].* — mm" (buffer-string))))))))


;;; Security regression tests

(ert-deftest hitl-plan-ui-test-sec-clean-string ()
  (should (equal (hitl-plan-ui-clean-string "a\nb\rc\td") "a\\nb\\rc\\td"))
  (should-not (string-match-p "[‮⁦‏‎‪⁩]"
                              (hitl-plan-ui-clean-string "x‮y⁦z‏‎‪⁩")))
  (should (equal (hitl-plan-ui-clean-string "a\0b\e[31m") "a\\x00b\\x1b[31m"))
  (should (<= (string-width (hitl-plan-ui-clean-string (make-string 500 ?x) 40)) 40))
  (should (string-suffix-p "…" (hitl-plan-ui-clean-string (make-string 500 ?x) 40)))
  (should (equal (hitl-plan-ui-clean-string nil) ""))
  (should (equal (hitl-plan-ui-clean-string 42) "42")))

(ert-deftest hitl-plan-ui-test-sec-render-injection ()
  (hitl-plan-ui-test--with-env
    (let* ((evil "x\nOperations:  1. harmless\nProgress:    9/9 done‮")
           (plan (hitl-plan-create
                  :domain 'stub :ops (list (cons 'mark (list evil)))
                  :description evil :note evil :log-notes 'one
                  :targets (list (list :file (concat "/very/long/dir/" (make-string 80 ?d) "/a.org")
                                       :name evil))))
           (step (car (hitl-plan-steps plan))))
      (setf (hitl-plan-step-message step) evil
            (hitl-plan-step-note step) evil)
      (with-current-buffer (hitl-plan-review plan nil)
        (let ((s (buffer-string)))
          (should-not (string-match-p "‮" s))
          (should (= 1 (how-many "^Operations:" (point-min) (point-max))))
          (should (= 1 (how-many "^Progress:" (point-min) (point-max))))
          (should (= 1 (how-many "^Description:" (point-min) (point-max))))
          ;; exactly one step line
          (should (= 1 (how-many "\\[pending" (point-min) (point-max))))
          (should (string-match-p "x\\\\nOperations:" s)))))))

(ert-deftest hitl-plan-ui-test-sec-file-column-full-path ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-create
                  :domain 'stub :ops '(mark)
                  :targets (list (list :file "/tmp/one/dir/a.org" :name "A")
                                 (list :file "/tmp/two/dir/a.org" :name "B"))))
           (buf (hitl-plan-review plan nil)))
      (with-current-buffer buf
        (let ((s (buffer-string)))
          (should (string-match-p "/tmp/one/dir/a\\.org" s))
          (should (string-match-p "/tmp/two/dir/a\\.org" s))))
      (let ((long (concat "/" (mapconcat #'identity (make-list 30 "dirname") "/") "/leaf.org")))
        (setf (hitl-plan-step-target (car (hitl-plan-steps plan))) (list :file long :name "A"))
        (with-current-buffer (hitl-plan-review plan nil)
          (should (string-match-p "…[^ ]*leaf\\.org" (buffer-string))))))))

(ert-deftest hitl-plan-ui-test-sec-op-args-printed-with-S ()
  (hitl-plan-ui-test--with-env
    (hitl-plan-register-op 'stub 'plain :apply #'ignore)
    (let ((plan (hitl-plan-create :domain 'stub :ops (list (cons 'plain (list "a\nb c")))
                                  :targets '((:file "a" :name "A")))))
      (with-current-buffer (hitl-plan-review plan nil)
        (should (string-match-p "plain \"a\\\\nb c\"" (buffer-string)))))))

(ert-deftest hitl-plan-ui-test-sec-mcp-plan-needs-confirmation ()
  (hitl-plan-ui-test--with-env
    (let* ((plan (hitl-plan-create :domain 'stub :ops '(mark)
                                   :metadata '(:origin mcp)
                                   :targets '((:file "a" :name "A") (:file "c" :name "C"))))
           (buf (hitl-plan-review plan nil))
           (prompts nil))
      (with-current-buffer buf
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (p) (push p prompts) nil)))
          (should-error (hitl-plan-ui-execute) :type 'user-error))
        (should-not hitl-plan-ui-test--applied)
        (should (= 1 (length prompts)))
        (should (string-match-p (regexp-quote (hitl-plan-id plan)) (car prompts)))
        (should (string-match-p "mark" (car prompts)))
        (should (string-match-p "2 step" (car prompts)))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) t)))
          (hitl-plan-ui-execute))
        (should (= 2 (length hitl-plan-ui-test--applied)))))
    ;; Non-mcp plans never prompt.
    (let* ((plan (hitl-plan-ui-test--plan))
           (buf (hitl-plan-review plan nil)))
      (with-current-buffer buf
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_p) (error "prompted"))))
          (hitl-plan-ui-execute))))))

(ert-deftest hitl-plan-ui-test-keys-legend-short-lines-and-bindings ()
  (hitl-plan-ui-test--with-env
    (with-current-buffer (hitl-plan-review (hitl-plan-ui-test--plan))
      (let ((s (buffer-string)))
        (should (string-match-p "^Run: +\\[C-c C-c/c/x\\] execute +\\[o\\] one step" s))
        (should (string-match-p "\\[,\\] commands" s)))
      (seq-do (lambda (line) (should (< (length line) 70)))
              (seq-filter (lambda (l) (string-match-p "\\`\\(Run\\|Select\\|Plan\\|Other\\):" l))
                          (split-string (buffer-string) "\n")))))
  (should (eq #'hitl-plan-ui-command (keymap-lookup hitl-plan-mode-map ",")))
  (should (eq #'hitl-plan-ui-execute-one (keymap-lookup hitl-plan-mode-map "o")))
  ;; Every legend command is bound to something in the map.
  (seq-do (lambda (e) (should (commandp (nth 2 e))))
          (seq-mapcat #'cdr hitl-plan-ui--key-groups)))

(ert-deftest hitl-plan-ui-test-command-picker-runs-choice ()
  (let (ran)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "one step"))
              ((symbol-function 'hitl-plan-ui-execute-one) (lambda () (interactive) (setq ran t))))
      (hitl-plan-ui-command))
    (should ran)))

(provide 'hitl-plan-ui-test)
;;; hitl-plan-ui-test.el ends here
