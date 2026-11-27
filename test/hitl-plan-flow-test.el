;;; hitl-plan-flow-test.el --- Dynamic plan tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise durable gates, branching, and recovery in ordered plans.

;;; Code:

(require 'ert)
(require 'hitl-plan)

(defvar hitl-plan-flow-test--events nil)

(defmacro hitl-plan-flow-test--with-env (&rest body)
  "Run BODY with an isolated plan store and operation registry."
  (declare (indent 0))
  `(let* ((hitl-plan-directory (make-temp-file "hitl-plan-flow-" t))
          (hitl-plan--store (make-hash-table :test #'equal))
          (hitl-plan--op-registry (make-hash-table :test #'equal))
          (hitl-plan--domain-registry (make-hash-table :test #'eq))
          (hitl-plan-save-buffers nil)
          (hitl-plan-step-functions nil)
          (hitl-plan-after-apply-functions nil)
          (hitl-plan-flow-test--events nil))
     (unwind-protect
         (progn
           (hitl-plan-register-domain 'flow-test :resolve-target #'identity)
           ,@body)
       (delete-directory hitl-plan-directory t))))

(ert-deftest hitl-plan-flow-confirmation-survives-reload ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'gate
      :apply (lambda ()
               (push 'asked hitl-plan-flow-test--events)
               (hitl-plan-suspend '(:kind confirm :prompt "Continue?")))
      :resume (lambda (response)
                (push response hitl-plan-flow-test--events)))
    (hitl-plan-register-op 'flow-test 'finish
      :apply (lambda () (push 'finished hitl-plan-flow-test--events)))
    (let* ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                   :targets '((:id target)) :ops '(gate finish)))
           (plan-id (hitl-plan-id plan)))
      (hitl-plan-execute plan)
      (should (eq (hitl-plan-status plan) 'paused))
      (should (eq (hitl-plan-step-status (car (hitl-plan-steps plan))) 'waiting))
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(asked)))
      (clrhash hitl-plan--store)
      (setq plan (hitl-plan-load plan-id))
      (should (eq (hitl-plan-execution plan) 'steps))
      (should (equal (hitl-plan-step-request (car (hitl-plan-steps plan)))
                     '(:kind confirm :prompt "Continue?")))
      (hitl-plan-resolve-step plan 1 nil)
      (should-error (hitl-plan-resolve-step plan 1 t) :type 'user-error)
      (setq plan (hitl-plan-load plan-id))
      (should (eq (hitl-plan-step-status (car (hitl-plan-steps plan))) 'ready))
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(finished nil asked)))
      (should (eq (hitl-plan-status plan) 'complete))
      (should (equal (seq-map (lambda (entry) (plist-get entry :event))
                             (hitl-plan-step-history (car (hitl-plan-steps plan))))
                     '(waiting resolved done))))))

(ert-deftest hitl-plan-flow-choice-inserts-only-selected-branch ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'choose
      :apply (lambda () (hitl-plan-suspend '(:options (left right))))
      :resume (lambda (response)
                (hitl-plan-insert-steps
                 hitl-plan-current-plan 2
                 (list (list :target '(:id branch) :op (list 'record response))))))
    (hitl-plan-register-op 'flow-test 'record
      :apply (lambda (value) (push value hitl-plan-flow-test--events)))
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target))
                                  :ops '(choose (record finish)))))
      (hitl-plan-execute plan)
      (hitl-plan-resolve-step plan 1 'right)
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(finish right)))
      (should (equal (seq-map #'hitl-plan-step-id (hitl-plan-steps plan)) '(1 3 2)))
      (should (eq (hitl-plan-status plan) 'complete)))))

(ert-deftest hitl-plan-flow-repair-before-retry ()
  (hitl-plan-flow-test--with-env
    (let ((repaired nil))
      (hitl-plan-register-op 'flow-test 'merge
        :apply (lambda ()
                 (push 'merge hitl-plan-flow-test--events)
                 (unless repaired (error "Conflict"))))
      (hitl-plan-register-op 'flow-test 'repair
        :apply (lambda () (hitl-plan-suspend '(:queue-item "repair-1")))
        :resume (lambda (_response)
                  (setq repaired t)
                  (hitl-plan-retry-step hitl-plan-current-plan 1)))
      (hitl-plan-register-op 'flow-test 'finish
        :apply (lambda () (push 'finish hitl-plan-flow-test--events)))
      (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                    :targets '((:id target)) :ops '(merge finish))))
        (hitl-plan-execute plan)
        (should (equal hitl-plan-flow-test--events '(merge)))
        (should (eq (hitl-plan-status plan) 'paused))
        (hitl-plan-insert-steps plan 1 '((:target (:id repair) :op repair)))
        (hitl-plan-execute plan)
        (should (eq (hitl-plan-step-status (car (hitl-plan-steps plan))) 'waiting))
        (hitl-plan-resolve-step plan 3 'fixed)
        (hitl-plan-execute plan)
        (should (equal (seq-map (lambda (step)
                                 (list (hitl-plan-step-status step)
                                       (hitl-plan-step-message step)))
                               (hitl-plan-steps plan))
                       '((done nil) (done nil) (done nil))))
        (should (equal hitl-plan-flow-test--events '(finish merge merge)))
        (should (eq (hitl-plan-status plan) 'complete))
        (should (equal (seq-map (lambda (entry) (plist-get entry :event))
                               (hitl-plan-step-history (nth 1 (hitl-plan-steps plan))))
                       '(failed retry done)))))))

(ert-deftest hitl-plan-flow-resume-error-retries-resume-after-reload ()
  (hitl-plan-flow-test--with-env
    (let ((fail t))
      (hitl-plan-register-op 'flow-test 'gate
        :apply (lambda (value)
                 (push value hitl-plan-flow-test--events)
                 (hitl-plan-suspend '(:kind confirm)))
        :resume (lambda (response value)
                  (if fail
                      (error "Resume failed")
                    (push (list response value) hitl-plan-flow-test--events))))
      (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                    :targets '((:id target)) :ops '((gate original)))))
        (hitl-plan-execute plan)
        (hitl-plan-resolve-step plan 1 'approved)
        (hitl-plan-execute plan)
        (should (eq (hitl-plan-step-status (car (hitl-plan-steps plan))) 'failed))
        (setq plan (hitl-plan-load (hitl-plan-id plan)))
        (setq fail nil)
        (hitl-plan-retry-step plan 1)
        (should (eq (hitl-plan-step-status (car (hitl-plan-steps plan))) 'ready))
        (hitl-plan-execute plan)
        (should (equal hitl-plan-flow-test--events '((approved original) original)))
        (should (equal (seq-map (lambda (entry) (plist-get entry :event))
                               (hitl-plan-step-history (car (hitl-plan-steps plan))))
                       '(waiting resolved failed retry done)))))))

(ert-deftest hitl-plan-flow-repeat-wait-and-branch-skip ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'gate
      :apply (lambda () (hitl-plan-suspend '(:stage choice)))
      :resume (lambda (response)
                (pcase (plist-get (hitl-plan-step-request hitl-plan-current-step)
                                  :stage)
                  ('choice
                   (hitl-plan-skip-step hitl-plan-current-plan response "Other branch")
                   (hitl-plan-suspend '(:stage confirmation)))
                  ('confirmation
                   (unless response
                     (hitl-plan-skip-step hitl-plan-current-plan 3 "Declined"))))))
    (hitl-plan-register-op 'flow-test 'record
      :apply (lambda (value) (push value hitl-plan-flow-test--events)))
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target))
                                  :ops '(gate (record left) (record right)))))
      (hitl-plan-execute plan)
      (hitl-plan-resolve-step plan 1 2)
      (hitl-plan-execute plan)
      (should (eq (hitl-plan-status plan) 'paused))
      (should-not hitl-plan-flow-test--events)
      (setq plan (hitl-plan-load (hitl-plan-id plan)))
      (hitl-plan-resolve-step plan 1 t)
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(right)))
      (should (equal (seq-map (lambda (entry) (plist-get entry :response))
                             (hitl-plan-step-history (car (hitl-plan-steps plan))))
                     '(nil 2 nil t t))))))

(ert-deftest hitl-plan-flow-pause-limit-and-order ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-domain 'flow-test :resolve-target #'identity
      :driver (lambda (&rest _) (error "Ordered plans must not use batch drivers")))
    (hitl-plan-register-op 'flow-test 'pause :order-last t
      :apply (lambda ()
               (push 'pause hitl-plan-flow-test--events)
               (hitl-plan-pause hitl-plan-current-plan)))
    (hitl-plan-register-op 'flow-test 'record
      :apply (lambda (value) (push value hitl-plan-flow-test--events)))
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target))
                                  :ops '(pause (record second) (record third)))))
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(pause)))
      (should (eq (hitl-plan-status plan) 'paused))
      (hitl-plan-execute-one plan)
      (should (equal hitl-plan-flow-test--events '(second pause)))
      (should (eq (hitl-plan-status plan) 'paused))
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(third second pause)))
      (should (eq (hitl-plan-status plan) 'complete)))))

(ert-deftest hitl-plan-flow-append-preserves-inserted-order ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'record
      :apply (lambda (value) (push value hitl-plan-flow-test--events)))
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target)) :ops '((record initial)))))
      (hitl-plan-insert-steps plan 1 '((:target (:id target) :op (record before))))
      (hitl-plan-add-op plan '(record after) '((:id target)))
      (should (equal (seq-map #'hitl-plan-step-id (hitl-plan-steps plan)) '(2 1 3)))
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(after initial before)))
      (hitl-plan-add-op plan '(record later))
      (should (eq (hitl-plan-status plan) 'paused))
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(later after initial before))))))

(ert-deftest hitl-plan-flow-insert-callback-and-validation ()
  (hitl-plan-flow-test--with-env
    (let (added reset)
      (hitl-plan-register-domain 'flow-test :resolve-target #'identity
        :after-steps-added (lambda (_plan steps) (setq added steps))
        :after-reset (lambda (_plan steps) (setq reset steps)))
      (hitl-plan-register-op 'flow-test 'fail :apply (lambda () (error "Failure")))
      (let* ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                     :targets '((:id target)) :ops '(fail)))
             (before (hitl-plan-to-plist plan)))
        (should-error (hitl-plan-insert-steps plan 99 '((:op fail :target (:id x))))
                      :type 'user-error)
        (should-error (hitl-plan-insert-steps plan nil '((:op missing :target x)))
                      :type 'user-error)
        (should-error (hitl-plan-insert-steps plan nil '((:op fail))) :type 'user-error)
        (should-error (hitl-plan-insert-steps plan nil 'invalid) :type 'user-error)
        (should (equal before (hitl-plan-to-plist plan)))
        (let ((inserted (hitl-plan-insert-steps plan nil '((:op fail :target (:id x))))))
          (should (equal added inserted))
          (should (= (hitl-plan-step-id (car inserted)) 2)))
        (hitl-plan-execute plan)
        (hitl-plan-retry-step plan 1)
        (should (equal reset (list (car (hitl-plan-steps plan)))))
        (should-error (hitl-plan-retry-step plan 1) :type 'user-error)
        (should-error (hitl-plan-resolve-step plan 1 t) :type 'user-error)
        (hitl-plan-skip-step plan 1)
        (should-error (hitl-plan-skip-step plan 1) :type 'user-error)
        (hitl-plan-discard plan)
        (should-error (hitl-plan-insert-steps plan nil '((:op fail :target x)))
                      :type 'user-error)
        (should-error (hitl-plan-resolve-step plan 1 t) :type 'user-error)))))

(ert-deftest hitl-plan-flow-restrictions-and-reentrancy ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'guard
      :apply (lambda ()
               (should-error (hitl-plan-execute hitl-plan-current-plan)
                             :type 'user-error)
               (should-error (hitl-plan-skip-step hitl-plan-current-plan 1)
                             :type 'user-error)))
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target)) :ops '(guard))))
      (let ((hitl-plan-restricted t))
        (should-error (hitl-plan-insert-steps plan nil '((:op guard :target x)))
                      :type 'user-error))
      (should-error (hitl-plan-execute plan (car (hitl-plan-ops plan)))
                    :type 'user-error)
      (should-error (hitl-plan-execute plan nil 0) :type 'user-error)
      (should-error (hitl-plan-pause plan) :type 'user-error)
      (let ((hitl-plan--active-plans
             (list (cons (hitl-plan-id plan) (hitl-plan--directory plan)))))
        (should-error (hitl-plan-skip-step plan 1) :type 'user-error)
        (should-error (hitl-plan-insert-steps plan nil '((:op guard :target x)))
                      :type 'user-error))
      (hitl-plan-execute plan)
      (should (eq (hitl-plan-status plan) 'complete)))
    (should-error (hitl-plan-suspend '(:kind confirm)) :type 'user-error)
    (should-error (hitl-plan-create :domain 'flow-test :execution 'unknown)
                  :type 'user-error)))

(ert-deftest hitl-plan-flow-bulk-remains-compatible ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'fail :apply (lambda () (error "Failed")))
    (hitl-plan-register-op 'flow-test 'finish
      :apply (lambda () (push 'finish hitl-plan-flow-test--events)))
    (let ((plan (hitl-plan-create :domain 'flow-test :targets '((:id target))
                                  :ops '(fail finish))))
      (hitl-plan-execute plan)
      (should (eq (hitl-plan-status plan) 'complete))
      (should (equal hitl-plan-flow-test--events '(finish)))
      (should-error (hitl-plan-insert-steps plan nil '((:op finish :target x)))
                    :type 'user-error)
      (should-error (hitl-plan-resolve-step plan 1 t) :type 'user-error)
      (should (eq (hitl-plan-execution
                   (hitl-plan-from-plist '(:id "legacy" :ops nil :steps nil)))
                  'bulk)))))

(ert-deftest hitl-plan-flow-quit-during-resume-keeps-response ()
  (hitl-plan-flow-test--with-env
    (let ((interrupt t))
      (hitl-plan-register-op 'flow-test 'gate
        :apply (lambda ()
                 (push 'apply hitl-plan-flow-test--events)
                 (hitl-plan-suspend '(:kind confirm)))
        :resume (lambda (response)
                  (if interrupt
                      (signal 'quit nil)
                    (push response hitl-plan-flow-test--events))))
      (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                    :targets '((:id target)) :ops '(gate))))
        (hitl-plan-execute plan)
        (hitl-plan-resolve-step plan 1 'approved)
        (hitl-plan-execute plan)
        (should (eq (hitl-plan-status plan) 'paused))
        (should (eq (hitl-plan-step-status (car (hitl-plan-steps plan))) 'ready))
        (setq plan (hitl-plan-load (hitl-plan-id plan)))
        (setq interrupt nil)
        (hitl-plan-execute plan)
        (should (equal hitl-plan-flow-test--events '(approved apply)))))))

(ert-deftest hitl-plan-flow-missing-resume-handler-blocks ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'gate
      :apply (lambda () (hitl-plan-suspend '(:kind confirm))))
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target)) :ops '(gate))))
      (hitl-plan-execute plan)
      (should (eq (hitl-plan-status plan) 'paused))
      (should (string-match-p "no resume handler"
                              (hitl-plan-step-message (car (hitl-plan-steps plan))))))))

(ert-deftest hitl-plan-flow-running-checkpoint-recovers-after-restart ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'gate
      :apply (lambda () (hitl-plan-suspend '(:kind confirm)))
      :resume (lambda (response) (push response hitl-plan-flow-test--events)))
    (let* ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                   :targets '((:id target)) :ops '(gate)))
           (plan-id (hitl-plan-id plan)))
      (hitl-plan-execute plan)
      (setf (hitl-plan-status plan) 'running)
      (hitl-plan-save plan)
      (clrhash hitl-plan--store)
      (setq plan (hitl-plan-load plan-id))
      (should (eq (hitl-plan-status plan) 'paused))
      (hitl-plan-resolve-step plan 1 'approved)
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(approved)))
      (should (eq (hitl-plan-status plan) 'complete)))))

(ert-deftest hitl-plan-flow-invalid-response-preserves-saved-wait ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'gate
      :apply (lambda () (hitl-plan-suspend '(:kind confirm)))
      :resume #'ignore)
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target)) :ops '(gate))))
      (hitl-plan-execute plan)
      (let ((before (hitl-plan-to-plist plan)))
        (should-error (hitl-plan-resolve-step plan 1 (list (current-buffer)))
                      :type 'user-error)
        (should (equal before (hitl-plan-to-plist plan)))
        (should (equal before (hitl-plan-to-plist (hitl-plan-load (hitl-plan-id plan)))))))))

(ert-deftest hitl-plan-flow-invalid-request-does-not-corrupt-plan ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'gate
      :apply (lambda () (hitl-plan-suspend (list :buffer (current-buffer))))
      :resume #'ignore)
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target)) :ops '(gate))))
      (hitl-plan-execute plan)
      (should (eq (hitl-plan-step-status (car (hitl-plan-steps plan))) 'failed))
      (should-not (hitl-plan-step-request (car (hitl-plan-steps plan))))
      (should (hitl-plan-load (hitl-plan-id plan))))))

(ert-deftest hitl-plan-flow-choice-can-discard ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'gate
      :apply (lambda () (hitl-plan-suspend '(:options (stop continue))))
      :resume (lambda (response)
                (when (eq response 'stop)
                  (hitl-plan-discard hitl-plan-current-plan))))
    (hitl-plan-register-op 'flow-test 'finish
      :apply (lambda () (push 'finish hitl-plan-flow-test--events)))
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target)) :ops '(gate finish))))
      (hitl-plan-execute plan)
      (hitl-plan-resolve-step plan 1 'stop)
      (hitl-plan-execute plan)
      (should-not hitl-plan-flow-test--events)
      (should (eq (hitl-plan-status plan) 'discarded))
      (should (eq (hitl-plan-status (hitl-plan-load (hitl-plan-id plan))) 'discarded)))))

(ert-deftest hitl-plan-flow-resume-registration-can-be-restored ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'gate
      :apply (lambda () (hitl-plan-suspend '(:kind confirm))) :resume #'ignore)
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target)) :ops '(gate))))
      (hitl-plan-execute plan)
      (hitl-plan-resolve-step plan 1 'approved)
      (hitl-plan-register-op 'flow-test 'gate
        :apply (lambda () (error "Apply must not be replayed")))
      (hitl-plan-execute plan)
      (should (string-match-p "no resume handler"
                              (hitl-plan-step-message (car (hitl-plan-steps plan)))))
      (hitl-plan-register-op 'flow-test 'gate :apply #'ignore
        :resume (lambda (response) (push response hitl-plan-flow-test--events)))
      (hitl-plan-retry-step plan 1)
      (hitl-plan-execute plan)
      (should (equal hitl-plan-flow-test--events '(approved))))))

(ert-deftest hitl-plan-flow-unresolved-resume-retries-with-response ()
  (hitl-plan-flow-test--with-env
    (let ((available t))
      (hitl-plan-register-domain 'flow-test
        :resolve-target (lambda (target) (and available target))
        :unresolved-status 'missing)
      (hitl-plan-register-op 'flow-test 'gate
        :apply (lambda ()
                 (push 'apply hitl-plan-flow-test--events)
                 (hitl-plan-suspend '(:kind confirm)))
        :resume (lambda (response) (push response hitl-plan-flow-test--events)))
      (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                    :targets '((:id target)) :ops '(gate))))
        (hitl-plan-execute plan)
        (hitl-plan-resolve-step plan 1 'approved)
        (setq available nil)
        (hitl-plan-execute plan)
        (should (eq (hitl-plan-step-status (car (hitl-plan-steps plan))) 'missing))
        (should (eq (hitl-plan-status plan) 'paused))
        (setq available t)
        (hitl-plan-retry-step plan 1)
        (hitl-plan-execute plan)
        (should (equal hitl-plan-flow-test--events '(approved apply)))))))

(ert-deftest hitl-plan-flow-circular-response-remains-readable ()
  (hitl-plan-flow-test--with-env
    (hitl-plan-register-op 'flow-test 'gate
      :apply (lambda () (hitl-plan-suspend '(:kind text))) :resume #'ignore)
    (let ((plan (hitl-plan-create :domain 'flow-test :execution 'steps
                                  :targets '((:id target)) :ops '(gate)))
          (response (list 'answer)))
      (setcdr response response)
      (hitl-plan-execute plan)
      (hitl-plan-resolve-step plan 1 response)
      (setq plan (hitl-plan-load (hitl-plan-id plan)))
      (let ((saved (hitl-plan-step-response (car (hitl-plan-steps plan)))))
        (should (eq saved (cdr saved))))
      (hitl-plan-execute plan)
      (should (eq (hitl-plan-status plan) 'complete)))))

(provide 'hitl-plan-flow-test)
;;; hitl-plan-flow-test.el ends here
