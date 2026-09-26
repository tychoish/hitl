;;; test-hitl.el --- Isolated tests for hitl -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Keywords: tools, test

;;; Commentary:
;; Unit tests for hitl, hitl-prompt, hitl-cursor, and hitl-ui.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'hitl)

(defmacro hitl-test/isolate (&rest body)
  "Execute BODY with an isolated HITL store and cursor state."
  `(let ((hitl--store (make-hash-table :test #'equal))
         (hitl--cursors (make-hash-table :test #'equal))
         (hitl-on-question-created-functions nil)
         (hitl-on-question-answered-functions nil)
         (hitl-on-question-cancelled-functions nil)
         (hitl-on-question-expired-functions nil))
     ,@body))

(ert-deftest hitl-test/create-and-get ()
  "Test creating and retrieving questions."
  (hitl-test/isolate
   (let ((q (hitl-ask :prompt "Continue?" :kind :boolean :id "q-1")))
     (should (equal (hitl-question-id q) "q-1"))
     (should (equal (hitl-question-prompt q) "Continue?"))
     (should (eq (hitl-question-kind q) :boolean))
     (should (eq (hitl-question-status q) 'pending))
     (should (equal (hitl-get "q-1") q)))))

(ert-deftest hitl-test/kinds-normalization ()
  "Test kind normalization from symbols to keywords."
  (hitl-test/isolate
   (let ((q1 (hitl-ask :prompt "Single" :kind 'single-choice :id "q1"))
         (q2 (hitl-ask :prompt "Confirm" :kind :confirm :id "q2"))
         (q3 (hitl-ask :prompt "Number" :kind 'number :id "q3")))
     (should (eq (hitl-question-kind q1) :single-choice))
     (should (eq (hitl-question-kind q2) :confirm))
     (should (eq (hitl-question-kind q3) :number)))))

(ert-deftest hitl-test/answer-and-callback ()
  "Test answering a question and callback invocation."
  (hitl-test/isolate
   (let* ((callback-received nil)
          (cb (lambda (resp _q) (setq callback-received resp)))
          (q (hitl-ask :prompt "Pick color"
                       :kind :single-choice
                       :options '("red" "green" "blue")
                       :callback cb
                       :id "q-color")))
     (hitl-answer "q-color" "green")
     (should (eq (hitl-question-status q) 'answered))
     (should (equal (hitl-question-response q) "green"))
     (should (numberp (hitl-question-answered-at q)))
     (should (equal callback-received "green")))))

(ert-deftest hitl-test/cancel ()
  "Test cancelling a question."
  (hitl-test/isolate
   (let ((q (hitl-ask :prompt "Cancel me" :id "q-cancel")))
     (hitl-cancel "q-cancel" "aborted by user")
     (should (eq (hitl-question-status q) 'cancelled))
     (should (equal (hitl-question-response q) "Cancelled: aborted by user"))
     (should-not (member q (hitl-list-pending))))))

(ert-deftest hitl-test/timeout-expiration ()
  "Test question expiration when timeout is exceeded."
  (hitl-test/isolate
   (let ((q (hitl-ask :prompt "Quick" :timeout 0.001 :id "q-exp")))
     (sleep-for 0.01)
     (let ((expired (hitl-check-expirations)))
       (should (member "q-exp" expired))
       (should (eq (hitl-question-status q) 'expired))
       (should-not (member q (hitl-list-pending)))))))

(ert-deftest hitl-test/cursors ()
  "Test cursor iteration, peek, reset, and answer."
  (hitl-test/isolate
   (let ((q1 (hitl-ask :prompt "First" :id "c-1"))
         (q2 (hitl-ask :prompt "Second" :id "c-2")))
     ;; Peek does not advance
     (should (equal (hitl-question-id (hitl-cursor-peek "c")) "c-1"))
     (should (equal (hitl-question-id (hitl-cursor-peek "c")) "c-1"))
     ;; Next advances
     (should (equal (hitl-question-id (hitl-cursor-next "c")) "c-1"))
     (should (equal (hitl-question-id (hitl-cursor-next "c")) "c-2"))
     ;; Reset starts over
     (hitl-cursor-reset "c")
     (should (equal (hitl-question-id (hitl-cursor-next "c")) "c-1"))
     ;; Answering c-1 leaves c-2 as next
     (hitl-cursor-answer "c-1" "ans1")
     (should (equal (hitl-question-id (hitl-cursor-next "c-new")) "c-2")))))

(ert-deftest hitl-test/plist-roundtrip ()
  "Test question serialization and deserialization via plist."
  (hitl-test/isolate
   (let* ((q (hitl-ask :prompt "Name?"
                       :kind :text
                       :default-value "Tycho"
                       :options '("opt1")
                       :target "agent-1"
                       :directory "/tmp/"
                       :timeout 60
                       :metadata '(:foo "bar")
                       :id "q-plist"))
          (plist (hitl-question-to-plist q))
          (restored (hitl-question-from-plist plist)))
     (should (equal (hitl-question-id restored) "q-plist"))
     (should (equal (hitl-question-prompt restored) "Name?"))
     (should (eq (hitl-question-kind restored) :text))
     (should (equal (hitl-question-default-value restored) "Tycho"))
     (should (equal (hitl-question-target restored) "agent-1"))
     (should (equal (hitl-question-directory restored) "/tmp/"))
     (should (equal (hitl-question-timeout restored) 60))
     (should (equal (plist-get (hitl-question-metadata restored) :foo) "bar")))))

(ert-deftest hitl-test/json-roundtrip ()
  "Test question serialization and deserialization via JSON."
  (hitl-test/isolate
   (let* ((q (hitl-ask :prompt "JSON test" :kind :text :id "q-json"))
          (json-str (hitl-question-to-json q))
          (restored (hitl-question-from-json json-str)))
     (should (equal (hitl-question-id restored) "q-json"))
     (should (equal (hitl-question-prompt restored) "JSON test"))
     (should (eq (hitl-question-kind restored) :text)))))

(ert-deftest hitl-test/prompt-widgets ()
  "Test prompt widgets for various kinds."
  (hitl-test/isolate
   ;; Boolean
   (let ((qb (hitl-ask :prompt "Agree?" :kind :boolean :id "qb")))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
       (should (eq (hitl-prompt-question qb) t))))
   ;; Confirm
   (let ((qc (hitl-ask :prompt "Deploy?" :kind :confirm :default-value "yes" :id "qc")))
     (cl-letf (((symbol-function 'read-string) (lambda (_) "yes")))
       (should (eq (hitl-prompt-question qc) t))))
   ;; Number
   (let ((qn (hitl-ask :prompt "Count?" :kind :number :default-value 42 :id "qn")))
     (cl-letf (((symbol-function 'read-number) (lambda (_ def) def)))
       (should (= (hitl-prompt-question qn) 42))))))

(ert-deftest hitl-test/tabulated-ui ()
  "Test tabulated UI buffer creation."
  (hitl-test/isolate
   (hitl-ask :prompt "Prompt 1" :id "ui-1")
   (hitl-ask :prompt "Prompt 2" :id "ui-2")
   (hitl-list)
   (let ((buf (get-buffer "*hitl-questions*")))
     (should buf)
     (with-current-buffer buf
       (should (eq major-mode 'hitl-list-mode))
       (should (= (length tabulated-list-entries) 2)))
     (kill-buffer buf))))

(ert-deftest hitl-test/modeline-indicator ()
  "Test modeline indicator minor mode."
  (hitl-test/isolate
   (hitl-indicator-mode 1)
   (should (equal hitl--indicator-string ""))
   (hitl-ask :prompt "Needs answer" :id "ind-1")
   (should (string-match-p "HITL\\[1\\]" hitl--indicator-string))
   (hitl-answer "ind-1" "ok")
   (should (equal hitl--indicator-string ""))
   (hitl-indicator-mode -1)))

(provide 'test-hitl)
;;; test-hitl.el ends here
