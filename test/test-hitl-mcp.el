;;; test-hitl-mcp.el --- ERT tests for hitl-mcp.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'mcpkit)
(require 'hitl)
(require 'hitl-mcp)

(ert-deftest test-hitl-mcp/top-level-definition ()
  "Test that `hitl-mcp-service' is defined and has tools at load time."
  (should (mcpkit-service-p hitl-mcp-service))
  (should (eq (mcpkit-service-name hitl-mcp-service) 'hitl))
  (should (= (mcpkit-service-port hitl-mcp-service) 8766))
  (should (>= (hash-table-count (mcpkit-service-tools hitl-mcp-service)) 6)))

(ert-deftest test-hitl-mcp/registration ()
  "Test registering tools on the hitl service."
  (let ((mcpkit-registry nil))
    (let ((svc (hitl-mcp-register)))
      (should (mcpkit-service-p svc))
      (should (eq (mcpkit-service-name svc) 'hitl))
      (let ((tools (mcpkit-service-tools svc)))
        (should (gethash "ask_user" tools))
        (should (gethash "poll_question" tools))
        (should (gethash "get_next_question" tools))
        (should (gethash "list_pending_questions" tools))
        (should (gethash "cancel_question" tools))
        (should (gethash "answer_question" tools))))))

(ert-deftest test-hitl-mcp/question-lifecycle-tools ()
  "Test full question lifecycle via MCP tool handlers."
  (let ((hitl--store (make-hash-table :test #'equal))
        (hitl--cursors (make-hash-table :test #'equal))
        (svc (hitl-mcp-register)))
    ;; 1. ask_user
    (let* ((ask-tool (gethash "ask_user" (mcpkit-service-tools svc)))
           (ask-res (funcall (mcpkit-tool-handler ask-tool)
                             (list :prompt "Choose option"
                                   :kind "single-choice"
                                   :options ["Alpha" "Beta"]
                                   :target "shell-1")
                             (lambda (_status r) r))))
      (should (plist-get ask-res :question_id))
      (should (equal (plist-get ask-res :status) "pending"))
      (let ((qid (plist-get ask-res :question_id)))
        ;; 2. poll_question
        (let* ((poll-tool (gethash "poll_question" (mcpkit-service-tools svc)))
               (poll-res (funcall (mcpkit-tool-handler poll-tool)
                                  (list :question_id qid)
                                  (lambda (_status r) r))))
          (should (equal (plist-get poll-res :question_id) qid))
          (should (equal (plist-get poll-res :status) "pending"))
          (should (null (plist-get poll-res :response))))

        ;; 3. list_pending_questions
        (let* ((list-tool (gethash "list_pending_questions" (mcpkit-service-tools svc)))
               (list-res (funcall (mcpkit-tool-handler list-tool)
                                  (list :target "shell-1")
                                  (lambda (_status r) r))))
          (should (= (length list-res) 1))
          (should (equal (plist-get (car list-res) :question_id) qid)))

        ;; 4. get_next_question
        (let* ((cursor-tool (gethash "get_next_question" (mcpkit-service-tools svc)))
               (cursor-res (funcall (mcpkit-tool-handler cursor-tool)
                                    (list :cursor_id "c1" :target "shell-1")
                                    (lambda (_status r) r))))
          (should (equal (plist-get cursor-res :question_id) qid)))

        ;; 5. answer_question
        (let* ((answer-tool (gethash "answer_question" (mcpkit-service-tools svc)))
               (answer-res (funcall (mcpkit-tool-handler answer-tool)
                                    (list :question_id qid :response "Alpha")
                                    (lambda (_status r) r))))
          (should (equal (plist-get answer-res :status) "answered"))
          (should (equal (plist-get answer-res :response) "Alpha")))

        ;; 6. poll_question after answer
        (let* ((poll-tool (gethash "poll_question" (mcpkit-service-tools svc)))
               (poll-res (funcall (mcpkit-tool-handler poll-tool)
                                  (list :question_id qid)
                                  (lambda (_status r) r))))
          (should (equal (plist-get poll-res :status) "answered"))
          (should (equal (plist-get poll-res :response) "Alpha")))))))

(ert-deftest test-hitl-mcp/cancel-tool ()
  "Test cancel_question via MCP tool."
  (let ((hitl--store (make-hash-table :test #'equal))
        (svc (hitl-mcp-register)))
    (let* ((ask-tool (gethash "ask_user" (mcpkit-service-tools svc)))
           (ask-res (funcall (mcpkit-tool-handler ask-tool)
                             (list :prompt "Cancel me" :kind "text")
                             (lambda (_status r) r)))
           (qid (plist-get ask-res :question_id))
           (cancel-tool (gethash "cancel_question" (mcpkit-service-tools svc)))
           (cancel-res (funcall (mcpkit-tool-handler cancel-tool)
                                (list :question_id qid :reason "Operator aborted")
                                (lambda (_status r) r))))
      (should (equal (plist-get cancel-res :status) "cancelled")))))

(provide 'test-hitl-mcp)
;;; test-hitl-mcp.el ends here
