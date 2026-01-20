;;; claude-cli-protocol.el --- Protocol types and NDJSON parsing for Claude CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude CLI Wrapper Contributors
;; Keywords: tools, processes
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides protocol message types and JSON parsing for
;; communicating with the Claude CLI over NDJSON (newline-delimited JSON).
;;
;; Message types from CLI:
;; - system (init, error)
;; - stream_event (content streaming)
;; - assistant (complete message)
;; - user (echoed messages)
;; - result (turn completion)
;; - control_request (permission prompts)
;;
;; Message types to CLI:
;; - user message
;; - control_request (interrupt, set_permission_mode)
;; - control_response (permission decisions)

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Message type constants

(defconst claude-cli-protocol-msg-system "system"
  "System message type (init, error).")

(defconst claude-cli-protocol-msg-assistant "assistant"
  "Assistant message type.")

(defconst claude-cli-protocol-msg-user "user"
  "User message type.")

(defconst claude-cli-protocol-msg-result "result"
  "Result message type (turn completion).")

(defconst claude-cli-protocol-msg-stream-event "stream_event"
  "Stream event message type.")

(defconst claude-cli-protocol-msg-control-request "control_request"
  "Control request message type.")

(defconst claude-cli-protocol-msg-control-response "control_response"
  "Control response message type.")

;;; Content block type constants

(defconst claude-cli-protocol-block-text "text"
  "Text content block type.")

(defconst claude-cli-protocol-block-thinking "thinking"
  "Thinking content block type.")

(defconst claude-cli-protocol-block-tool-use "tool_use"
  "Tool use content block type.")

(defconst claude-cli-protocol-block-tool-result "tool_result"
  "Tool result content block type.")

;;; Stream event type constants

(defconst claude-cli-protocol-stream-message-start "message_start"
  "Stream event: message started.")

(defconst claude-cli-protocol-stream-message-delta "message_delta"
  "Stream event: message delta.")

(defconst claude-cli-protocol-stream-message-stop "message_stop"
  "Stream event: message stopped.")

(defconst claude-cli-protocol-stream-content-block-start "content_block_start"
  "Stream event: content block started.")

(defconst claude-cli-protocol-stream-content-block-delta "content_block_delta"
  "Stream event: content block delta.")

(defconst claude-cli-protocol-stream-content-block-stop "content_block_stop"
  "Stream event: content block stopped.")

;;; Permission mode constants

(defconst claude-cli-protocol-permission-default "default"
  "Default permission mode.")

(defconst claude-cli-protocol-permission-accept-edits "acceptEdits"
  "Accept edits permission mode.")

(defconst claude-cli-protocol-permission-plan "plan"
  "Plan permission mode.")

(defconst claude-cli-protocol-permission-bypass "bypassPermissions"
  "Bypass permissions mode.")

;;; Error type

(define-error 'claude-cli-protocol-error "Claude protocol error")

;;; JSON Parsing

(defun claude-cli-protocol-parse-message (json-string)
  "Parse JSON-STRING into a message plist.
Returns a plist with at minimum a :type key.
Signals `claude-cli-protocol-error' on parse failure."
  (condition-case err
      (let* ((json-object-type 'plist)
             (json-array-type 'list)
             (json-key-type 'keyword)
             (msg (json-read-from-string json-string)))
        msg)
    (json-error
     (signal 'claude-cli-protocol-error
             (list "Failed to parse JSON" json-string (cdr err))))))

(defun claude-cli-protocol-encode-message (msg)
  "Encode MSG plist as JSON string with trailing newline."
  (concat (json-encode msg) "\n"))

(defun claude-cli-protocol-message-type (msg)
  "Get the type string from MSG plist."
  (plist-get msg :type))

(defun claude-cli-protocol-message-subtype (msg)
  "Get the subtype string from MSG plist."
  (plist-get msg :subtype))

;;; Message Constructors (for sending to CLI)

(defvar claude-cli-protocol--request-counter 0
  "Counter for generating unique request IDs.")

(defun claude-cli-protocol--generate-request-id ()
  "Generate a unique request ID."
  (cl-incf claude-cli-protocol--request-counter)
  (format "req_%d_%08x"
          claude-cli-protocol--request-counter
          (random (expt 16 8))))

(defun claude-cli-protocol-user-message (content)
  "Create a user message to send to CLI with CONTENT string."
  `(:type "user"
    :message (:role "user" :content ,content)))

(defun claude-cli-protocol-control-request (request-id request)
  "Create a control request wrapper with REQUEST-ID and REQUEST body."
  `(:type "control_request"
    :request_id ,request-id
    :request ,request))

(defun claude-cli-protocol-set-permission-mode-request (mode)
  "Create set_permission_mode request body for MODE.
MODE should be a symbol: default, accept-edits, plan, or bypass."
  `(:subtype "set_permission_mode"
    :mode ,(pcase mode
             ('default claude-cli-protocol-permission-default)
             ('accept-edits claude-cli-protocol-permission-accept-edits)
             ('plan claude-cli-protocol-permission-plan)
             ('bypass claude-cli-protocol-permission-bypass)
             (_ (symbol-name mode)))))

(defun claude-cli-protocol-interrupt-request ()
  "Create interrupt request body."
  '(:subtype "interrupt"))

(defun claude-cli-protocol-permission-response (request-id response)
  "Create a control response wrapper with REQUEST-ID and RESPONSE body."
  `(:type "control_response"
    :response (:subtype "success"
               :request_id ,request-id
               :response ,response)))

(defun claude-cli-protocol-permission-allow (&optional updated-input)
  "Create permission allow response body.
Optional UPDATED-INPUT provides modified input to use."
  (if updated-input
      `(:behavior "allow" :updated_input ,updated-input)
    '(:behavior "allow")))

(defun claude-cli-protocol-permission-deny (message &optional interrupt)
  "Create permission deny response body with MESSAGE.
If INTERRUPT is non-nil, also interrupt the session."
  `(:behavior "deny"
    :message ,message
    ,@(when interrupt '(:interrupt t))))

;;; Message Accessors

(defun claude-cli-protocol-get-event (msg)
  "Get the event payload from a stream_event MSG."
  (plist-get msg :event))

(defun claude-cli-protocol-get-event-type (event)
  "Get the type from an EVENT payload."
  (plist-get event :type))

(defun claude-cli-protocol-get-content-block (event)
  "Get the content_block from a content_block_start EVENT."
  (plist-get event :content_block))

(defun claude-cli-protocol-get-delta (event)
  "Get the delta from a content_block_delta EVENT."
  (plist-get event :delta))

(defun claude-cli-protocol-get-index (event)
  "Get the index from a content block EVENT."
  (plist-get event :index))

(defun claude-cli-protocol-get-usage (msg)
  "Get the usage statistics from a result MSG."
  (plist-get msg :usage))

;;; Permission mode conversion

(defun claude-cli-protocol-permission-mode-to-symbol (mode-string)
  "Convert MODE-STRING to a symbol."
  (pcase mode-string
    ("default" 'default)
    ("acceptEdits" 'accept-edits)
    ("plan" 'plan)
    ("bypassPermissions" 'bypass)
    (_ (intern mode-string))))

(defun claude-cli-protocol-permission-mode-to-string (mode-symbol)
  "Convert MODE-SYMBOL to a wire format string."
  (pcase mode-symbol
    ('default claude-cli-protocol-permission-default)
    ('accept-edits claude-cli-protocol-permission-accept-edits)
    ('plan claude-cli-protocol-permission-plan)
    ('bypass claude-cli-protocol-permission-bypass)
    (_ (symbol-name mode-symbol))))

(provide 'claude-cli-protocol)
;;; claude-cli-protocol.el ends here
