;;; claude-cli-permission.el --- Permission handling for Claude sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude CLI Wrapper Contributors
;; Keywords: tools, processes
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module handles permission requests from the Claude CLI.
;;
;; When the CLI wants to execute a potentially dangerous tool, it sends
;; a control_request with subtype "can_use_tool". The SDK must respond
;; with either an allow or deny response.
;;
;; Permission handlers are functions that receive (tool-name input) and
;; return a permission decision:
;; - '(allow) or '(allow . updated-input)
;; - '(deny message) or '(deny message . t) for deny+interrupt
;;
;; Built-in handlers:
;; - claude-cli-permission-allow-all: Allows everything
;; - claude-cli-permission-deny-all: Denies everything
;; - claude-cli-permission-interactive: Prompts user via minibuffer

;;; Code:

(require 'cl-lib)
(require 'claude-cli-protocol)

;;; Permission decision types

(defun claude-cli-permission-allow (&optional updated-input)
  "Create an allow decision, optionally with UPDATED-INPUT."
  (if updated-input
      (cons 'allow updated-input)
    '(allow)))

(defun claude-cli-permission-deny (message &optional interrupt)
  "Create a deny decision with MESSAGE.
If INTERRUPT is non-nil, also interrupt the session."
  (if interrupt
      `(deny ,message . t)
    `(deny ,message)))

;;; Permission decision accessors

(defun claude-cli-permission-decision-type (decision)
  "Get the type (allow or deny) from DECISION."
  (car decision))

(defun claude-cli-permission-decision-allow-p (decision)
  "Check if DECISION is an allow."
  (eq (car decision) 'allow))

(defun claude-cli-permission-decision-deny-p (decision)
  "Check if DECISION is a deny."
  (eq (car decision) 'deny))

(defun claude-cli-permission-decision-updated-input (decision)
  "Get updated input from an allow DECISION, or nil."
  (when (claude-cli-permission-decision-allow-p decision)
    (cdr decision)))

(defun claude-cli-permission-decision-message (decision)
  "Get the message from a deny DECISION."
  (when (claude-cli-permission-decision-deny-p decision)
    (cadr decision)))

(defun claude-cli-permission-decision-interrupt-p (decision)
  "Check if a deny DECISION also requests interrupt."
  (when (claude-cli-permission-decision-deny-p decision)
    (cddr decision)))

;;; Built-in permission handlers

(defun claude-cli-permission-allow-all (_tool-name _input)
  "Permission handler that allows all tool executions.
Suitable for trusted environments or bypass mode."
  (claude-cli-permission-allow))

(defun claude-cli-permission-deny-all (_tool-name _input)
  "Permission handler that denies all tool executions.
Suitable for read-only or sandboxed sessions."
  (claude-cli-permission-deny "All tool executions denied by policy"))

(defun claude-cli-permission-interactive (tool-name input)
  "Permission handler that prompts the user interactively.
Shows TOOL-NAME and INPUT, asks for confirmation."
  (let* ((input-preview (if (> (length (format "%S" input)) 100)
                            (concat (substring (format "%S" input) 0 100) "...")
                          (format "%S" input)))
         (prompt (format "Allow tool '%s' with input: %s? (y/n/q) "
                         tool-name input-preview))
         (response (read-char-choice prompt '(?y ?n ?q))))
    (pcase response
      (?y (claude-cli-permission-allow))
      (?n (claude-cli-permission-deny "User denied permission"))
      (?q (claude-cli-permission-deny "User interrupted" t)))))

;;; Permission request processing

(defun claude-cli-permission-handle-request (request-id request handler)
  "Process a permission REQUEST with REQUEST-ID using HANDLER.
Returns a control_response message plist ready to send to CLI."
  (let* ((tool-name (plist-get request :tool_name))
         (input (plist-get request :input))
         (decision (if handler
                       (funcall handler tool-name input)
                     (claude-cli-permission-deny "No permission handler configured"))))
    (claude-cli-permission--make-response request-id decision)))

(defun claude-cli-permission--make-response (request-id decision)
  "Create a control_response for REQUEST-ID based on DECISION."
  (if (claude-cli-permission-decision-allow-p decision)
      (claude-cli-protocol-permission-response
       request-id
       (claude-cli-protocol-permission-allow
        (claude-cli-permission-decision-updated-input decision)))
    (claude-cli-protocol-permission-response
     request-id
     (claude-cli-protocol-permission-deny
      (claude-cli-permission-decision-message decision)
      (claude-cli-permission-decision-interrupt-p decision)))))

;;; Handler composition

(defun claude-cli-permission-compose (&rest handlers)
  "Compose multiple permission HANDLERS.
Returns a handler that tries each in order until one allows.
If all deny, returns the last denial."
  (lambda (tool-name input)
    (let ((last-denial nil))
      (catch 'allowed
        (dolist (handler handlers)
          (let ((decision (funcall handler tool-name input)))
            (if (claude-cli-permission-decision-allow-p decision)
                (throw 'allowed decision)
              (setq last-denial decision))))
        (or last-denial
            (claude-cli-permission-deny "All handlers denied"))))))

(defun claude-cli-permission-for-tools (tool-list handler)
  "Create a handler that only applies HANDLER to tools in TOOL-LIST.
Returns nil (no decision) for other tools, allowing fallback."
  (lambda (tool-name input)
    (when (member tool-name tool-list)
      (funcall handler tool-name input))))

(defun claude-cli-permission-except-tools (tool-list handler)
  "Create a handler that applies HANDLER except for tools in TOOL-LIST.
Returns nil (no decision) for tools in the list."
  (lambda (tool-name input)
    (unless (member tool-name tool-list)
      (funcall handler tool-name input))))

;;; Safe tool lists

(defconst claude-cli-permission-safe-read-tools
  '("Read" "Glob" "Grep" "WebFetch" "WebSearch")
  "Tools that only read data and don't modify anything.")

(defconst claude-cli-permission-write-tools
  '("Write" "Edit" "Bash" "NotebookEdit")
  "Tools that can modify files or execute commands.")

(defun claude-cli-permission-allow-reads-only ()
  "Create a handler that allows read-only tools, denies others."
  (lambda (tool-name _input)
    (if (member tool-name claude-cli-permission-safe-read-tools)
        (claude-cli-permission-allow)
      (claude-cli-permission-deny
       (format "Tool '%s' is not in the safe read-only list" tool-name)))))

(provide 'claude-cli-permission)
;;; claude-cli-permission.el ends here
