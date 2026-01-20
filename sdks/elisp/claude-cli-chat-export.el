;;; claude-cli-chat-export.el --- Export functionality for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file provides export functionality for Claude Chat conversations.
;; Supports exporting to Markdown, Org-mode, JSON, and plain text.

;;; Code:

(require 'cl-lib)
(require 'json)

;; Buffer-local variables
(defvar claude-cli-chat--conversation-history)
(defvar claude-cli-chat--total-input-tokens)
(defvar claude-cli-chat--total-output-tokens)
(defvar claude-cli-chat--total-cost)

;; Forward declarations
(declare-function claude-cli-chat--current-model "claude-cli-chat")

;;; Export to Markdown

(defun claude-cli-chat-export-markdown ()
  "Export conversation as Markdown to a file."
  (interactive)
  (let ((content (claude-cli-chat-export--to-markdown))
        (file (read-file-name "Export to file: " nil nil nil
                              (format "claude-cli-chat-%s.md"
                                      (format-time-string "%Y%m%d-%H%M%S")))))
    (with-temp-file file
      (insert content))
    (message "Exported to %s" file)))

(defun claude-cli-chat-export--to-markdown ()
  "Convert conversation history to Markdown format."
  (with-temp-buffer
    (insert "# Claude Chat Export\n\n")
    (insert (format "**Model**: %s\n" (or (claude-cli-chat--current-model) "Unknown")))
    (insert (format "**Date**: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (format "**Total Tokens**: %d input / %d output\n"
                    (or claude-cli-chat--total-input-tokens 0)
                    (or claude-cli-chat--total-output-tokens 0)))
    (insert (format "**Total Cost**: $%.4f\n\n"
                    (or claude-cli-chat--total-cost 0.0)))
    (insert "---\n\n")
    ;; Export each turn (history is in reverse order)
    (let ((turns (reverse claude-cli-chat--conversation-history)))
      (dolist (turn turns)
        (let ((num (plist-get turn :number))
              (user (plist-get turn :user-content))
              (assistant (plist-get turn :assistant-content))
              (thinking (plist-get turn :thinking))
              (tools (plist-get turn :tools)))
          (insert (format "## Turn %d\n\n" num))
          ;; User message
          (insert "### You\n\n")
          (insert user)
          (insert "\n\n")
          ;; Thinking (if present)
          (when (and thinking (not (string-empty-p thinking)))
            (insert "<details>\n")
            (insert "<summary>Thinking</summary>\n\n")
            (insert "```\n")
            (insert thinking)
            (insert "\n```\n\n")
            (insert "</details>\n\n"))
          ;; Tools (if present)
          (when tools
            (dolist (tool tools)
              (let ((name (plist-get tool :name))
                    (input (plist-get tool :input))
                    (result (plist-get tool :result))
                    (is-error (plist-get tool :is-error)))
                (insert (format "#### Tool: %s %s\n\n"
                                name
                                (if is-error "[Error]" "")))
                (when input
                  (insert "**Input**:\n```json\n")
                  (insert (claude-cli-chat-export--format-json input))
                  (insert "\n```\n\n"))
                (when result
                  (insert "**Result**:\n```\n")
                  (insert (format "%s" result))
                  (insert "\n```\n\n")))))
          ;; Assistant response
          (insert "### Claude\n\n")
          (insert (or assistant ""))
          (insert "\n\n")
          (insert "---\n\n"))))
    (buffer-string)))

;;; Export to Org-mode

(defun claude-cli-chat-export-org ()
  "Export conversation as Org-mode to a file."
  (interactive)
  (let ((content (claude-cli-chat-export--to-org))
        (file (read-file-name "Export to file: " nil nil nil
                              (format "claude-cli-chat-%s.org"
                                      (format-time-string "%Y%m%d-%H%M%S")))))
    (with-temp-file file
      (insert content))
    (message "Exported to %s" file)))

(defun claude-cli-chat-export--to-org ()
  "Convert conversation history to Org-mode format."
  (with-temp-buffer
    (insert "#+TITLE: Claude Chat Export\n")
    (insert (format "#+DATE: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (insert (format "#+PROPERTY: MODEL %s\n" (or (claude-cli-chat--current-model) "Unknown")))
    (insert (format "#+PROPERTY: TOKENS_IN %d\n" (or claude-cli-chat--total-input-tokens 0)))
    (insert (format "#+PROPERTY: TOKENS_OUT %d\n" (or claude-cli-chat--total-output-tokens 0)))
    (insert (format "#+PROPERTY: COST $%.4f\n\n" (or claude-cli-chat--total-cost 0.0)))
    ;; Export each turn
    (let ((turns (reverse claude-cli-chat--conversation-history)))
      (dolist (turn turns)
        (let ((num (plist-get turn :number))
              (user (plist-get turn :user-content))
              (assistant (plist-get turn :assistant-content))
              (thinking (plist-get turn :thinking))
              (tools (plist-get turn :tools)))
          (insert (format "* Turn %d\n" num))
          ;; User message
          (insert "** You\n")
          (insert user)
          (insert "\n\n")
          ;; Thinking
          (when (and thinking (not (string-empty-p thinking)))
            (insert "** Thinking\n")
            (insert ":PROPERTIES:\n")
            (insert ":VISIBILITY: folded\n")
            (insert ":END:\n")
            (insert "#+begin_example\n")
            (insert thinking)
            (insert "\n#+end_example\n\n"))
          ;; Tools
          (when tools
            (dolist (tool tools)
              (let ((name (plist-get tool :name))
                    (input (plist-get tool :input))
                    (result (plist-get tool :result))
                    (is-error (plist-get tool :is-error)))
                (insert (format "** Tool: %s%s\n"
                                name
                                (if is-error " [ERROR]" "")))
                (when input
                  (insert "*** Input\n")
                  (insert "#+begin_src json\n")
                  (insert (claude-cli-chat-export--format-json input))
                  (insert "\n#+end_src\n\n"))
                (when result
                  (insert "*** Result\n")
                  (insert "#+begin_example\n")
                  (insert (format "%s" result))
                  (insert "\n#+end_example\n\n")))))
          ;; Assistant
          (insert "** Claude\n")
          (insert (or assistant ""))
          (insert "\n\n"))))
    (buffer-string)))

;;; Export to JSON

(defun claude-cli-chat-export-json ()
  "Export conversation as JSON to a file."
  (interactive)
  (let ((content (claude-cli-chat-export--to-json))
        (file (read-file-name "Export to file: " nil nil nil
                              (format "claude-cli-chat-%s.json"
                                      (format-time-string "%Y%m%d-%H%M%S")))))
    (with-temp-file file
      (insert content))
    (message "Exported to %s" file)))

(defun claude-cli-chat-export--to-json ()
  "Convert conversation history to JSON format."
  (let ((data `((metadata . ((model . ,(or (claude-cli-chat--current-model) "unknown"))
                             (date . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                             (total_input_tokens . ,(or claude-cli-chat--total-input-tokens 0))
                             (total_output_tokens . ,(or claude-cli-chat--total-output-tokens 0))
                             (total_cost_usd . ,(or claude-cli-chat--total-cost 0.0))))
                (turns . ,(vconcat
                           (mapcar
                            (lambda (turn)
                              `((turn_number . ,(plist-get turn :number))
                                (user_message . ,(plist-get turn :user-content))
                                (assistant_response . ,(plist-get turn :assistant-content))
                                (thinking . ,(or (plist-get turn :thinking) ""))
                                (tools . ,(vconcat
                                           (mapcar
                                            (lambda (tool)
                                              `((name . ,(plist-get tool :name))
                                                (id . ,(plist-get tool :id))
                                                (input . ,(plist-get tool :input))
                                                (result . ,(format "%s" (plist-get tool :result)))
                                                (is_error . ,(if (plist-get tool :is-error) t :json-false))))
                                            (or (plist-get turn :tools) nil))))))
                            (reverse claude-cli-chat--conversation-history)))))))
    (with-temp-buffer
      (insert (json-encode data))
      (json-pretty-print-buffer)
      (buffer-string))))

;;; Export to Plain Text

(defun claude-cli-chat-export-text ()
  "Export conversation as plain text to a file."
  (interactive)
  (let ((content (claude-cli-chat-export--to-text))
        (file (read-file-name "Export to file: " nil nil nil
                              (format "claude-cli-chat-%s.txt"
                                      (format-time-string "%Y%m%d-%H%M%S")))))
    (with-temp-file file
      (insert content))
    (message "Exported to %s" file)))

(defun claude-cli-chat-export--to-text ()
  "Convert conversation history to plain text format."
  (with-temp-buffer
    (insert "CLAUDE CHAT EXPORT\n")
    (insert (make-string 60 ?=))
    (insert "\n\n")
    (insert (format "Model: %s\n" (or (claude-cli-chat--current-model) "Unknown")))
    (insert (format "Date: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (format "Tokens: %d input / %d output\n"
                    (or claude-cli-chat--total-input-tokens 0)
                    (or claude-cli-chat--total-output-tokens 0)))
    (insert (format "Cost: $%.4f\n" (or claude-cli-chat--total-cost 0.0)))
    (insert "\n")
    (insert (make-string 60 ?=))
    (insert "\n\n")
    ;; Export each turn
    (let ((turns (reverse claude-cli-chat--conversation-history)))
      (dolist (turn turns)
        (let ((num (plist-get turn :number))
              (user (plist-get turn :user-content))
              (assistant (plist-get turn :assistant-content))
              (thinking (plist-get turn :thinking))
              (tools (plist-get turn :tools)))
          (insert (format "--- Turn %d " num))
          (insert (make-string (- 50 (length (format "--- Turn %d " num))) ?-))
          (insert "\n\n")
          ;; User
          (insert "YOU:\n")
          (insert user)
          (insert "\n\n")
          ;; Thinking
          (when (and thinking (not (string-empty-p thinking)))
            (insert "[THINKING]\n")
            (insert thinking)
            (insert "\n\n"))
          ;; Tools
          (when tools
            (dolist (tool tools)
              (let ((name (plist-get tool :name))
                    (result (plist-get tool :result))
                    (is-error (plist-get tool :is-error)))
                (insert (format "[TOOL: %s%s]\n"
                                name
                                (if is-error " - ERROR" "")))
                (when result
                  (insert (format "%s" result))
                  (insert "\n"))
                (insert "\n"))))
          ;; Assistant
          (insert "CLAUDE:\n")
          (insert (or assistant ""))
          (insert "\n\n"))))
    (buffer-string)))

;;; Helper Functions

(defun claude-cli-chat-export--format-json (data)
  "Format DATA as indented JSON string."
  (if (null data)
      "{}"
    (condition-case nil
        (with-temp-buffer
          (insert (json-encode data))
          (json-pretty-print-buffer)
          (buffer-string))
      (error (format "%S" data)))))

;;; Quick Export Functions

(defun claude-cli-chat-export-to-buffer ()
  "Export conversation to a new buffer in Markdown format."
  (interactive)
  (let ((content (claude-cli-chat-export--to-markdown)))
    (with-current-buffer (get-buffer-create "*Claude Chat Export*")
      (erase-buffer)
      (insert content)
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (goto-char (point-min)))
    (pop-to-buffer "*Claude Chat Export*")))

(defun claude-cli-chat-export-to-kill-ring ()
  "Copy conversation to kill ring as plain text."
  (interactive)
  (let ((content (claude-cli-chat-export--to-text)))
    (kill-new content)
    (message "Conversation copied to kill ring (%d characters)" (length content))))

(defun claude-cli-chat-export-last-response ()
  "Copy the last assistant response to kill ring."
  (interactive)
  (if claude-cli-chat--conversation-history
      (let* ((last-turn (car claude-cli-chat--conversation-history))
             (response (plist-get last-turn :assistant-content)))
        (kill-new response)
        (message "Last response copied (%d characters)" (length response)))
    (message "No conversation history")))

(provide 'claude-cli-chat-export)
;;; claude-cli-chat-export.el ends here
