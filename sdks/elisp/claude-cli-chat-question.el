;;; claude-cli-chat-question.el --- AskUserQuestion UI for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file provides a UI for handling the AskUserQuestion tool in Claude Chat.
;; When Claude needs to ask the user a question with predefined options,
;; this module displays the question and collects the user's response.
;;
;; The AskUserQuestion tool input format:
;; {"questions": [{"question": "...", "options": ["opt1", "opt2", ...]}]}
;;
;; The user can select a numbered option or type a custom response.
;; The response is sent back as a tool result.

;;; Code:

(require 'cl-lib)

;;; Forward declarations

(declare-function claude-cli-chat-buffer--with-writable "claude-cli-chat-buffer")

;;; Customization

(defgroup claude-cli-chat-question nil
  "AskUserQuestion UI for Claude Chat."
  :group 'claude-cli-chat
  :prefix "claude-cli-chat-question-")

;;; Buffer-local Variables

(defvar-local claude-cli-chat-question--pending nil
  "Pending question request: plist with :tool-use-id, :questions.")

(defvar-local claude-cli-chat-question--response-marker nil
  "Marker for response input area.")

(defvar-local claude-cli-chat-question--options nil
  "Current question options for selection.")

;;; Faces

(defface claude-cli-chat-question-header
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for question header."
  :group 'claude-cli-chat-question)

(defface claude-cli-chat-question-text
  '((t :inherit default :weight bold))
  "Face for question text."
  :group 'claude-cli-chat-question)

(defface claude-cli-chat-question-option
  '((t :inherit font-lock-string-face))
  "Face for question options."
  :group 'claude-cli-chat-question)

(defface claude-cli-chat-question-option-number
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for option numbers."
  :group 'claude-cli-chat-question)

(defface claude-cli-chat-question-prompt
  '((t :inherit font-lock-comment-face))
  "Face for input prompt."
  :group 'claude-cli-chat-question)

;;; Question Display

(defun claude-cli-chat-question--display (tool-use-id questions streaming-marker)
  "Display QUESTIONS with TOOL-USE-ID at STREAMING-MARKER.
Returns the marker where user input should be collected."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char streaming-marker)
      (insert "\n")
      ;; Header
      (insert (propertize "❓ Claude has a question:\n"
                          'face 'claude-cli-chat-question-header))
      ;; Process each question
      (let ((all-options nil)
            (option-index 1))
        (dolist (q questions)
          (let ((question-text (or (plist-get q :question)
                                   (cdr (assq 'question q))))
                (options (or (plist-get q :options)
                             (cdr (assq 'options q)))))
            ;; Question text
            (insert "\n")
            (insert (propertize question-text 'face 'claude-cli-chat-question-text))
            (insert "\n\n")
            ;; Options
            (when options
              (dolist (opt (append options nil))  ; Convert vector to list if needed
                (insert (propertize (format "  [%d] " option-index)
                                    'face 'claude-cli-chat-question-option-number))
                (insert (propertize (format "%s\n" opt)
                                    'face 'claude-cli-chat-question-option))
                (push (cons option-index opt) all-options)
                (cl-incf option-index)))))
        ;; Store options for lookup
        (setq claude-cli-chat-question--options (nreverse all-options)))
      ;; Input prompt
      (insert "\n")
      (insert (propertize "Enter number or type your response: "
                          'face 'claude-cli-chat-question-prompt))
      ;; Create marker for response collection
      (setq claude-cli-chat-question--response-marker (point-marker))
      (set-marker-insertion-type claude-cli-chat-question--response-marker t)
      (insert "\n")
      ;; Store pending request
      (setq claude-cli-chat-question--pending
            (list :tool-use-id tool-use-id
                  :questions questions))
      claude-cli-chat-question--response-marker)))

;;; Response Collection

(defun claude-cli-chat-question--get-response ()
  "Get the user's response text from the question area."
  (when claude-cli-chat-question--response-marker
    (let ((start claude-cli-chat-question--response-marker)
          (end (save-excursion
                 (goto-char claude-cli-chat-question--response-marker)
                 (line-end-position))))
      (string-trim (buffer-substring-no-properties start end)))))

(defun claude-cli-chat-question--parse-response (response)
  "Parse RESPONSE into final answer text.
If RESPONSE is a number, return the corresponding option.
Otherwise return RESPONSE as-is."
  (let ((num (string-to-number response)))
    (if (and (> num 0) (assq num claude-cli-chat-question--options))
        (cdr (assq num claude-cli-chat-question--options))
      response)))

(defun claude-cli-chat-question--submit ()
  "Submit the current question response.
Returns the tool result content to send back, or nil if no pending question."
  (when claude-cli-chat-question--pending
    (let* ((response (claude-cli-chat-question--get-response))
           (answer (claude-cli-chat-question--parse-response response))
           (tool-use-id (plist-get claude-cli-chat-question--pending :tool-use-id)))
      ;; Clear pending state
      (setq claude-cli-chat-question--pending nil
            claude-cli-chat-question--response-marker nil
            claude-cli-chat-question--options nil)
      ;; Return the result to send
      (list :tool-use-id tool-use-id
            :content answer))))

(defun claude-cli-chat-question--pending-p ()
  "Return non-nil if there's a pending question."
  (and (boundp 'claude-cli-chat-question--pending)
       claude-cli-chat-question--pending))

(defun claude-cli-chat-question--cancel ()
  "Cancel the pending question."
  (setq claude-cli-chat-question--pending nil
        claude-cli-chat-question--response-marker nil
        claude-cli-chat-question--options nil))

;;; Integration with Chat

(defun claude-cli-chat-question-handle-tool (tool-use-id tool-name input streaming-marker)
  "Handle AskUserQuestion TOOL-NAME with INPUT at STREAMING-MARKER.
TOOL-USE-ID is used for sending the response.
Returns non-nil if this was an AskUserQuestion tool."
  (when (string= tool-name "AskUserQuestion")
    (let ((questions (or (plist-get input :questions)
                         (cdr (assq 'questions input)))))
      (when questions
        (claude-cli-chat-question--display tool-use-id questions streaming-marker)
        t))))

(provide 'claude-cli-chat-question)
;;; claude-cli-chat-question.el ends here
