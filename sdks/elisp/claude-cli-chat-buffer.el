;;; claude-cli-chat-buffer.el --- Buffer management for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file handles buffer structure, rendering, and event handling
;; for the Claude Chat interface.

;;; Code:

(require 'cl-lib)
(require 'claude-cli-events)
(require 'claude-cli-chat-faces)

;; Forward declarations
(declare-function claude-cli-chat-sections--insert-turn-start "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--insert-user "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--insert-assistant-start "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--append-assistant-text "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--finalize-assistant "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--insert-thinking "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--append-thinking "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--insert-tool-start "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--update-tool-input "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--update-tool-result "claude-cli-chat-sections")
(declare-function claude-cli-chat-sections--insert-error "claude-cli-chat-sections")
(declare-function claude-cli-chat-input--setup "claude-cli-chat-input")
(declare-function claude-cli-chat-input--new-prompt "claude-cli-chat-input")
(declare-function claude-cli-chat-question--display "claude-cli-chat-question")
(declare-function claude-cli-chat-question--pending-p "claude-cli-chat-question")
(declare-function claude-cli-chat-question--submit "claude-cli-chat-question")
(declare-function claude-cli-send-tool-result "claude-cli")

;; Buffer-local variable declarations (defined in claude-cli-chat.el)
(defvar claude-cli-chat--session)
(defvar claude-cli-chat--turn-number)
(defvar claude-cli-chat--total-input-tokens)
(defvar claude-cli-chat--total-output-tokens)
(defvar claude-cli-chat--total-cost)
(defvar claude-cli-chat--conversation-history)
(defvar claude-cli-chat--conversation-start)
(defvar claude-cli-chat--conversation-end)
(defvar claude-cli-chat--input-start)
(defvar claude-cli-chat--streaming-marker)
(defvar claude-cli-chat--current-turn)

;;; Buffer Setup

(defmacro claude-cli-chat-buffer--with-writable (&rest body)
  "Execute BODY with buffer writable."
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     ,@body))

(defun claude-cli-chat-buffer--setup ()
  "Initialize buffer structure with conversation and input areas."
  (claude-cli-chat-buffer--with-writable
    (erase-buffer)
    ;; Insert welcome message
    (insert (propertize "Claude Chat\n" 'face '(:weight bold :height 1.3)))
    (insert (propertize "Type your message below and press C-c C-c to send.\n"
                        'face 'font-lock-comment-face))
    (insert (propertize "Press ? for help and commands.\n\n"
                        'face 'font-lock-comment-face))
    ;; Set conversation start marker
    (setq claude-cli-chat--conversation-start (point-marker))
    (set-marker-insertion-type claude-cli-chat--conversation-start nil)
    ;; Set conversation end marker (will be updated as content is added)
    (setq claude-cli-chat--conversation-end (point-marker))
    (set-marker-insertion-type claude-cli-chat--conversation-end t)
    ;; Set up input area
    (require 'claude-cli-chat-input)
    (claude-cli-chat-input--setup)))

;;; Turn Insertion

(defun claude-cli-chat-buffer--insert-user-turn (turn-number content)
  "Insert user CONTENT, ready to stream response.
Minimal display: just user message, then response streams in."
  (claude-cli-chat-buffer--with-writable
    (save-excursion
      (goto-char claude-cli-chat--conversation-end)
      ;; Blank line between turns (but not before first)
      (when (> turn-number 1)
        (insert "\n"))
      ;; Insert user message with styling
      (let ((msg-start (point)))
        (insert content)
        (put-text-property msg-start (point) 'face 'claude-cli-chat-user-content))
      (insert "\n")
      ;; Start response inline - set marker here, response streams in at end
      ;; The 🔄 spinner will be at the end of the response as it streams
      (setq claude-cli-chat--streaming-marker (point-marker))
      (set-marker-insertion-type claude-cli-chat--streaming-marker t)
      ;; Add initial spinner
      (insert (propertize "🔄" 'claude-cli-chat-progress-indicator t
                          'face 'claude-cli-chat-assistant-content))
      ;; Update conversation end
      (set-marker claude-cli-chat--conversation-end (point)))))

;;; Event Handlers

(defun claude-cli-chat-buffer--handle-text (event)
  "Handle streaming text EVENT.
Insert text before spinner, keeping spinner at the end."
  (when claude-cli-chat--streaming-marker
    (let ((text (claude-cli-text-event-text event)))
      (claude-cli-chat-buffer--with-writable
        (save-excursion
          ;; Find the spinner (last character before streaming marker)
          (goto-char (- claude-cli-chat--streaming-marker 1))
          ;; Delete the spinner temporarily
          (when (looking-at "🔄")
            (delete-char 1))
          ;; Insert the new text
          (insert (propertize text 'face 'claude-cli-chat-assistant-content))
          ;; Re-insert spinner at the end
          (insert (propertize "🔄" 'claude-cli-chat-progress-indicator t
                              'face 'claude-cli-chat-assistant-content))
          ;; Update current turn tracking
          (when claude-cli-chat--current-turn
            (plist-put claude-cli-chat--current-turn :assistant-content
                       (concat (plist-get claude-cli-chat--current-turn :assistant-content)
                               text)))))
      ;; Update conversation end marker
      (set-marker claude-cli-chat--conversation-end
                  (marker-position claude-cli-chat--streaming-marker)))))

(defvar-local claude-cli-chat--thinking-marker nil
  "Marker for thinking section insertion.")

(defvar-local claude-cli-chat--thinking-started nil
  "Whether thinking section has been started for current turn.")

(defun claude-cli-chat-buffer--handle-thinking (event)
  "Handle thinking EVENT."
  (when claude-cli-chat--streaming-marker
    (let ((thinking (claude-cli-thinking-event-thinking event)))
      (claude-cli-chat-buffer--with-writable
        (save-excursion
          ;; If thinking not started, insert thinking header
          (unless claude-cli-chat--thinking-started
            (goto-char claude-cli-chat--streaming-marker)
            ;; Insert before the streaming marker
            (insert "\n")
            (insert (propertize "  Thinking" 'face 'claude-cli-chat-thinking-label))
            (insert (propertize " (collapsed by default)" 'face 'claude-cli-chat-thinking-meta))
            (insert "\n")
            (setq claude-cli-chat--thinking-marker (point-marker))
            (set-marker-insertion-type claude-cli-chat--thinking-marker t)
            (insert "\n")
            (setq claude-cli-chat--thinking-started t))
          ;; Append thinking content
          (goto-char claude-cli-chat--thinking-marker)
          (insert (propertize thinking 'face 'claude-cli-chat-thinking-content))
          ;; Update current turn tracking
          (when claude-cli-chat--current-turn
            (plist-put claude-cli-chat--current-turn :thinking
                       (concat (plist-get claude-cli-chat--current-turn :thinking)
                               thinking))))))))

(defvar-local claude-cli-chat--tool-markers (make-hash-table :test 'equal)
  "Hash table mapping tool IDs to their insertion markers.")

(defun claude-cli-chat-buffer--handle-tool-start (event)
  "Handle tool start EVENT."
  (let ((tool-id (claude-cli-tool-start-event-id event))
        (tool-name (claude-cli-tool-start-event-name event)))
    (claude-cli-chat-buffer--with-writable
      (save-excursion
        (goto-char claude-cli-chat--streaming-marker)
        ;; Insert tool section
        (insert "\n")
        (insert (propertize "  Tool: " 'face 'claude-cli-chat-tool-label))
        (insert (propertize tool-name 'face 'claude-cli-chat-tool-name))
        (insert " ")
        (insert (propertize "[Running...]" 'face 'claude-cli-chat-tool-pending))
        (insert "\n")
        ;; Create marker for this tool
        (let ((marker (point-marker)))
          (set-marker-insertion-type marker t)
          (puthash tool-id marker claude-cli-chat--tool-markers))
        (insert "\n")))))

(defun claude-cli-chat-buffer--handle-tool-complete (event)
  "Handle tool complete EVENT (input fully received)."
  (let* ((tool-id (claude-cli-tool-complete-event-id event))
         (tool-name (claude-cli-tool-complete-event-name event))
         (input (claude-cli-tool-complete-event-input event))
         (marker (gethash tool-id claude-cli-chat--tool-markers)))
    ;; Check if this is AskUserQuestion - needs special handling
    (if (string= tool-name "AskUserQuestion")
        (claude-cli-chat-buffer--handle-ask-user-question tool-id input)
      ;; Regular tool - just show input
      (when marker
        (claude-cli-chat-buffer--with-writable
          (save-excursion
            (goto-char marker)
            ;; Insert formatted input
            (insert (propertize "    Input: " 'face 'claude-cli-chat-tool-sublabel))
            (insert (propertize (claude-cli-chat-buffer--format-json input)
                                'face 'claude-cli-chat-tool-json))
            (insert "\n")))))))

(defun claude-cli-chat-buffer--handle-tool-result (event)
  "Handle tool result EVENT."
  (let* ((tool-id (claude-cli-tool-result-event-tool-use-id event))
         (tool-name (claude-cli-tool-result-event-tool-name event))
         (content (claude-cli-tool-result-event-content event))
         (is-error (claude-cli-tool-result-event-is-error event))
         (marker (gethash tool-id claude-cli-chat--tool-markers)))
    (when marker
      (claude-cli-chat-buffer--with-writable
        (save-excursion
          (goto-char marker)
          ;; Insert result
          (insert (propertize "    Result: " 'face 'claude-cli-chat-tool-sublabel))
          (if is-error
              (insert (propertize (format "%s" content) 'face 'claude-cli-chat-tool-error))
            (insert (propertize (claude-cli-chat-buffer--truncate-result content)
                                'face 'claude-cli-chat-assistant-content)))
          (insert "\n")
          ;; Update status
          (save-excursion
            ;; Find and update the [Running...] status
            (when (re-search-backward "\\[Running\\.\\.\\.\\]" nil t)
              (replace-match (if is-error "[Error]" "[Done]"))
              (put-text-property (match-beginning 0) (match-end 0)
                                 'face (if is-error
                                           'claude-cli-chat-tool-error
                                         'claude-cli-chat-tool-success)))))
        ;; Track tool in current turn
        (when claude-cli-chat--current-turn
          (let ((tools (plist-get claude-cli-chat--current-turn :tools)))
            (plist-put claude-cli-chat--current-turn :tools
                       (append tools
                               (list (list :id tool-id
                                           :name tool-name
                                           :input nil ; TODO: track from complete event
                                           :result content
                                           :is-error is-error))))))))))

(defun claude-cli-chat-buffer--handle-turn-complete (event)
  "Handle turn complete EVENT."
  (let ((usage (claude-cli-turn-complete-event-usage event)))
    ;; Update token counts
    (when usage
      (cl-incf claude-cli-chat--total-input-tokens
               (or (claude-cli-turn-usage-input-tokens usage) 0))
      (cl-incf claude-cli-chat--total-output-tokens
               (or (claude-cli-turn-usage-output-tokens usage) 0))
      (cl-incf claude-cli-chat--total-cost
               (or (claude-cli-turn-usage-cost-usd usage) 0.0)))
    ;; Finalize current turn - remove spinner and add newline
    (claude-cli-chat-buffer--with-writable
      (save-excursion
        (goto-char claude-cli-chat--streaming-marker)
        ;; Remove the spinner at the end
        (backward-char 1)
        (when (looking-at "🔄")
          (delete-char 1))
        (insert "\n")))
    ;; Save to history
    (when claude-cli-chat--current-turn
      (plist-put claude-cli-chat--current-turn :usage usage)
      (push claude-cli-chat--current-turn claude-cli-chat--conversation-history))
    ;; Reset turn state
    (setq claude-cli-chat--current-turn nil
          claude-cli-chat--streaming-marker nil
          claude-cli-chat--thinking-marker nil
          claude-cli-chat--thinking-started nil)
    (clrhash claude-cli-chat--tool-markers)
    ;; Insert new prompt for next message
    (require 'claude-cli-chat-input)
    (claude-cli-chat-input--insert-prompt)
    ;; Update mode line
    (force-mode-line-update)))

(defun claude-cli-chat-buffer--handle-error (event)
  "Handle error EVENT."
  (let ((error-msg (claude-cli-error-event-error event))
        (context (claude-cli-error-event-context event)))
    (claude-cli-chat-buffer--with-writable
      (save-excursion
        (goto-char (or claude-cli-chat--streaming-marker
                       claude-cli-chat--conversation-end))
        (insert "\n")
        (insert (propertize "Error: " 'face 'claude-cli-chat-error-label))
        (insert (propertize (format "%s" error-msg) 'face 'claude-cli-chat-error-content))
        (when context
          (insert "\n")
          (insert (propertize (format "Context: %s" context)
                              'face 'claude-cli-chat-error-context)))
        (insert "\n\n")))
    (message "Claude error: %s" error-msg)))

(defun claude-cli-chat-buffer--handle-state-change (event)
  "Handle state change EVENT."
  (force-mode-line-update)
  (let ((to (claude-cli-state-change-event-to event)))
    (when (eq to 'closed)
      (message "Session closed"))))

(defun claude-cli-chat-buffer--handle-ready (event)
  "Handle ready EVENT."
  (force-mode-line-update)
  (message "Session ready (model: %s)"
           (claude-cli-ready-event-model event)))

;;; AskUserQuestion Handling

(defun claude-cli-chat-buffer--handle-ask-user-question (tool-use-id input)
  "Handle AskUserQuestion tool with TOOL-USE-ID and INPUT."
  (let ((questions (or (plist-get input :questions)
                       (cdr (assq 'questions input)))))
    (when questions
      ;; Display question in buffer
      (claude-cli-chat-buffer--with-writable
        (save-excursion
          (goto-char claude-cli-chat--streaming-marker)
          (insert "\n")
          (insert (propertize "❓ Claude has a question:\n\n"
                              'face '(:inherit font-lock-warning-face :weight bold)))))
      ;; Build prompt with options
      (let ((prompt-parts nil)
            (options-alist nil)
            (option-index 1))
        (dolist (q questions)
          (let ((question-text (or (plist-get q :question)
                                   (cdr (assq 'question q))))
                (options (or (plist-get q :options)
                             (cdr (assq 'options q)))))
            ;; Display question in buffer
            (claude-cli-chat-buffer--with-writable
              (save-excursion
                (goto-char claude-cli-chat--streaming-marker)
                (insert (propertize question-text 'face '(:weight bold)))
                (insert "\n\n")))
            ;; Build options
            (when options
              (dolist (opt (append options nil))
                (claude-cli-chat-buffer--with-writable
                  (save-excursion
                    (goto-char claude-cli-chat--streaming-marker)
                    (insert (propertize (format "  [%d] " option-index)
                                        'face 'font-lock-keyword-face))
                    (insert (propertize (format "%s\n" opt)
                                        'face 'font-lock-string-face))))
                (push (cons (number-to-string option-index) opt) options-alist)
                (cl-incf option-index)))
            (push question-text prompt-parts)))
        ;; Prompt user
        (let* ((prompt (format "%s\nEnter number or type response: "
                               (string-join (nreverse prompt-parts) "\n")))
               (response (read-string prompt))
               (answer (or (cdr (assoc response options-alist))
                           response)))
          ;; Show answer in buffer
          (claude-cli-chat-buffer--with-writable
            (save-excursion
              (goto-char claude-cli-chat--streaming-marker)
              (insert "\n")
              (insert (propertize "Your answer: " 'face 'font-lock-comment-face))
              (insert (propertize answer 'face 'font-lock-string-face))
              (insert "\n\n")))
          ;; Send tool result
          (claude-cli-send-tool-result
           claude-cli-chat--session
           tool-use-id
           answer))))))

;;; Formatting Helpers

(defun claude-cli-chat-buffer--format-json (data)
  "Format DATA as indented JSON string."
  (if (null data)
      "{}"
    (condition-case nil
        (with-temp-buffer
          (insert (json-encode data))
          (json-pretty-print-buffer)
          (buffer-string))
      (error (format "%S" data)))))

(defun claude-cli-chat-buffer--truncate-result (result &optional max-lines)
  "Truncate RESULT to MAX-LINES for display."
  (let* ((max-lines (or max-lines 20))
         (result-str (format "%s" result))
         (lines (split-string result-str "\n")))
    (if (> (length lines) max-lines)
        (concat (string-join (seq-take lines max-lines) "\n")
                (propertize (format "\n... (%d more lines)"
                                    (- (length lines) max-lines))
                            'face 'claude-cli-chat-truncated))
      result-str)))

(provide 'claude-cli-chat-buffer)
;;; claude-cli-chat-buffer.el ends here
