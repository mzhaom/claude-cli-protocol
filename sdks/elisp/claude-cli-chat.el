;;; claude-cli-chat.el --- Interactive chat interface for Claude CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Chat Contributors
;; Keywords: tools, ai, chat
;; Package-Requires: ((emacs "27.1") (magit-section "3.0") (transient "0.4"))
;; Version: 0.1.0

;;; Commentary:

;; This package provides an interactive Emacs interface for chatting with Claude.
;; Uses an eshell-style design: conversation grows upward, input prompt at bottom.
;;
;; Features:
;; - Real-time streaming responses
;; - Collapsible sections for turns, thinking, and tool use
;; - Model and permission mode switching (C-c C-t)
;; - Token and cost tracking in header
;; - Full message history navigation (M-p/M-n)
;;
;; Start:
;;   M-x claude-cli-chat
;;
;; Key bindings (all C-c prefix to avoid text input interference):
;;   C-c C-c  Send message (from input area or anywhere)
;;   C-c C-k  Interrupt current operation
;;   C-c C-n  Start new session
;;   C-c C-q  Close session
;;   C-c C-t  Open menu (switch model, permission mode, etc.)
;;   C-c C-i  Focus input area
;;
;; In input area:
;;   RET      Insert newline (multi-line support)
;;   M-p/M-n  Browse message history
;;   All text editing works normally (no single-letter command keys)

;;; Code:

(require 'cl-lib)
(require 'json)

;; Load SDK components
(require 'claude-cli)
(require 'claude-cli-events)
(require 'claude-cli-state)
(require 'claude-cli-permission)

;; Load UI components
(require 'claude-cli-chat-faces)

;; Forward declarations for components loaded later
(declare-function claude-cli-chat-buffer--setup "claude-cli-chat-buffer")
(declare-function claude-cli-chat-buffer--handle-text "claude-cli-chat-buffer")
(declare-function claude-cli-chat-buffer--handle-thinking "claude-cli-chat-buffer")
(declare-function claude-cli-chat-buffer--handle-tool-start "claude-cli-chat-buffer")
(declare-function claude-cli-chat-buffer--handle-tool-complete "claude-cli-chat-buffer")
(declare-function claude-cli-chat-buffer--handle-tool-result "claude-cli-chat-buffer")
(declare-function claude-cli-chat-buffer--handle-turn-complete "claude-cli-chat-buffer")
(declare-function claude-cli-chat-buffer--handle-error "claude-cli-chat-buffer")
(declare-function claude-cli-chat-buffer--handle-state-change "claude-cli-chat-buffer")
(declare-function claude-cli-chat-buffer--handle-ready "claude-cli-chat-buffer")
(declare-function claude-cli-chat-input--setup "claude-cli-chat-input")
(declare-function claude-cli-chat-input--get-content "claude-cli-chat-input")
(declare-function claude-cli-chat-input--clear "claude-cli-chat-input")
(declare-function claude-cli-chat-input--focus "claude-cli-chat-input")
(declare-function claude-cli-chat-input--in-input-p "claude-cli-chat-input")
(declare-function claude-cli-chat-input--insert-prompt "claude-cli-chat-input")
(declare-function claude-cli-chat-input--add-to-history "claude-cli-chat-input")
(declare-function claude-cli-chat-buffer--insert-user-turn "claude-cli-chat-buffer")
(declare-function claude-cli-chat-sections--insert-turn "claude-cli-chat-sections")
(declare-function claude-cli-chat-transient-menu "claude-cli-chat-transient")
(declare-function claude-cli-chat-permission-handler "claude-cli-chat-permission")
;; Navigation functions - autoloaded from claude-cli-chat-navigation
(autoload 'claude-cli-chat-navigation-next-turn "claude-cli-chat-navigation")
(autoload 'claude-cli-chat-navigation-previous-turn "claude-cli-chat-navigation")
(autoload 'claude-cli-chat-navigation-next-section "claude-cli-chat-navigation")
(autoload 'claude-cli-chat-navigation-previous-section "claude-cli-chat-navigation")
(autoload 'claude-cli-chat-navigation-toggle-section "claude-cli-chat-navigation")
(autoload 'claude-cli-chat-navigation-section-up "claude-cli-chat-navigation")

;;; Customization

(defgroup claude-cli-chat nil
  "Interactive chat interface for Claude CLI."
  :group 'tools
  :prefix "claude-cli-chat-")

(defcustom claude-cli-chat-default-model "sonnet"
  "Default model to use for new chat sessions."
  :type 'string
  :group 'claude-cli-chat)

(defcustom claude-cli-chat-default-permission-mode 'default
  "Default permission mode for new chat sessions.
One of: `default', `accept-edits', `plan', `bypass'."
  :type '(choice (const :tag "Default (ask for dangerous)" default)
                 (const :tag "Accept edits" accept-edits)
                 (const :tag "Plan mode (read-only)" plan)
                 (const :tag "Bypass (approve all)" bypass))
  :group 'claude-cli-chat)

(defcustom claude-cli-chat-buffer-name "*Claude Chat*"
  "Name for the Claude chat buffer."
  :type 'string
  :group 'claude-cli-chat)

;;; Buffer-local Variables

(defvar-local claude-cli-chat--session nil
  "The claude-cli session for this chat buffer.")

(defvar-local claude-cli-chat--turn-number 0
  "Current conversation turn number.")

(defvar-local claude-cli-chat--total-input-tokens 0
  "Total input tokens used in this session.")

(defvar-local claude-cli-chat--total-output-tokens 0
  "Total output tokens generated in this session.")

(defvar-local claude-cli-chat--total-cost 0.0
  "Total cost in USD for this session.")

(defvar-local claude-cli-chat--conversation-history nil
  "List of conversation turns for export.
Each turn is a plist with :number, :user-content, :assistant-content,
:thinking, :tools, :usage.")

(defvar-local claude-cli-chat--conversation-start nil
  "Marker for start of conversation area.")

(defvar-local claude-cli-chat--conversation-end nil
  "Marker for end of conversation area.")

(defvar-local claude-cli-chat--input-start nil
  "Marker for start of input area.")

(defvar-local claude-cli-chat--streaming-marker nil
  "Marker for current streaming insertion point.")

(defvar-local claude-cli-chat--current-turn nil
  "Current turn being processed (plist).")

;; Hook functions for cleanup - stored in a list for easier management
(defvar-local claude-cli-chat--hook-functions nil
  "List of (hook-var . fn) pairs for cleanup on buffer kill.")

;;; Keymap

(defvar claude-cli-chat-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Only C-c prefix bindings - no single letter keys
    ;; This allows normal text editing in the input area
    (define-key map (kbd "C-c C-c") #'claude-cli-chat-send)
    (define-key map (kbd "C-c C-k") #'claude-cli-chat-interrupt)
    (define-key map (kbd "C-c C-n") #'claude-cli-chat-new-session)
    (define-key map (kbd "C-c C-q") #'claude-cli-chat-close-session)
    (define-key map (kbd "C-c C-t") #'claude-cli-chat-menu)
    (define-key map (kbd "C-c C-i") #'claude-cli-chat-focus-input)
    map)
  "Keymap for `claude-cli-chat-mode'.
Only uses C-c prefix bindings to avoid interfering with text input.")

;;; Header Line

(defun claude-cli-chat--header-line ()
  "Construct header line showing session info (model, mode, tokens, cost)."
  (format " Model: %s | Mode: %s | Tokens: %d/%d | Cost: $%.4f"
          (or (claude-cli-chat--current-model) "N/A")
          (or (claude-cli-chat--permission-mode) "N/A")
          claude-cli-chat--total-input-tokens
          claude-cli-chat--total-output-tokens
          claude-cli-chat--total-cost))

;;; Mode Line

(defun claude-cli-chat--mode-line-format ()
  "Construct mode line format for claude-cli-chat-mode."
  `("%e"
    mode-line-front-space
    mode-line-buffer-identification
    "   "
    (:eval (format "Turn %d" claude-cli-chat--turn-number))
    "   "
    (:eval (pcase (claude-cli-chat--session-state)
             ('processing "[Processing...]")
             ('ready "[Ready]")
             ('closed "[Closed]")
             (_ "")))
    mode-line-end-spaces))

;;; Helper Functions

(defun claude-cli-chat--session-info ()
  "Return session info plist or nil if no session."
  (when claude-cli-chat--session
    (claude-cli-session-get-info claude-cli-chat--session)))

(defun claude-cli-chat--current-model ()
  "Return current model name or nil."
  (when-let ((info (claude-cli-chat--session-info)))
    (claude-cli-session-info-model info)))

(defun claude-cli-chat--permission-mode ()
  "Return current permission mode or nil."
  (when-let ((info (claude-cli-chat--session-info)))
    (claude-cli-session-info-permission-mode info)))

(defun claude-cli-chat--session-state ()
  "Return current session state symbol."
  (if claude-cli-chat--session
      (claude-cli-session-get-state claude-cli-chat--session)
    'uninitialized))

(defun claude-cli-chat--processing-p ()
  "Return non-nil if session is currently processing."
  (when claude-cli-chat--session
    (claude-cli-session-processing-p claude-cli-chat--session)))

(defun claude-cli-chat--ready-p ()
  "Return non-nil if session is ready for input."
  (when claude-cli-chat--session
    (claude-cli-session-ready-p claude-cli-chat--session)))

;;; Major Mode

(define-derived-mode claude-cli-chat-mode text-mode "Claude-Chat"
  "Major mode for Claude AI chat interface.

Like eshell, this mode allows normal text editing. The conversation
area is made read-only via text properties, while the input area
at the bottom is editable.

\\{claude-cli-chat-mode-map}"
  :group 'claude-cli-chat
  ;; Set up header and mode line
  (setq header-line-format '(:eval (claude-cli-chat--header-line)))
  (setq mode-line-format (claude-cli-chat--mode-line-format))
  ;; Enable visual line mode for better text wrapping
  (visual-line-mode 1)
  ;; Set up kill buffer hook for cleanup
  (add-hook 'kill-buffer-hook #'claude-cli-chat--cleanup nil t))

;;; Session Management

(defun claude-cli-chat--make-event-handler (handler-name)
  "Create a buffer-safe event handler for HANDLER-NAME.
Returns a function that calls claude-cli-chat-buffer--handle-HANDLER-NAME
only if buffer is still live."
  (let ((buffer (current-buffer))
        (handler-sym (intern (format "claude-cli-chat-buffer--handle-%s" handler-name))))
    (lambda (event)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (funcall handler-sym event))))))

(defun claude-cli-chat--setup-hooks ()
  "Set up hooks for SDK events in current buffer."
  (setq claude-cli-chat--hook-functions nil)
  ;; Create and register handlers for each event type
  (dolist (event-type '(text thinking tool-start tool-complete tool-result turn-complete error state-change ready))
    (let* ((hook-name (format "claude-cli-%s-hook" event-type))
           (hook-var (intern hook-name))
           (fn (claude-cli-chat--make-event-handler (symbol-name event-type))))
      (push (cons hook-var fn) claude-cli-chat--hook-functions)
      (add-hook hook-var fn))))

(defun claude-cli-chat--teardown-hooks ()
  "Remove all registered SDK event hooks."
  (dolist (hook-pair claude-cli-chat--hook-functions)
    (remove-hook (car hook-pair) (cdr hook-pair)))
  (setq claude-cli-chat--hook-functions nil))

(defun claude-cli-chat--cleanup ()
  "Clean up when buffer is killed."
  (claude-cli-chat--teardown-hooks)
  (when (and claude-cli-chat--session
             (not (eq (claude-cli-session-get-state claude-cli-chat--session) 'closed)))
    (claude-cli-stop claude-cli-chat--session)))

(defun claude-cli-chat--create-session ()
  "Create and start a new Claude CLI session."
  (setq claude-cli-chat--session
        (claude-cli-create-session
         :model claude-cli-chat-default-model
         :permission-mode claude-cli-chat-default-permission-mode
         :permission-handler #'claude-cli-chat--permission-handler))
  (claude-cli-chat--setup-hooks)
  (claude-cli-start claude-cli-chat--session))

(defun claude-cli-chat--permission-handler (tool-name input)
  "Interactive permission handler for TOOL-NAME with INPUT.
Uses the enhanced permission UI with auto-allow/deny support."
  (require 'claude-cli-chat-permission)
  (claude-cli-chat-permission-handler tool-name input))

;;; Interactive Commands

;;;###autoload
(defun claude-cli-chat ()
  "Start a new Claude chat session.
If a chat buffer already exists, switch to it."
  (interactive)
  (let ((buffer (get-buffer-create claude-cli-chat-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'claude-cli-chat-mode)
        (claude-cli-chat-mode)
        ;; Load additional components
        (require 'claude-cli-chat-buffer)
        (require 'claude-cli-chat-sections)
        (require 'claude-cli-chat-input)
        (require 'claude-cli-chat-navigation)
        (require 'claude-cli-chat-transient)
        ;; Set up buffer structure
        (claude-cli-chat-buffer--setup)
        ;; Create session
        (claude-cli-chat--create-session)))
    (pop-to-buffer buffer)))

(defun claude-cli-chat-send ()
  "Send the current input to Claude.
Displays error message if input is empty or session not ready."
  (interactive)
  (require 'claude-cli-chat-input)

  ;; Check for empty input
  (let ((content (string-trim (claude-cli-chat-input--get-content))))
    (when (string-empty-p content)
      (user-error "Message cannot be empty")))

  ;; Check for active session
  (unless claude-cli-chat--session
    (user-error "No active session - use M-x claude-cli-chat to start"))

  ;; Check for session ready (allow starting state for first message)
  (let ((state (claude-cli-chat--session-state)))
    (unless (memq state '(ready starting))
      (user-error "Session not ready (state: %s)" state)))

  ;; OK to send - get content and send it
  (let ((content (string-trim (claude-cli-chat-input--get-content))))
    ;; Increment turn counter
    (cl-incf claude-cli-chat--turn-number)

    ;; Initialize turn tracking
    (setq claude-cli-chat--current-turn
          (list :number claude-cli-chat--turn-number
                :user-content content
                :assistant-content ""
                :thinking ""
                :tools nil))

    ;; Make the input message read-only and prepare for response
    (require 'claude-cli-chat-input)
    (let ((inhibit-read-only t))
      ;; Record where user message ends (before we add spinner)
      (let ((content-start (claude-cli-chat-input--content-start))
            (message-end (point-max)))
        ;; Position after user message
        (goto-char message-end)
        ;; Add newline and spinner for response
        (insert "\n")
        (setq claude-cli-chat--streaming-marker (point-marker))
        (set-marker-insertion-type claude-cli-chat--streaming-marker t)
        ;; Add initial spinner
        (insert (propertize "🔄" 'claude-cli-chat-progress-indicator t
                            'face 'claude-cli-chat-assistant-content))
        ;; Update conversation end
        (setq claude-cli-chat--conversation-end (point-marker))
        (set-marker-insertion-type claude-cli-chat--conversation-end t)
        ;; NOW mark the user message (before spinner) as read-only
        (when content-start
          (add-text-properties content-start message-end '(read-only t)))))

    ;; Add to input history for M-p/M-n navigation
    (claude-cli-chat-input--add-to-history content)

    ;; Send to SDK for processing
    (claude-cli-send-message claude-cli-chat--session content)))

(defun claude-cli-chat-interrupt ()
  "Interrupt the current Claude operation.
Only works if session is actively processing."
  (interactive)
  (unless claude-cli-chat--session
    (user-error "No active session"))
  (unless (claude-cli-chat--processing-p)
    (user-error "No operation in progress"))
  (claude-cli-interrupt claude-cli-chat--session)
  (message "Operation interrupted"))

(defun claude-cli-chat-new-session ()
  "Start a new chat session, clearing conversation history."
  (interactive)
  ;; Stop existing session if active
  (when claude-cli-chat--session
    (unless (eq (claude-cli-session-get-state claude-cli-chat--session) 'closed)
      (claude-cli-stop claude-cli-chat--session))
    (claude-cli-chat--teardown-hooks))

  ;; Reset all state
  (setq claude-cli-chat--turn-number 0
        claude-cli-chat--total-input-tokens 0
        claude-cli-chat--total-output-tokens 0
        claude-cli-chat--total-cost 0.0
        claude-cli-chat--conversation-history nil
        claude-cli-chat--current-turn nil
        claude-cli-chat--streaming-marker nil)

  ;; Clear buffer
  (let ((inhibit-read-only t))
    (erase-buffer))

  ;; Set up fresh buffer and session
  (require 'claude-cli-chat-buffer)
  (claude-cli-chat-buffer--setup)
  (claude-cli-chat--create-session)
  (message "New session started"))

(defun claude-cli-chat-close-session ()
  "Close the current chat session."
  (interactive)
  (unless claude-cli-chat--session
    (user-error "No active session"))
  (claude-cli-stop claude-cli-chat--session)
  (message "Session closed"))

(defun claude-cli-chat-focus-input ()
  "Move point to the input area."
  (interactive)
  (require 'claude-cli-chat-input)
  (claude-cli-chat-input--focus))

(defun claude-cli-chat-menu ()
  "Open the Claude chat transient menu."
  (interactive)
  (require 'claude-cli-chat-transient)
  (claude-cli-chat-transient-menu))

;;; Model and Permission Mode Commands

(defun claude-cli-chat-set-model (model)
  "Set the MODEL for the current session.
MODEL should be \"haiku\", \"sonnet\", \"opus\" or a full model ID.
Takes effect immediately for subsequent messages."
  (interactive
   (list (completing-read "Model: " '("haiku" "sonnet" "opus")
                          nil nil nil nil "sonnet")))
  (unless model
    (user-error "Model selection cancelled"))
  (if claude-cli-chat--session
      (progn
        (claude-cli-set-model claude-cli-chat--session model)
        (message "Switched to %s model" model))
    (setq claude-cli-chat-default-model model)
    (message "Default model set to %s (applies to next session)" model)))

(defun claude-cli-chat-set-permission-mode (mode)
  "Set permission MODE for current session.
MODE should be one of: default, accept-edits, plan, bypass."
  (interactive
   (list (intern (completing-read "Permission mode: "
                                  '("default" "accept-edits" "plan" "bypass")
                                  nil t))))
  (unless mode
    (user-error "Permission mode selection cancelled"))
  (unless claude-cli-chat--session
    (user-error "No active session"))
  (claude-cli-set-permission-mode claude-cli-chat--session mode)
  (message "Permission mode set to %s" mode))

(provide 'claude-cli-chat)
;;; claude-cli-chat.el ends here
