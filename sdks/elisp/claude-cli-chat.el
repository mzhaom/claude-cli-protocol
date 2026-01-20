;;; claude-cli-chat.el --- Interactive chat interface for Claude CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Chat Contributors
;; Keywords: tools, ai, chat
;; Package-Requires: ((emacs "27.1") (magit-section "3.0") (transient "0.4"))
;; Version: 0.1.0

;;; Commentary:

;; This package provides a modern, interactive Emacs interface for chatting
;; with Claude via the Claude CLI SDK.  Features include:
;;
;; - Streaming responses with real-time updates
;; - Magit-style collapsible sections for conversation turns
;; - Transient menus for model and permission mode switching
;; - Token usage and cost tracking
;; - Export to Markdown, Org-mode, JSON
;;
;; Usage:
;;   M-x claude-cli-chat
;;
;; Key bindings in conversation area:
;;   n/p          Next/previous turn
;;   M-n/M-p      Next/previous section
;;   TAB          Toggle section visibility
;;   i            Focus input area
;;   ?            Open command menu
;;   q            Quit window
;;
;; Key bindings in input area:
;;   C-c C-c      Send message
;;   C-RET/M-RET  Send message
;;   M-p/M-n      History previous/next
;;   C-c C-k      Interrupt operation
;;
;; Global bindings:
;;   C-c C-c  Send message
;;   C-c C-k  Interrupt current operation
;;   C-c C-n  New session
;;   C-c C-q  Close session
;;   C-c C-t  Open transient menu

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
(declare-function claude-cli-chat-sections--insert-turn "claude-cli-chat-sections")
(declare-function claude-cli-chat-transient-menu "claude-cli-chat-transient")
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

(defcustom claude-cli-chat-disable-plugins t
  "Whether to disable plugins for faster startup.
When non-nil, adds --plugin-dir /dev/null to CLI arguments."
  :type 'boolean
  :group 'claude-cli-chat)

(defcustom claude-cli-chat-buffer-name "*Claude Chat*"
  "Name for the Claude chat buffer."
  :type 'string
  :group 'claude-cli-chat)

(defcustom claude-cli-chat-show-thinking t
  "Whether to display extended thinking sections."
  :type 'boolean
  :group 'claude-cli-chat)

(defcustom claude-cli-chat-collapse-thinking t
  "Whether thinking sections should be collapsed by default."
  :type 'boolean
  :group 'claude-cli-chat)

(defcustom claude-cli-chat-collapse-tools t
  "Whether tool sections should be collapsed by default."
  :type 'boolean
  :group 'claude-cli-chat)

(defcustom claude-cli-chat-max-tool-result-lines 20
  "Maximum lines to show for tool results before truncating."
  :type 'integer
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

;; Hook function storage for cleanup
(defvar-local claude-cli-chat--text-hook-fn nil)
(defvar-local claude-cli-chat--thinking-hook-fn nil)
(defvar-local claude-cli-chat--tool-start-hook-fn nil)
(defvar-local claude-cli-chat--tool-complete-hook-fn nil)
(defvar-local claude-cli-chat--tool-result-hook-fn nil)
(defvar-local claude-cli-chat--turn-complete-hook-fn nil)
(defvar-local claude-cli-chat--error-hook-fn nil)
(defvar-local claude-cli-chat--state-change-hook-fn nil)
(defvar-local claude-cli-chat--ready-hook-fn nil)

;;; Keymap

(defvar claude-cli-chat-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from special-mode-map for basic read-only buffer behavior
    (set-keymap-parent map special-mode-map)

    ;; Core operations (work everywhere)
    (define-key map (kbd "C-c C-c") #'claude-cli-chat-send)
    (define-key map (kbd "C-c C-k") #'claude-cli-chat-interrupt)
    (define-key map (kbd "C-c C-n") #'claude-cli-chat-new-session)
    (define-key map (kbd "C-c C-q") #'claude-cli-chat-close-session)

    ;; Navigation in conversation area
    ;; These are simple - they just call navigation functions directly
    ;; The input area has its own keymap via overlay that takes precedence
    (define-key map (kbd "n") #'claude-cli-chat-navigation-next-turn)
    (define-key map (kbd "p") #'claude-cli-chat-navigation-previous-turn)
    (define-key map (kbd "M-n") #'claude-cli-chat-navigation-next-section)
    (define-key map (kbd "M-p") #'claude-cli-chat-navigation-previous-section)
    (define-key map (kbd "TAB") #'claude-cli-chat-toggle-section)
    (define-key map (kbd "<tab>") #'claude-cli-chat-toggle-section)
    (define-key map (kbd "^") #'claude-cli-chat-navigation-section-up)

    ;; Transient menu
    (define-key map (kbd "?") #'claude-cli-chat-menu)
    (define-key map (kbd "C-c C-t") #'claude-cli-chat-menu)

    ;; Input focus
    (define-key map (kbd "i") #'claude-cli-chat-focus-input)

    ;; Quick actions
    (define-key map (kbd "g") #'claude-cli-chat-refresh)
    (define-key map (kbd "q") #'quit-window)

    map)
  "Keymap for `claude-cli-chat-mode'.
Navigation keys (n, p, TAB, etc.) work in the conversation area.
The input area has its own keymap with text editing bindings.")

;;; Header Line

(defun claude-cli-chat--header-line ()
  "Construct header line showing session info."
  (let* ((model (claude-cli-chat--current-model))
         (mode (claude-cli-chat--permission-mode))
         (state (claude-cli-chat--session-state))
         (state-str (claude-cli-chat--state-indicator state)))
    (concat
     (propertize " Model: " 'face 'claude-cli-chat-header-label)
     (propertize (or model "N/A") 'face 'claude-cli-chat-header-value)
     (propertize " | " 'face 'claude-cli-chat-header-separator)
     (propertize "Mode: " 'face 'claude-cli-chat-header-label)
     (propertize (symbol-name (or mode 'N/A)) 'face 'claude-cli-chat-header-value)
     (propertize " | " 'face 'claude-cli-chat-header-separator)
     (propertize "Tokens: " 'face 'claude-cli-chat-header-label)
     (propertize (format "%d" claude-cli-chat--total-input-tokens)
                 'face 'claude-cli-chat-tokens)
     (propertize "/" 'face 'claude-cli-chat-header-separator)
     (propertize (format "%d" claude-cli-chat--total-output-tokens)
                 'face 'claude-cli-chat-tokens)
     (propertize " | " 'face 'claude-cli-chat-header-separator)
     (propertize "Cost: " 'face 'claude-cli-chat-header-label)
     (propertize (format "$%.4f" claude-cli-chat--total-cost)
                 'face 'claude-cli-chat-cost)
     ;; Only add separator and state if state indicator is non-empty
     (if (string-empty-p state-str)
         ""
       (concat (propertize " | " 'face 'claude-cli-chat-header-separator)
               state-str)))))

(defun claude-cli-chat--state-indicator (state)
  "Return propertized string for STATE indicator.
Starting state is not shown - errors appear in the conversation area."
  (pcase state
    ('ready (propertize "[Ready]" 'face 'claude-cli-chat-state-ready))
    ('processing (propertize "[Processing...]" 'face 'claude-cli-chat-state-processing))
    ('starting "")  ; Don't show starting state - errors go to conversation
    ('closed (propertize "[Closed]" 'face 'claude-cli-chat-state-closed))
    ('uninitialized "")  ; Don't show until session is active
    (_ "")))

;;; Mode Line

(defun claude-cli-chat--mode-line-format ()
  "Construct mode line format for claude-cli-chat-mode."
  `("%e"
    mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    mode-line-buffer-identification
    "   "
    (:eval (claude-cli-chat--mode-line-state))
    "   "
    (:eval (format "Turn %d" claude-cli-chat--turn-number))
    mode-line-end-spaces))

(defun claude-cli-chat--mode-line-state ()
  "Return mode line segment showing session state."
  (claude-cli-chat--state-indicator (claude-cli-chat--session-state)))

;;; Helper Functions

(defun claude-cli-chat--current-model ()
  "Return current model name or nil."
  (when claude-cli-chat--session
    (let ((info (claude-cli-session-get-info claude-cli-chat--session)))
      (when info
        (claude-cli-session-info-model info)))))

(defun claude-cli-chat--permission-mode ()
  "Return current permission mode or nil."
  (when claude-cli-chat--session
    (let ((info (claude-cli-session-get-info claude-cli-chat--session)))
      (when info
        (claude-cli-session-info-permission-mode info)))))

(defun claude-cli-chat--session-state ()
  "Return current session state symbol."
  (if claude-cli-chat--session
      (claude-cli-session-get-state claude-cli-chat--session)
    'uninitialized))

(defun claude-cli-chat--processing-p ()
  "Return non-nil if session is currently processing."
  (and claude-cli-chat--session
       (claude-cli-session-processing-p claude-cli-chat--session)))

(defun claude-cli-chat--ready-p ()
  "Return non-nil if session is ready for input."
  (and claude-cli-chat--session
       (claude-cli-session-ready-p claude-cli-chat--session)))

;;; Major Mode

(define-derived-mode claude-cli-chat-mode special-mode "Claude-Chat"
  "Major mode for Claude AI chat interface.

\\{claude-cli-chat-mode-map}"
  :group 'claude-cli-chat
  ;; Set up header and mode line
  (setq header-line-format '(:eval (claude-cli-chat--header-line)))
  (setq mode-line-format (claude-cli-chat--mode-line-format))
  ;; Buffer is read-only by default, input area handles editability
  (setq buffer-read-only t)
  ;; Enable visual line mode for better text wrapping
  (visual-line-mode 1)
  ;; Set up kill buffer hook for cleanup
  (add-hook 'kill-buffer-hook #'claude-cli-chat--cleanup nil t))

;;; Session Management

(defun claude-cli-chat--setup-hooks ()
  "Set up hooks for SDK events in current buffer."
  (let ((buffer (current-buffer)))
    ;; Create buffer-specific hook functions
    (setq claude-cli-chat--text-hook-fn
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claude-cli-chat-buffer--handle-text event)))))

    (setq claude-cli-chat--thinking-hook-fn
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claude-cli-chat-buffer--handle-thinking event)))))

    (setq claude-cli-chat--tool-start-hook-fn
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claude-cli-chat-buffer--handle-tool-start event)))))

    (setq claude-cli-chat--tool-complete-hook-fn
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claude-cli-chat-buffer--handle-tool-complete event)))))

    (setq claude-cli-chat--tool-result-hook-fn
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claude-cli-chat-buffer--handle-tool-result event)))))

    (setq claude-cli-chat--turn-complete-hook-fn
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claude-cli-chat-buffer--handle-turn-complete event)))))

    (setq claude-cli-chat--error-hook-fn
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claude-cli-chat-buffer--handle-error event)))))

    (setq claude-cli-chat--state-change-hook-fn
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claude-cli-chat-buffer--handle-state-change event)))))

    (setq claude-cli-chat--ready-hook-fn
          (lambda (event)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (claude-cli-chat-buffer--handle-ready event)))))

    ;; Add to global hooks
    (add-hook 'claude-cli-text-hook claude-cli-chat--text-hook-fn)
    (add-hook 'claude-cli-thinking-hook claude-cli-chat--thinking-hook-fn)
    (add-hook 'claude-cli-tool-start-hook claude-cli-chat--tool-start-hook-fn)
    (add-hook 'claude-cli-tool-complete-hook claude-cli-chat--tool-complete-hook-fn)
    (add-hook 'claude-cli-tool-result-hook claude-cli-chat--tool-result-hook-fn)
    (add-hook 'claude-cli-turn-complete-hook claude-cli-chat--turn-complete-hook-fn)
    (add-hook 'claude-cli-error-hook claude-cli-chat--error-hook-fn)
    (add-hook 'claude-cli-state-change-hook claude-cli-chat--state-change-hook-fn)
    (add-hook 'claude-cli-ready-hook claude-cli-chat--ready-hook-fn)))

(defun claude-cli-chat--teardown-hooks ()
  "Remove SDK event hooks."
  (when claude-cli-chat--text-hook-fn
    (remove-hook 'claude-cli-text-hook claude-cli-chat--text-hook-fn))
  (when claude-cli-chat--thinking-hook-fn
    (remove-hook 'claude-cli-thinking-hook claude-cli-chat--thinking-hook-fn))
  (when claude-cli-chat--tool-start-hook-fn
    (remove-hook 'claude-cli-tool-start-hook claude-cli-chat--tool-start-hook-fn))
  (when claude-cli-chat--tool-complete-hook-fn
    (remove-hook 'claude-cli-tool-complete-hook claude-cli-chat--tool-complete-hook-fn))
  (when claude-cli-chat--tool-result-hook-fn
    (remove-hook 'claude-cli-tool-result-hook claude-cli-chat--tool-result-hook-fn))
  (when claude-cli-chat--turn-complete-hook-fn
    (remove-hook 'claude-cli-turn-complete-hook claude-cli-chat--turn-complete-hook-fn))
  (when claude-cli-chat--error-hook-fn
    (remove-hook 'claude-cli-error-hook claude-cli-chat--error-hook-fn))
  (when claude-cli-chat--state-change-hook-fn
    (remove-hook 'claude-cli-state-change-hook claude-cli-chat--state-change-hook-fn))
  (when claude-cli-chat--ready-hook-fn
    (remove-hook 'claude-cli-ready-hook claude-cli-chat--ready-hook-fn)))

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
         :permission-handler #'claude-cli-chat--permission-handler
         :disable-plugins claude-cli-chat-disable-plugins))
  (claude-cli-chat--setup-hooks)
  (claude-cli-start claude-cli-chat--session))

(defun claude-cli-chat--permission-handler (tool-name input)
  "Interactive permission handler for TOOL-NAME with INPUT."
  ;; For now, use simple y-or-n prompt
  ;; Can be enhanced with detailed popup later
  (let ((prompt (format "Allow tool '%s'? " tool-name)))
    (if (y-or-n-p prompt)
        (claude-cli-permission-allow)
      (claude-cli-permission-deny "User denied permission"))))

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
  "Send the current input to Claude."
  (interactive)
  (require 'claude-cli-chat-input)
  (let ((content (string-trim (claude-cli-chat-input--get-content))))
    (cond
     ((string-empty-p content)
      (message "Cannot send empty message"))
     ;; Allow sending in both 'ready' and 'starting' states
     ;; The CLI only emits init after receiving the first message
     ((not (memq (claude-cli-chat--session-state) '(ready starting)))
      (message "Session not ready (state: %s)"
               (claude-cli-chat--session-state)))
     (t
      ;; Increment turn counter
      (cl-incf claude-cli-chat--turn-number)
      ;; Initialize current turn tracking
      (setq claude-cli-chat--current-turn
            (list :number claude-cli-chat--turn-number
                  :user-content content
                  :assistant-content ""
                  :thinking ""
                  :tools nil))
      ;; Insert turn into buffer
      (require 'claude-cli-chat-sections)
      (require 'claude-cli-chat-buffer)
      (claude-cli-chat-buffer--insert-user-turn claude-cli-chat--turn-number content)
      ;; Clear input
      (claude-cli-chat-input--clear)
      ;; Send to SDK
      (claude-cli-send-message claude-cli-chat--session content)))))

(defun claude-cli-chat-interrupt ()
  "Interrupt the current Claude operation."
  (interactive)
  (if (claude-cli-chat--processing-p)
      (progn
        (claude-cli-interrupt claude-cli-chat--session)
        (message "Interrupted"))
    (message "No operation in progress")))

(defun claude-cli-chat-new-session ()
  "Start a new chat session, closing the existing one."
  (interactive)
  (when (and claude-cli-chat--session
             (not (eq (claude-cli-session-get-state claude-cli-chat--session) 'closed)))
    (claude-cli-stop claude-cli-chat--session)
    (claude-cli-chat--teardown-hooks))
  ;; Reset state
  (setq claude-cli-chat--turn-number 0
        claude-cli-chat--total-input-tokens 0
        claude-cli-chat--total-output-tokens 0
        claude-cli-chat--total-cost 0.0
        claude-cli-chat--conversation-history nil
        claude-cli-chat--current-turn nil
        claude-cli-chat--streaming-marker nil)
  ;; Clear buffer and re-setup
  (let ((inhibit-read-only t))
    (erase-buffer))
  (require 'claude-cli-chat-buffer)
  (claude-cli-chat-buffer--setup)
  ;; Create new session
  (claude-cli-chat--create-session)
  (message "New session started"))

(defun claude-cli-chat-close-session ()
  "Close the current chat session."
  (interactive)
  (when claude-cli-chat--session
    (claude-cli-stop claude-cli-chat--session)
    (message "Session closed")))

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

(defun claude-cli-chat-refresh ()
  "Refresh the header and mode line display."
  (interactive)
  (force-mode-line-update))

(defun claude-cli-chat-toggle-section ()
  "Toggle visibility of current section.
In the input area, performs tab completion instead."
  (interactive)
  (require 'claude-cli-chat-input)
  (require 'claude-cli-chat-navigation)
  (if (claude-cli-chat-input--in-input-p)
      (indent-for-tab-command)
    (claude-cli-chat-navigation-toggle-section)))

;;; Model and Permission Mode Commands

(defun claude-cli-chat-set-model (model)
  "Set the MODEL for the current session.
Note: This requires restarting the session to take effect."
  (interactive
   (list (completing-read "Model: "
                          '("haiku" "sonnet" "opus" "claude-3-haiku-20240307"
                            "claude-3-sonnet-20240229" "claude-3-opus-20240229")
                          nil nil nil nil claude-cli-chat-default-model)))
  (setq claude-cli-chat-default-model model)
  (message "Model set to %s (restart session to apply)" model))

(defun claude-cli-chat-set-permission-mode (mode)
  "Set permission MODE for current session."
  (interactive
   (list (intern (completing-read "Permission mode: "
                                  '("default" "accept-edits" "plan" "bypass")
                                  nil t))))
  (when claude-cli-chat--session
    (claude-cli-set-permission-mode claude-cli-chat--session mode)
    (message "Permission mode set to %s" mode)))

(provide 'claude-cli-chat)
;;; claude-cli-chat.el ends here
