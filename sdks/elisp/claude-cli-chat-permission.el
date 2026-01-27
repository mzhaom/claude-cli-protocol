;;; claude-cli-chat-permission.el --- Permission UI for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file provides a permission prompt UI for Claude Chat.
;; When a tool needs permission, it displays a popup buffer showing
;; the tool name, input details, and allows the user to allow/deny.
;;
;; Key bindings in permission buffer:
;;   a         Allow this tool
;;   d         Deny this tool
;;   A         Allow all instances of this tool
;;   D         Deny all instances of this tool
;;   q         Deny and quit
;;   ?         Show help

;;; Code:

(require 'cl-lib)
(require 'claude-cli-permission)

;;; Customization

(defgroup claude-cli-chat-permission nil
  "Permission UI for Claude Chat."
  :group 'claude-cli-chat
  :prefix "claude-cli-chat-permission-")

(defcustom claude-cli-chat-permission-auto-allow-tools nil
  "List of tool names to automatically allow without prompting."
  :type '(repeat string)
  :group 'claude-cli-chat-permission)

(defcustom claude-cli-chat-permission-auto-deny-tools nil
  "List of tool names to automatically deny without prompting."
  :type '(repeat string)
  :group 'claude-cli-chat-permission)

;;; Session-local auto-allow/deny (reset on new session)

(defvar-local claude-cli-chat-permission--session-allow nil
  "List of tools auto-allowed for this session.")

(defvar-local claude-cli-chat-permission--session-deny nil
  "List of tools auto-denied for this session.")

;;; Permission Buffer

(defvar claude-cli-chat-permission-buffer-name "*Claude Permission*"
  "Name of the permission prompt buffer.")

(defvar claude-cli-chat-permission-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'claude-cli-chat-permission-allow)
    (define-key map (kbd "d") #'claude-cli-chat-permission-deny)
    (define-key map (kbd "A") #'claude-cli-chat-permission-allow-always)
    (define-key map (kbd "D") #'claude-cli-chat-permission-deny-always)
    (define-key map (kbd "q") #'claude-cli-chat-permission-quit)
    (define-key map (kbd "?") #'claude-cli-chat-permission-help)
    (define-key map (kbd "RET") #'claude-cli-chat-permission-allow)
    map)
  "Keymap for permission prompt buffer.")

(define-derived-mode claude-cli-chat-permission-mode special-mode "Claude-Permission"
  "Major mode for Claude permission prompts."
  :group 'claude-cli-chat-permission
  (setq buffer-read-only t)
  (setq truncate-lines nil)
  (setq word-wrap t))

;;; Permission Display

(defvar claude-cli-chat-permission--current-tool nil
  "Current tool name waiting for permission.")

(defvar claude-cli-chat-permission--current-input nil
  "Current tool input waiting for permission.")

(defvar claude-cli-chat-permission--callback nil
  "Callback function to call with permission decision.")

(defun claude-cli-chat-permission--format-input (input)
  "Format INPUT for display in permission buffer."
  (condition-case nil
      (with-temp-buffer
        (insert (json-encode input))
        (json-pretty-print-buffer)
        (buffer-string))
    (error (format "%S" input))))

(defun claude-cli-chat-permission--display (tool-name input)
  "Display permission prompt for TOOL-NAME with INPUT."
  (let ((buf (get-buffer-create claude-cli-chat-permission-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (claude-cli-chat-permission-mode)
        ;; Header
        (insert (propertize "⚠ Permission Request\n\n"
                            'face '(:weight bold :height 1.3)))
        ;; Tool info
        (insert (propertize "Tool: " 'face '(:weight bold)))
        (insert (propertize tool-name 'face 'font-lock-function-name-face))
        (insert "\n\n")
        ;; Input
        (insert (propertize "Input:\n" 'face '(:weight bold)))
        (let ((input-str (claude-cli-chat-permission--format-input input)))
          (insert (propertize input-str 'face 'font-lock-string-face)))
        (insert "\n\n")
        ;; Options
        (insert (propertize "─────────────────────────────────────────────────────────\n"
                            'face 'shadow))
        (insert (propertize "[a]" 'face 'font-lock-keyword-face))
        (insert "llow  ")
        (insert (propertize "[d]" 'face 'font-lock-keyword-face))
        (insert "eny  ")
        (insert (propertize "[A]" 'face 'font-lock-keyword-face))
        (insert "llow-always  ")
        (insert (propertize "[D]" 'face 'font-lock-keyword-face))
        (insert "eny-always  ")
        (insert (propertize "[q]" 'face 'font-lock-keyword-face))
        (insert "uit\n")
        (goto-char (point-min))))
    ;; Display buffer
    (display-buffer buf '(display-buffer-below-selected
                          (window-height . fit-window-to-buffer)))))

(defun claude-cli-chat-permission--close-buffer ()
  "Close the permission buffer."
  (when-let ((buf (get-buffer claude-cli-chat-permission-buffer-name)))
    (when-let ((win (get-buffer-window buf)))
      (delete-window win))
    (kill-buffer buf)))

;;; Permission Commands

(defun claude-cli-chat-permission-allow ()
  "Allow the current tool request."
  (interactive)
  (claude-cli-chat-permission--respond (claude-cli-permission-allow)))

(defun claude-cli-chat-permission-deny ()
  "Deny the current tool request."
  (interactive)
  (claude-cli-chat-permission--respond
   (claude-cli-permission-deny "User denied permission")))

(defun claude-cli-chat-permission-allow-always ()
  "Allow this tool and remember for the session."
  (interactive)
  (when claude-cli-chat-permission--current-tool
    (push claude-cli-chat-permission--current-tool
          claude-cli-chat-permission--session-allow))
  (claude-cli-chat-permission--respond (claude-cli-permission-allow)))

(defun claude-cli-chat-permission-deny-always ()
  "Deny this tool and remember for the session."
  (interactive)
  (when claude-cli-chat-permission--current-tool
    (push claude-cli-chat-permission--current-tool
          claude-cli-chat-permission--session-deny))
  (claude-cli-chat-permission--respond
   (claude-cli-permission-deny "User denied permission (always)")))

(defun claude-cli-chat-permission-quit ()
  "Deny and interrupt the session."
  (interactive)
  (claude-cli-chat-permission--respond
   (claude-cli-permission-deny "User interrupted" t)))

(defun claude-cli-chat-permission-help ()
  "Show help for permission prompt."
  (interactive)
  (message "a=allow, d=deny, A=allow-always, D=deny-always, q=quit/interrupt"))

(defun claude-cli-chat-permission--respond (decision)
  "Respond with DECISION and close permission buffer."
  (let ((callback claude-cli-chat-permission--callback))
    (claude-cli-chat-permission--close-buffer)
    (setq claude-cli-chat-permission--current-tool nil
          claude-cli-chat-permission--current-input nil
          claude-cli-chat-permission--callback nil)
    (when callback
      (funcall callback decision))))

;;; Main Entry Point

(defun claude-cli-chat-permission-prompt (tool-name input callback)
  "Prompt for permission for TOOL-NAME with INPUT.
CALLBACK is called with the permission decision."
  ;; Check auto-allow/deny lists
  (cond
   ;; Auto-allow
   ((or (member tool-name claude-cli-chat-permission-auto-allow-tools)
        (member tool-name claude-cli-chat-permission--session-allow))
    (funcall callback (claude-cli-permission-allow)))
   ;; Auto-deny
   ((or (member tool-name claude-cli-chat-permission-auto-deny-tools)
        (member tool-name claude-cli-chat-permission--session-deny))
    (funcall callback (claude-cli-permission-deny "Auto-denied by policy")))
   ;; Show prompt
   (t
    (setq claude-cli-chat-permission--current-tool tool-name
          claude-cli-chat-permission--current-input input
          claude-cli-chat-permission--callback callback)
    (claude-cli-chat-permission--display tool-name input))))

;;; Synchronous Wrapper (for current SDK design)

(defun claude-cli-chat-permission-handler (tool-name input)
  "Permission handler that shows a popup buffer for TOOL-NAME with INPUT.
Returns a permission decision."
  ;; Check auto-allow/deny lists first (no UI needed)
  (cond
   ;; Auto-allow
   ((or (member tool-name claude-cli-chat-permission-auto-allow-tools)
        (and (boundp 'claude-cli-chat-permission--session-allow)
             (member tool-name claude-cli-chat-permission--session-allow)))
    (claude-cli-permission-allow))
   ;; Auto-deny
   ((or (member tool-name claude-cli-chat-permission-auto-deny-tools)
        (and (boundp 'claude-cli-chat-permission--session-deny)
             (member tool-name claude-cli-chat-permission--session-deny)))
    (claude-cli-permission-deny "Auto-denied by policy"))
   ;; Show detailed prompt
   (t
    (let* ((input-preview (claude-cli-chat-permission--format-input input))
           (prompt (format "Tool: %s\nInput: %s\n\nAllow? (y/n/Y=always/N=never/q=quit) "
                           tool-name
                           (if (> (length input-preview) 200)
                               (concat (substring input-preview 0 200) "...")
                             input-preview)))
           (response (read-char-choice prompt '(?y ?n ?Y ?N ?q))))
      (pcase response
        (?y (claude-cli-permission-allow))
        (?n (claude-cli-permission-deny "User denied permission"))
        (?Y
         (when (boundp 'claude-cli-chat-permission--session-allow)
           (push tool-name claude-cli-chat-permission--session-allow))
         (claude-cli-permission-allow))
        (?N
         (when (boundp 'claude-cli-chat-permission--session-deny)
           (push tool-name claude-cli-chat-permission--session-deny))
         (claude-cli-permission-deny "User denied permission (always)"))
        (?q (claude-cli-permission-deny "User interrupted" t)))))))

(provide 'claude-cli-chat-permission)
;;; claude-cli-chat-permission.el ends here
