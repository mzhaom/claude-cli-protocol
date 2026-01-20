;;; claude-cli-chat-transient.el --- Transient menus for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file provides transient-based keyboard menus for Claude Chat.
;; Menus allow switching models, permission modes, exporting, etc.

;;; Code:

(require 'transient)

;; Forward declarations
(declare-function claude-cli-chat--session-state "claude-cli-chat")
(declare-function claude-cli-chat--current-model "claude-cli-chat")
(declare-function claude-cli-chat--permission-mode "claude-cli-chat")
(declare-function claude-cli-chat--processing-p "claude-cli-chat")
(declare-function claude-cli-chat-new-session "claude-cli-chat")
(declare-function claude-cli-chat-close-session "claude-cli-chat")
(declare-function claude-cli-chat-interrupt "claude-cli-chat")
(declare-function claude-cli-chat-set-model "claude-cli-chat")
(declare-function claude-cli-chat-set-permission-mode "claude-cli-chat")
(declare-function claude-cli-chat-export-markdown "claude-cli-chat-export")
(declare-function claude-cli-chat-export-org "claude-cli-chat-export")
(declare-function claude-cli-chat-export-json "claude-cli-chat-export")
(declare-function claude-cli-chat-export-text "claude-cli-chat-export")

(defvar claude-cli-chat--session)
(defvar claude-cli-chat--turn-number)
(defvar claude-cli-chat--total-input-tokens)
(defvar claude-cli-chat--total-output-tokens)
(defvar claude-cli-chat--total-cost)

;;; Main Menu

(transient-define-prefix claude-cli-chat-transient-menu ()
  "Claude Chat command menu."
  [:description
   (lambda ()
     (format "Claude Chat - %s | Turn %d | $%.4f"
             (symbol-name (or (claude-cli-chat--session-state) 'none))
             (or claude-cli-chat--turn-number 0)
             (or claude-cli-chat--total-cost 0.0)))
   ["Session"
    ("n" "New session" claude-cli-chat-transient--new-session)
    ("q" "Close session" claude-cli-chat-transient--close-session)
    ("i" "Interrupt" claude-cli-chat-transient--interrupt
     :if claude-cli-chat--processing-p)]
   ["Settings"
    ("m" "Model" claude-cli-chat-transient-model)
    ("p" "Permission mode" claude-cli-chat-transient-permission)]
   ["Export"
    ("e" "Export..." claude-cli-chat-transient-export)]
   ["Help"
    ("?" "Show bindings" claude-cli-chat-transient--show-bindings)]])

;;; Model Selection

(transient-define-prefix claude-cli-chat-transient-model ()
  "Select Claude model."
  [:description
   (lambda ()
     (format "Current model: %s" (or (claude-cli-chat--current-model) "N/A")))
   ["Quick Select"
    ("h" "Haiku (fastest, cheapest)" claude-cli-chat-transient--set-model-haiku)
    ("s" "Sonnet (balanced)" claude-cli-chat-transient--set-model-sonnet)
    ("o" "Opus (most capable)" claude-cli-chat-transient--set-model-opus)]
   ["Custom"
    ("c" "Custom model..." claude-cli-chat-transient--set-model-custom)]])

(defun claude-cli-chat-transient--set-model-haiku ()
  "Set model to Haiku."
  (interactive)
  (claude-cli-chat-set-model "haiku")
  (message "Model set to haiku (restart session to apply)"))

(defun claude-cli-chat-transient--set-model-sonnet ()
  "Set model to Sonnet."
  (interactive)
  (claude-cli-chat-set-model "sonnet")
  (message "Model set to sonnet (restart session to apply)"))

(defun claude-cli-chat-transient--set-model-opus ()
  "Set model to Opus."
  (interactive)
  (claude-cli-chat-set-model "opus")
  (message "Model set to opus (restart session to apply)"))

(defun claude-cli-chat-transient--set-model-custom ()
  "Set custom model."
  (interactive)
  (let ((model (read-string "Model name: ")))
    (claude-cli-chat-set-model model)
    (message "Model set to %s (restart session to apply)" model)))

;;; Permission Mode Selection

(transient-define-prefix claude-cli-chat-transient-permission ()
  "Select permission mode."
  [:description
   (lambda ()
     (format "Current mode: %s" (or (claude-cli-chat--permission-mode) "N/A")))
   ["Permission Modes"
    ("d" "Default (ask for dangerous ops)"
     claude-cli-chat-transient--set-permission-default
     :description (lambda ()
                    (if (eq (claude-cli-chat--permission-mode) 'default)
                        (propertize "Default (current)" 'face 'success)
                      "Default")))
    ("a" "Accept edits (auto-approve file edits)"
     claude-cli-chat-transient--set-permission-accept-edits
     :description (lambda ()
                    (if (eq (claude-cli-chat--permission-mode) 'accept-edits)
                        (propertize "Accept edits (current)" 'face 'success)
                      "Accept edits")))
    ("p" "Plan (read-only, no writes)"
     claude-cli-chat-transient--set-permission-plan
     :description (lambda ()
                    (if (eq (claude-cli-chat--permission-mode) 'plan)
                        (propertize "Plan (current)" 'face 'success)
                      "Plan")))
    ("b" "Bypass (auto-approve ALL)"
     claude-cli-chat-transient--set-permission-bypass
     :description (lambda ()
                    (if (eq (claude-cli-chat--permission-mode) 'bypass)
                        (propertize "Bypass (current)" 'face 'warning)
                      (propertize "Bypass" 'face 'warning))))]])

(defun claude-cli-chat-transient--set-permission-default ()
  "Set permission mode to default."
  (interactive)
  (claude-cli-chat-set-permission-mode 'default))

(defun claude-cli-chat-transient--set-permission-accept-edits ()
  "Set permission mode to accept-edits."
  (interactive)
  (claude-cli-chat-set-permission-mode 'accept-edits))

(defun claude-cli-chat-transient--set-permission-plan ()
  "Set permission mode to plan."
  (interactive)
  (claude-cli-chat-set-permission-mode 'plan))

(defun claude-cli-chat-transient--set-permission-bypass ()
  "Set permission mode to bypass with confirmation."
  (interactive)
  (if (yes-or-no-p "Bypass mode will auto-approve ALL operations including writes. Continue? ")
      (claude-cli-chat-set-permission-mode 'bypass)
    (message "Cancelled")))

;;; Export Menu

(transient-define-prefix claude-cli-chat-transient-export ()
  "Export conversation."
  ["Export Formats"
   ("m" "Markdown (.md)" claude-cli-chat-transient--export-markdown)
   ("o" "Org-mode (.org)" claude-cli-chat-transient--export-org)
   ("j" "JSON (.json)" claude-cli-chat-transient--export-json)
   ("t" "Plain text (.txt)" claude-cli-chat-transient--export-text)]
  ["Quick Actions"
   ("c" "Copy to clipboard" claude-cli-chat-transient--copy-to-clipboard)
   ("b" "Export to buffer" claude-cli-chat-transient--export-to-buffer)])

(defun claude-cli-chat-transient--export-markdown ()
  "Export conversation as Markdown."
  (interactive)
  (require 'claude-cli-chat-export)
  (claude-cli-chat-export-markdown))

(defun claude-cli-chat-transient--export-org ()
  "Export conversation as Org-mode."
  (interactive)
  (require 'claude-cli-chat-export)
  (claude-cli-chat-export-org))

(defun claude-cli-chat-transient--export-json ()
  "Export conversation as JSON."
  (interactive)
  (require 'claude-cli-chat-export)
  (claude-cli-chat-export-json))

(defun claude-cli-chat-transient--export-text ()
  "Export conversation as plain text."
  (interactive)
  (require 'claude-cli-chat-export)
  (claude-cli-chat-export-text))

(defun claude-cli-chat-transient--copy-to-clipboard ()
  "Copy conversation to clipboard as plain text."
  (interactive)
  (require 'claude-cli-chat-export)
  (let ((content (claude-cli-chat-export--to-text)))
    (kill-new content)
    (message "Conversation copied to clipboard")))

(defun claude-cli-chat-transient--export-to-buffer ()
  "Export conversation to a new buffer."
  (interactive)
  (require 'claude-cli-chat-export)
  (let ((content (claude-cli-chat-export--to-markdown)))
    (with-current-buffer (get-buffer-create "*Claude Chat Export*")
      (erase-buffer)
      (insert content)
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (goto-char (point-min)))
    (pop-to-buffer "*Claude Chat Export*")))

;;; Session Commands

(defun claude-cli-chat-transient--new-session ()
  "Start a new session."
  (interactive)
  (when (yes-or-no-p "Start new session? Current conversation will be cleared. ")
    (claude-cli-chat-new-session)))

(defun claude-cli-chat-transient--close-session ()
  "Close the current session."
  (interactive)
  (claude-cli-chat-close-session))

(defun claude-cli-chat-transient--interrupt ()
  "Interrupt current operation."
  (interactive)
  (claude-cli-chat-interrupt))

;;; Help

(defun claude-cli-chat-transient--show-bindings ()
  "Show key bindings help."
  (interactive)
  (with-help-window "*Claude Chat Bindings*"
    (princ "Claude Chat Key Bindings\n")
    (princ "========================\n\n")
    (princ "Core Operations:\n")
    (princ "  C-c C-c    Send message\n")
    (princ "  C-c C-k    Interrupt current operation\n")
    (princ "  C-c C-n    New session\n")
    (princ "  C-c C-q    Close session\n")
    (princ "  ?          Open this menu\n")
    (princ "\n")
    (princ "Navigation:\n")
    (princ "  n          Next turn\n")
    (princ "  p          Previous turn\n")
    (princ "  N          Next section\n")
    (princ "  P          Previous section\n")
    (princ "  TAB        Toggle section collapse/expand\n")
    (princ "  ^          Go to parent section\n")
    (princ "\n")
    (princ "Input:\n")
    (princ "  i          Focus input area\n")
    (princ "  RET        New line in input area\n")
    (princ "  M-p        Previous history item\n")
    (princ "  M-n        Next history item\n")
    (princ "\n")
    (princ "Other:\n")
    (princ "  g          Refresh display\n")
    (princ "  q          Quit window\n")))

(provide 'claude-cli-chat-transient)
;;; claude-cli-chat-transient.el ends here
