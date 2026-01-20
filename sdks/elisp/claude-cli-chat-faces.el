;;; claude-cli-chat-faces.el --- Face definitions for Claude Chat UI -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Chat Contributors
;; Keywords: faces, tools
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This file defines all faces used by the Claude Chat UI.
;; Faces are organized by category and support both light and dark themes.

;;; Code:

(defgroup claude-cli-chat-faces nil
  "Faces for Claude chat interface."
  :group 'claude-cli-chat
  :group 'faces)

;;; Header and Mode Line Faces

(defface claude-cli-chat-header-label
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for header line labels (Model:, Mode:, etc.)."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-header-value
  '((t :inherit font-lock-string-face))
  "Face for header line values."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-header-separator
  '((t :inherit font-lock-comment-face))
  "Face for header line separators."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-state-ready
  '((((background dark)) :foreground "#98c379" :weight bold)
    (t :foreground "#50a14f" :weight bold))
  "Face for ready state indicator."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-state-processing
  '((((background dark)) :foreground "#e5c07b" :weight bold)
    (t :foreground "#c18401" :weight bold))
  "Face for processing state indicator."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-state-starting
  '((((background dark)) :foreground "#61afef" :weight bold)
    (t :foreground "#4078f2" :weight bold))
  "Face for starting state indicator."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-state-closed
  '((((background dark)) :foreground "#e06c75" :weight bold)
    (t :foreground "#e45649" :weight bold))
  "Face for closed state indicator."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-state-unknown
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for unknown state indicator."
  :group 'claude-cli-chat-faces)

;;; Turn Section Faces

(defface claude-cli-chat-turn-heading
  '((((background dark)) :foreground "#abb2bf" :weight bold :height 1.1
     :underline (:color "#3e4451" :style line))
    (t :foreground "#383a42" :weight bold :height 1.1
       :underline (:color "#e5e5e6" :style line)))
  "Face for turn section headings."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-turn-number
  '((((background dark)) :foreground "#c678dd" :weight bold)
    (t :foreground "#a626a4" :weight bold))
  "Face for turn numbers."
  :group 'claude-cli-chat-faces)

;;; User Message Faces

(defface claude-cli-chat-user-label
  '((((background dark)) :foreground "#61afef" :weight bold)
    (t :foreground "#4078f2" :weight bold))
  "Face for user message label."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-user-content
  '((t :inherit default))
  "Face for user message content."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-user-background
  '((((background dark)) :background "#2c323c" :extend t)
    (t :background "#f0f0f0" :extend t))
  "Face for user message background."
  :group 'claude-cli-chat-faces)

;;; Assistant Message Faces

(defface claude-cli-chat-assistant-label
  '((((background dark)) :foreground "#c678dd" :weight bold)
    (t :foreground "#a626a4" :weight bold))
  "Face for assistant message label."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-assistant-content
  '((t :inherit default))
  "Face for assistant message content."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-assistant-streaming
  '((((background dark)) :foreground "#abb2bf" :slant italic)
    (t :foreground "#383a42" :slant italic))
  "Face for streaming assistant content."
  :group 'claude-cli-chat-faces)

;;; Thinking Faces

(defface claude-cli-chat-thinking-label
  '((((background dark)) :foreground "#56b6c2" :weight bold)
    (t :foreground "#0184bc" :weight bold))
  "Face for thinking section label."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-thinking-meta
  '((t :inherit font-lock-comment-face :height 0.9))
  "Face for thinking section metadata (char count, etc.)."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-thinking-content
  '((((background dark)) :foreground "#5c6370" :slant italic)
    (t :foreground "#a0a1a7" :slant italic))
  "Face for thinking content text."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-thinking-background
  '((((background dark)) :background "#21252b" :extend t)
    (t :background "#fafafa" :extend t))
  "Face for thinking section background."
  :group 'claude-cli-chat-faces)

;;; Tool Faces

(defface claude-cli-chat-tool-label
  '((((background dark)) :foreground "#e5c07b")
    (t :foreground "#986801"))
  "Face for tool section label."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-tool-name
  '((((background dark)) :foreground "#e5c07b" :weight bold)
    (t :foreground "#986801" :weight bold))
  "Face for tool name."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-tool-id
  '((t :inherit font-lock-comment-face :height 0.8))
  "Face for tool use ID."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-tool-sublabel
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for tool input/result labels."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-tool-json
  '((((background dark)) :foreground "#98c379" :height 0.9)
    (t :foreground "#50a14f" :height 0.9))
  "Face for JSON content in tools."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-tool-success
  '((((background dark)) :foreground "#98c379" :weight bold)
    (t :foreground "#50a14f" :weight bold))
  "Face for successful tool execution status."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-tool-error
  '((((background dark)) :foreground "#e06c75" :weight bold)
    (t :foreground "#e45649" :weight bold))
  "Face for failed tool execution status."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-tool-pending
  '((((background dark)) :foreground "#e5c07b" :slant italic)
    (t :foreground "#c18401" :slant italic))
  "Face for pending tool execution status."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-tool-background
  '((((background dark)) :background "#2c323c" :extend t)
    (t :background "#f5f5f5" :extend t))
  "Face for tool section background."
  :group 'claude-cli-chat-faces)

;;; Error Faces

(defface claude-cli-chat-error-label
  '((((background dark)) :foreground "#e06c75" :weight bold)
    (t :foreground "#e45649" :weight bold))
  "Face for error section label."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-error-content
  '((((background dark)) :foreground "#e06c75")
    (t :foreground "#e45649"))
  "Face for error message content."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-error-context
  '((t :inherit font-lock-comment-face))
  "Face for error context information."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-error-background
  '((((background dark)) :background "#3e2c2c" :extend t)
    (t :background "#fdf2f2" :extend t))
  "Face for error section background."
  :group 'claude-cli-chat-faces)

;;; Input Area Faces

(defface claude-cli-chat-separator
  '((((background dark)) :foreground "#3e4451" :weight bold)
    (t :foreground "#e5e5e6" :weight bold))
  "Face for input separator line."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-input-area
  '((((background dark)) :background "#21252b" :extend t)
    (t :background "#fafafa" :extend t))
  "Face for input area background."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-input-prompt
  '((((background dark)) :foreground "#61afef" :weight bold)
    (t :foreground "#4078f2" :weight bold))
  "Face for input prompt indicator."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-input-placeholder
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for input placeholder text."
  :group 'claude-cli-chat-faces)

;;; Miscellaneous Faces

(defface claude-cli-chat-truncated
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for truncation indicator."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-timestamp
  '((t :inherit font-lock-comment-face :height 0.85))
  "Face for timestamps."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-cost
  '((((background dark)) :foreground "#98c379")
    (t :foreground "#50a14f"))
  "Face for cost display."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-tokens
  '((((background dark)) :foreground "#61afef")
    (t :foreground "#4078f2"))
  "Face for token count display."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-code-block
  '((((background dark)) :background "#2c323c" :extend t)
    (t :background "#f0f0f0" :extend t))
  "Face for code block background."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-link
  '((((background dark)) :foreground "#61afef" :underline t)
    (t :foreground "#4078f2" :underline t))
  "Face for clickable links."
  :group 'claude-cli-chat-faces)

(defface claude-cli-chat-section-highlight
  '((((background dark)) :background "#3e4451" :extend t)
    (t :background "#e5e5e6" :extend t))
  "Face for highlighted/selected section."
  :group 'claude-cli-chat-faces)

(provide 'claude-cli-chat-faces)
;;; claude-cli-chat-faces.el ends here
