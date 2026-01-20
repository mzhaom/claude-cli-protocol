;;; claude-cli-chat-input.el --- Input area management for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file manages the input area at the bottom of the Claude Chat buffer.
;; The input area is editable while the rest of the buffer is read-only.

;;; Code:

(require 'cl-lib)
(require 'claude-cli-chat-faces)

;; Buffer-local variable declarations (defined in claude-cli-chat.el)
(defvar claude-cli-chat--input-start)
(defvar claude-cli-chat--conversation-end)

;; Forward declarations
(declare-function claude-cli-chat-send "claude-cli-chat")
(declare-function claude-cli-chat-menu "claude-cli-chat")

;;; Input Area Keymap

(defvar claude-cli-chat-input-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Send message
    (define-key map (kbd "C-c C-c") #'claude-cli-chat-send)
    (define-key map (kbd "C-<return>") #'claude-cli-chat-send)
    (define-key map (kbd "M-<return>") #'claude-cli-chat-send)
    (define-key map (kbd "<C-return>") #'claude-cli-chat-send)
    (define-key map (kbd "<M-return>") #'claude-cli-chat-send)
    ;; History navigation
    (define-key map (kbd "M-p") #'claude-cli-chat-input--history-previous)
    (define-key map (kbd "M-n") #'claude-cli-chat-input--history-next)
    ;; Menu access
    (define-key map (kbd "C-c C-t") #'claude-cli-chat-menu)
    ;; Standard text editing keys are inherited
    map)
  "Keymap for the input area in `claude-cli-chat-mode'.
This keymap is active only in the editable input area.")

;;; Constants

(defconst claude-cli-chat-input--separator-char ?─
  "Character used for the input separator line.")

(defconst claude-cli-chat-input--separator-width 60
  "Width of the input separator line.")

;;; Buffer-local Variables

(defvar-local claude-cli-chat-input--overlay nil
  "Overlay for the input area styling.")

(defvar-local claude-cli-chat-input--placeholder-overlay nil
  "Overlay for placeholder text when input is empty.")

;;; Setup

(defun claude-cli-chat-input--setup ()
  "Set up the input area at the end of the buffer."
  (let ((inhibit-read-only t))
    ;; Insert separator
    (goto-char (point-max))
    (insert "\n")
    (insert (propertize (make-string claude-cli-chat-input--separator-width
                                     claude-cli-chat-input--separator-char)
                        'face 'claude-cli-chat-separator
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky t))
    (insert "\n")
    ;; Mark input start
    (setq claude-cli-chat--input-start (point-marker))
    (set-marker-insertion-type claude-cli-chat--input-start nil)
    ;; Insert prompt indicator
    (insert (propertize "> " 'face 'claude-cli-chat-input-prompt
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky t))
    ;; Create input area overlay for styling and keymap
    (setq claude-cli-chat-input--overlay
          (make-overlay (point) (point-max) nil nil t))
    (overlay-put claude-cli-chat-input--overlay 'face 'claude-cli-chat-input-area)
    (overlay-put claude-cli-chat-input--overlay 'keymap claude-cli-chat-input-mode-map)
    (overlay-put claude-cli-chat-input--overlay 'modification-hooks
                 '(claude-cli-chat-input--extend-overlay))
    ;; Create placeholder overlay
    (claude-cli-chat-input--setup-placeholder)
    ;; Move point to input area
    (goto-char (point-max))))

(defun claude-cli-chat-input--setup-placeholder ()
  "Set up placeholder text overlay."
  (setq claude-cli-chat-input--placeholder-overlay
        (make-overlay (point) (point) nil t nil))
  (overlay-put claude-cli-chat-input--placeholder-overlay
               'before-string
               (propertize "Type your message here..."
                           'face 'claude-cli-chat-input-placeholder))
  (overlay-put claude-cli-chat-input--placeholder-overlay 'evaporate t)
  ;; Hook to hide placeholder when input is non-empty
  (add-hook 'post-command-hook #'claude-cli-chat-input--update-placeholder nil t))

(defun claude-cli-chat-input--update-placeholder ()
  "Update placeholder visibility based on input content."
  (when (and claude-cli-chat-input--placeholder-overlay
             (overlay-buffer claude-cli-chat-input--placeholder-overlay))
    (let ((content (claude-cli-chat-input--get-content)))
      (if (string-empty-p content)
          ;; Show placeholder
          (progn
            (move-overlay claude-cli-chat-input--placeholder-overlay
                          (claude-cli-chat-input--content-start)
                          (claude-cli-chat-input--content-start))
            (overlay-put claude-cli-chat-input--placeholder-overlay
                         'before-string
                         (propertize "Type your message here..."
                                     'face 'claude-cli-chat-input-placeholder)))
        ;; Hide placeholder
        (overlay-put claude-cli-chat-input--placeholder-overlay 'before-string nil)))))

(defun claude-cli-chat-input--extend-overlay (overlay after beg end &optional len)
  "Extend OVERLAY to cover new text when modified.
AFTER, BEG, END, LEN are standard modification hook arguments."
  (when (and after (overlay-buffer overlay))
    (move-overlay overlay (overlay-start overlay) (point-max))))

;;; Content Access

(defun claude-cli-chat-input--content-start ()
  "Return the position where actual input content starts (after prompt)."
  (when claude-cli-chat--input-start
    (save-excursion
      (goto-char claude-cli-chat--input-start)
      ;; Skip past the "> " prompt
      (when (looking-at "> ")
        (forward-char 2))
      (point))))

(defun claude-cli-chat-input--get-content ()
  "Get the current input content (without prompt)."
  (let ((start (claude-cli-chat-input--content-start)))
    (if start
        (buffer-substring-no-properties start (point-max))
      "")))

(defun claude-cli-chat-input--clear ()
  "Clear the input area content."
  (let ((inhibit-read-only t)
        (start (claude-cli-chat-input--content-start)))
    (when start
      (delete-region start (point-max))
      ;; Move overlay
      (when claude-cli-chat-input--overlay
        (move-overlay claude-cli-chat-input--overlay start (point-max)))
      ;; Trigger placeholder update
      (claude-cli-chat-input--update-placeholder))))

(defun claude-cli-chat-input--set-content (content)
  "Set the input area content to CONTENT."
  (claude-cli-chat-input--clear)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert content)
    (claude-cli-chat-input--update-placeholder)))

;;; Navigation

(defun claude-cli-chat-input--focus ()
  "Move point to the input area."
  (let ((target (or (claude-cli-chat-input--content-start)
                    claude-cli-chat--input-start)))
    (when target
      (goto-char target)
      ;; If there's content, go to end
      (unless (string-empty-p (claude-cli-chat-input--get-content))
        (goto-char (point-max))))))

(defun claude-cli-chat-input--in-input-p ()
  "Return non-nil if point is in the input area."
  (and claude-cli-chat--input-start
       (>= (point) claude-cli-chat--input-start)))

(defun claude-cli-chat-input--in-editable-p ()
  "Return non-nil if point is in the editable part of input area."
  (let ((content-start (claude-cli-chat-input--content-start)))
    (and content-start
         (>= (point) content-start))))

;;; Input Validation

(defun claude-cli-chat-input--empty-p ()
  "Return non-nil if input area is empty."
  (string-empty-p (string-trim (claude-cli-chat-input--get-content))))

(defun claude-cli-chat-input--get-trimmed-content ()
  "Get input content with leading/trailing whitespace removed."
  (string-trim (claude-cli-chat-input--get-content)))

;;; History (optional feature)

(defvar-local claude-cli-chat-input--history nil
  "History of sent messages in this buffer.")

(defvar-local claude-cli-chat-input--history-index -1
  "Current position in history (-1 means not browsing history).")

(defun claude-cli-chat-input--add-to-history (content)
  "Add CONTENT to input history."
  (when (and content (not (string-empty-p content)))
    (push content claude-cli-chat-input--history)
    ;; Keep history bounded
    (when (> (length claude-cli-chat-input--history) 100)
      (setq claude-cli-chat-input--history
            (seq-take claude-cli-chat-input--history 100))))
  (setq claude-cli-chat-input--history-index -1))

(defun claude-cli-chat-input--history-previous ()
  "Replace input with previous history item."
  (interactive)
  (when claude-cli-chat-input--history
    (let ((new-index (min (1+ claude-cli-chat-input--history-index)
                          (1- (length claude-cli-chat-input--history)))))
      (setq claude-cli-chat-input--history-index new-index)
      (claude-cli-chat-input--set-content
       (nth new-index claude-cli-chat-input--history)))))

(defun claude-cli-chat-input--history-next ()
  "Replace input with next history item, or clear if at end."
  (interactive)
  (cond
   ((< claude-cli-chat-input--history-index 0)
    ;; Not in history mode
    nil)
   ((= claude-cli-chat-input--history-index 0)
    ;; At most recent, clear and exit history mode
    (setq claude-cli-chat-input--history-index -1)
    (claude-cli-chat-input--clear))
   (t
    ;; Go to more recent entry
    (cl-decf claude-cli-chat-input--history-index)
    (claude-cli-chat-input--set-content
     (nth claude-cli-chat-input--history-index claude-cli-chat-input--history)))))

(provide 'claude-cli-chat-input)
;;; claude-cli-chat-input.el ends here
