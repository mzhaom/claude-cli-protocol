;;; claude-cli-chat-navigation.el --- Navigation commands for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file provides navigation commands for moving through the
;; Claude Chat conversation buffer.  Navigation uses magit-section
;; for section-based movement.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'claude-cli-chat-sections)

;; Forward declarations
(declare-function claude-cli-chat-input--in-input-p "claude-cli-chat-input")

;; Buffer-local variables
(defvar claude-cli-chat--conversation-start)
(defvar claude-cli-chat--input-start)

;;; Section Navigation

(defun claude-cli-chat-navigation-next-section ()
  "Move to the next section."
  (interactive)
  (condition-case nil
      (magit-section-forward)
    (error (message "No more sections"))))

(defun claude-cli-chat-navigation-previous-section ()
  "Move to the previous section."
  (interactive)
  (condition-case nil
      (magit-section-backward)
    (error (message "No previous section"))))

(defun claude-cli-chat-navigation-section-up ()
  "Move to parent section."
  (interactive)
  (let ((section (magit-current-section)))
    (if (and section (oref section parent))
        (goto-char (oref (oref section parent) start))
      (message "No parent section"))))

;;; Turn Navigation

(defun claude-cli-chat-navigation-next-turn (&optional n)
  "Move to the next turn section.
With prefix N, move N turns forward."
  (interactive "p")
  (claude-cli-chat-navigation--move-to-turn (or n 1)))

(defun claude-cli-chat-navigation-previous-turn (&optional n)
  "Move to the previous turn section.
With prefix N, move N turns backward."
  (interactive "p")
  (claude-cli-chat-navigation--move-to-turn (- (or n 1))))

(defun claude-cli-chat-navigation--move-to-turn (direction)
  "Move DIRECTION turns (positive = forward, negative = backward)."
  (let ((turns (claude-cli-chat-navigation--find-turn-sections))
        (current-pos (point))
        (target nil))
    (if (null turns)
        (message "No turns found")
      (if (> direction 0)
          ;; Moving forward
          (setq target
                (cl-find-if (lambda (section)
                              (> (oref section start) current-pos))
                            turns))
        ;; Moving backward
        (setq target
              (cl-find-if (lambda (section)
                            (< (oref section start) current-pos))
                          (reverse turns))))
      (if target
          (goto-char (oref target start))
        (message "No more turns in that direction")))))

(defun claude-cli-chat-navigation--find-turn-sections ()
  "Find all turn sections in the buffer."
  (let ((sections nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((section (magit-current-section)))
          (when (and section (cl-typep section 'claude-cli-chat-turn-section))
            (push section sections)))
        (condition-case nil
            (magit-section-forward)
          (error (goto-char (point-max))))))
    (nreverse sections)))

;;; Section Visibility

(defun claude-cli-chat-navigation-toggle-section ()
  "Toggle visibility of current section."
  (interactive)
  (let ((section (magit-current-section)))
    (if section
        (if (oref section hidden)
            (magit-section-show section)
          (magit-section-hide section))
      (message "No section at point"))))

(defun claude-cli-chat-navigation-expand-all ()
  "Expand all sections in the buffer."
  (interactive)
  (magit-section-show-level-4-all))

(defun claude-cli-chat-navigation-collapse-all ()
  "Collapse all sections to turn level."
  (interactive)
  (magit-section-hide-level-1-all))

(defun claude-cli-chat-navigation-expand-turn ()
  "Expand all subsections of current turn."
  (interactive)
  (let ((section (claude-cli-chat-navigation--current-turn-section)))
    (if section
        (magit-section-show-children section)
      (message "Not in a turn section"))))

(defun claude-cli-chat-navigation-collapse-turn ()
  "Collapse all subsections of current turn."
  (interactive)
  (let ((section (claude-cli-chat-navigation--current-turn-section)))
    (if section
        (magit-section-hide-children section)
      (message "Not in a turn section"))))

(defun claude-cli-chat-navigation--current-turn-section ()
  "Get the turn section containing point."
  (let ((section (magit-current-section)))
    (while (and section (not (cl-typep section 'claude-cli-chat-turn-section)))
      (setq section (oref section parent)))
    section))

;;; Jump Commands

(defun claude-cli-chat-navigation-jump-to-tool ()
  "Jump to the next tool section."
  (interactive)
  (claude-cli-chat-navigation--jump-to-section-type 'claude-cli-chat-tool-section))

(defun claude-cli-chat-navigation-jump-to-thinking ()
  "Jump to the next thinking section."
  (interactive)
  (claude-cli-chat-navigation--jump-to-section-type 'claude-cli-chat-thinking-section))

(defun claude-cli-chat-navigation-jump-to-error ()
  "Jump to the next error section."
  (interactive)
  (claude-cli-chat-navigation--jump-to-section-type 'claude-cli-chat-error-section))

(defun claude-cli-chat-navigation-jump-to-input ()
  "Jump to the input area."
  (interactive)
  (when claude-cli-chat--input-start
    (goto-char claude-cli-chat--input-start)
    ;; Skip prompt
    (when (looking-at "> ")
      (forward-char 2))))

(defun claude-cli-chat-navigation-jump-to-start ()
  "Jump to the start of the conversation."
  (interactive)
  (when claude-cli-chat--conversation-start
    (goto-char claude-cli-chat--conversation-start)))

(defun claude-cli-chat-navigation--jump-to-section-type (type)
  "Jump to the next section of TYPE."
  (let ((current-pos (point))
        (target nil))
    (save-excursion
      (goto-char current-pos)
      (while (and (not target) (not (eobp)))
        (condition-case nil
            (progn
              (magit-section-forward)
              (let ((section (magit-current-section)))
                (when (and section
                           (cl-typep section type)
                           (> (oref section start) current-pos))
                  (setq target section))))
          (error (goto-char (point-max))))))
    (if target
        (goto-char (oref target start))
      (message "No more %s sections" (symbol-name type)))))

;;; Search/Filter

(defun claude-cli-chat-navigation-occur (regexp)
  "Show all lines matching REGEXP in the conversation."
  (interactive
   (list (read-regexp "Search for: " nil)))
  (occur regexp))

(defun claude-cli-chat-navigation-search-forward (string)
  "Search forward for STRING in the conversation."
  (interactive "sSearch: ")
  (let ((case-fold-search t))
    (if (search-forward string nil t)
        (goto-char (match-beginning 0))
      (message "Not found: %s" string))))

(defun claude-cli-chat-navigation-search-backward (string)
  "Search backward for STRING in the conversation."
  (interactive "sSearch backward: ")
  (let ((case-fold-search t))
    (if (search-backward string nil t)
        (goto-char (match-beginning 0))
      (message "Not found: %s" string))))

;;; Bookmarking

(defvar-local claude-cli-chat-navigation--bookmarks nil
  "List of bookmarked positions in the conversation.")

(defun claude-cli-chat-navigation-bookmark-set ()
  "Set a bookmark at current position."
  (interactive)
  (let ((pos (point))
        (name (format "Turn at %d" (line-number-at-pos))))
    (push (cons name pos) claude-cli-chat-navigation--bookmarks)
    (message "Bookmark set: %s" name)))

(defun claude-cli-chat-navigation-bookmark-jump ()
  "Jump to a bookmark."
  (interactive)
  (if (null claude-cli-chat-navigation--bookmarks)
      (message "No bookmarks set")
    (let* ((names (mapcar #'car claude-cli-chat-navigation--bookmarks))
           (choice (completing-read "Jump to bookmark: " names nil t))
           (entry (assoc choice claude-cli-chat-navigation--bookmarks)))
      (when entry
        (goto-char (cdr entry))))))

;;; Imenu Integration

(defun claude-cli-chat-navigation--imenu-create-index ()
  "Create imenu index for Claude chat buffer."
  (let ((index nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((section (magit-current-section)))
          (when (and section (cl-typep section 'claude-cli-chat-turn-section))
            (let* ((turn-num (oref section turn-number))
                   (name (format "Turn %d" turn-num))
                   (pos (oref section start)))
              (push (cons name pos) index))))
        (condition-case nil
            (magit-section-forward)
          (error (goto-char (point-max))))))
    (nreverse index)))

(defun claude-cli-chat-navigation-setup-imenu ()
  "Set up imenu for the current buffer."
  (setq-local imenu-create-index-function
              #'claude-cli-chat-navigation--imenu-create-index)
  (setq-local imenu-auto-rescan t))

(provide 'claude-cli-chat-navigation)
;;; claude-cli-chat-navigation.el ends here
