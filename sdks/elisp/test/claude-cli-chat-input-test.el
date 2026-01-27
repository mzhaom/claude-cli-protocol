;;; claude-cli-chat-input-test.el --- Tests for input area -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Unit tests for claude-cli-chat-input module.
;; Tests the eshell-style input area with minor mode.

;;; Code:

(require 'ert)

;; Add SDK to load path
(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory load-file-name)))

(require 'claude-cli-chat)
(require 'claude-cli-chat-input)

;;; Test Utilities

(defmacro claude-cli-chat-input-test--with-buffer (&rest body)
  "Execute BODY in a temporary claude-cli-chat buffer."
  (declare (indent 0))
  `(let ((buf (generate-new-buffer "*claude-chat-test*")))
     (unwind-protect
         (with-current-buffer buf
           ;; Set up the mode without starting a real session
           (claude-cli-chat-mode)
           ;; Manually set up input area since we don't have a real session
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert "Welcome to Claude Chat\n")
             (setq claude-cli-chat--conversation-end (point-marker)))
           (claude-cli-chat-input--setup)
           ,@body)
       (kill-buffer buf))))

;;; Basic Setup Tests

(ert-deftest claude-cli-chat-input-test-setup ()
  "Test that input area is set up correctly."
  (claude-cli-chat-input-test--with-buffer
    ;; Verify input start marker exists
    (should claude-cli-chat--input-start)
    ;; Verify prompt is present
    (should (string-match-p "^> " (buffer-substring claude-cli-chat--input-start
                                                     (point-max))))
    ;; Verify minor mode is enabled
    (should claude-cli-chat-input-mode)))

(ert-deftest claude-cli-chat-input-test-prompt-is-readonly ()
  "Test that the prompt is read-only."
  (claude-cli-chat-input-test--with-buffer
    (goto-char claude-cli-chat--input-start)
    ;; The prompt "> " should be read-only
    (should (get-text-property (point) 'read-only))))

(ert-deftest claude-cli-chat-input-test-input-area-editable ()
  "Test that the input area after prompt is editable."
  (claude-cli-chat-input-test--with-buffer
    ;; Focus on input
    (claude-cli-chat-input--focus)
    ;; Should be in editable area
    (should (claude-cli-chat-input--in-editable-p))
    ;; Should be able to insert text
    (let ((inhibit-read-only t))
      (insert "test message"))
    (should (string= (claude-cli-chat-input--get-content) "test message"))))

;;; Content Tests

(ert-deftest claude-cli-chat-input-test-get-content ()
  "Test getting input content."
  (claude-cli-chat-input-test--with-buffer
    (claude-cli-chat-input--focus)
    ;; Initially empty
    (should (string= (claude-cli-chat-input--get-content) ""))
    ;; Insert content
    (let ((inhibit-read-only t))
      (insert "hello world"))
    (should (string= (claude-cli-chat-input--get-content) "hello world"))))

(ert-deftest claude-cli-chat-input-test-multiline-content ()
  "Test multi-line input content."
  (claude-cli-chat-input-test--with-buffer
    (claude-cli-chat-input--focus)
    (let ((inhibit-read-only t))
      (insert "line 1\nline 2\nline 3"))
    (should (string= (claude-cli-chat-input--get-content) "line 1\nline 2\nline 3"))))

(ert-deftest claude-cli-chat-input-test-clear-content ()
  "Test clearing input content."
  (claude-cli-chat-input-test--with-buffer
    (claude-cli-chat-input--focus)
    (let ((inhibit-read-only t))
      (insert "some text"))
    (should (not (string-empty-p (claude-cli-chat-input--get-content))))
    (claude-cli-chat-input--clear)
    (should (string-empty-p (claude-cli-chat-input--get-content)))))

(ert-deftest claude-cli-chat-input-test-set-content ()
  "Test setting input content."
  (claude-cli-chat-input-test--with-buffer
    (claude-cli-chat-input--set-content "new content")
    (should (string= (claude-cli-chat-input--get-content) "new content"))))

;;; Navigation Tests

(ert-deftest claude-cli-chat-input-test-focus ()
  "Test focusing on input area."
  (claude-cli-chat-input-test--with-buffer
    ;; Go somewhere else
    (goto-char (point-min))
    (should-not (claude-cli-chat-input--in-input-p))
    ;; Focus input
    (claude-cli-chat-input--focus)
    (should (claude-cli-chat-input--in-input-p))
    (should (claude-cli-chat-input--in-editable-p))))

(ert-deftest claude-cli-chat-input-test-in-input-p ()
  "Test input area detection."
  (claude-cli-chat-input-test--with-buffer
    ;; At beginning, not in input
    (goto-char (point-min))
    (should-not (claude-cli-chat-input--in-input-p))
    ;; At input start, in input
    (goto-char claude-cli-chat--input-start)
    (should (claude-cli-chat-input--in-input-p))))

;;; State Control Tests

(ert-deftest claude-cli-chat-input-test-new-prompt ()
  "Test inserting a new prompt."
  (claude-cli-chat-input-test--with-buffer
    ;; Add some content
    (claude-cli-chat-input--focus)
    (let ((inhibit-read-only t))
      (insert "test content"))
    ;; Mark current input as read-only
    (let ((inhibit-read-only t)
          (start (claude-cli-chat-input--content-start)))
      (add-text-properties start (point-max) '(read-only t)))
    ;; Clear input area and insert new prompt
    (let ((inhibit-read-only t))
      (delete-region claude-cli-chat--input-start (point-max)))
    (claude-cli-chat-input--insert-prompt)
    ;; Should have new empty prompt
    (should (string= (claude-cli-chat-input--get-content) ""))
    ;; Should be editable
    (should (claude-cli-chat-input--in-editable-p))))

;;; History Tests

(ert-deftest claude-cli-chat-input-test-history ()
  "Test input history."
  (claude-cli-chat-input-test--with-buffer
    ;; Add to history
    (claude-cli-chat-input--add-to-history "first message")
    (claude-cli-chat-input--add-to-history "second message")
    ;; Navigate history
    (claude-cli-chat-input--history-previous)
    (should (string= (claude-cli-chat-input--get-content) "second message"))
    (claude-cli-chat-input--history-previous)
    (should (string= (claude-cli-chat-input--get-content) "first message"))
    ;; Go back
    (claude-cli-chat-input--history-next)
    (should (string= (claude-cli-chat-input--get-content) "second message"))))

;;; Minor Mode Tests

(ert-deftest claude-cli-chat-input-test-minor-mode-keymap ()
  "Test that minor mode keymap has correct bindings."
  (should (eq (lookup-key claude-cli-chat-input-mode-map (kbd "C-c C-c"))
              'claude-cli-chat-send))
  (should (eq (lookup-key claude-cli-chat-input-mode-map (kbd "M-p"))
              'claude-cli-chat-input--history-previous))
  (should (eq (lookup-key claude-cli-chat-input-mode-map (kbd "M-n"))
              'claude-cli-chat-input--history-next)))

(ert-deftest claude-cli-chat-input-test-text-mode-parent ()
  "Test that input keymap inherits from text-mode."
  ;; The keymap should have text-mode-map as parent (set at definition time)
  (should (keymap-parent claude-cli-chat-input-mode-map)))

(provide 'claude-cli-chat-input-test)
;;; claude-cli-chat-input-test.el ends here
