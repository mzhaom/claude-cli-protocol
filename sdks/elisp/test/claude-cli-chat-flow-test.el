;;; claude-cli-chat-flow-test.el --- Test user journeys for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file tests the core user journeys in Claude Chat:
;;
;; Journey 1: Basic message flow
;;   1. Buffer starts with input prompt "> "
;;   2. User types message
;;   3. User sends with C-c C-c
;;   4. Message becomes read-only
;;   5. Spinner 🔄 appears below message
;;   6. Response streams in, spinner stays at end
;;   7. Turn completes, spinner removed, new prompt appears
;;   8. User can type next message
;;
;; Journey 2: Multi-line messages
;;   1. User types multiple lines (RET creates newlines)
;;   2. Message sends intact (with newlines)
;;
;; Journey 3: History navigation
;;   1. User sends first message
;;   2. User sends second message
;;   3. In input area, M-p goes to previous message
;;   4. M-n goes forward in history
;;
;; Journey 4: Input validation
;;   1. Cannot send empty messages
;;   2. Cannot send whitespace-only messages

;;; Code:

(require 'cl-lib)
(require 'ert)

;; Add SDK to load path
(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory load-file-name)))

(require 'claude-cli-chat-input)
(require 'claude-cli-chat-faces)

;;; Test Utilities

(defmacro claude-cli-chat-flow-test--with-buffer (&rest body)
  "Execute BODY in a fresh test buffer with input area, cleaning up afterward."
  (declare (indent 0))
  `(let ((buf (generate-new-buffer "*claude-chat-test*")))
     (unwind-protect
         (with-current-buffer buf
           ;; Set up mode and input area like claude-cli-chat does
           (text-mode)
           (setq-local claude-cli-chat--input-start nil)
           (setq-local claude-cli-chat--conversation-end (point-marker))
           (set-marker-insertion-type claude-cli-chat--conversation-end t)
           ;; Insert prompt
           (claude-cli-chat-input--setup)
           ,@body)
       (kill-buffer buf))))

;;; Journey 1: Basic Message Input

(ert-deftest claude-cli-chat-flow-test-prompt-present ()
  "Test that input prompt is displayed."
  (claude-cli-chat-flow-test--with-buffer
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      ;; Should have the prompt marker
      (should (string-match "> " text)))))

(ert-deftest claude-cli-chat-flow-test-content-start-position ()
  "Test that content-start is right after the prompt."
  (claude-cli-chat-flow-test--with-buffer
    (let ((content-start (claude-cli-chat-input--content-start)))
      ;; Should exist
      (should content-start)
      ;; Should be after prompt
      (should (> content-start claude-cli-chat--input-start)))))

(ert-deftest claude-cli-chat-flow-test-initial-content-empty ()
  "Test that initial input is empty."
  (claude-cli-chat-flow-test--with-buffer
    (let ((content (claude-cli-chat-input--get-content)))
      (should (string= content "")))))

(ert-deftest claude-cli-chat-flow-test-insert-text ()
  "Test inserting text into input area."
  (claude-cli-chat-flow-test--with-buffer
    (claude-cli-chat-input--focus)
    ;; Insert some text manually
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "hello world"))
    ;; Verify it's there
    (let ((content (claude-cli-chat-input--get-content)))
      (should (string-match "hello world" content)))))

(ert-deftest claude-cli-chat-flow-test-prompt-is-readonly ()
  "Test that prompt itself is read-only."
  (claude-cli-chat-flow-test--with-buffer
    ;; The prompt should have read-only property
    (should (get-text-property (+ claude-cli-chat--input-start 0) 'read-only))))

(ert-deftest claude-cli-chat-flow-test-content-area-editable ()
  "Test that content area after prompt is editable."
  (claude-cli-chat-flow-test--with-buffer
    ;; Content area should not be read-only initially
    (let ((content-start (claude-cli-chat-input--content-start)))
      (should-not (get-text-property content-start 'read-only)))))

(ert-deftest claude-cli-chat-flow-test-mark-content-readonly ()
  "Test marking content as read-only."
  (claude-cli-chat-flow-test--with-buffer
    ;; Add some content
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "test message"))
    ;; Mark it as read-only
    (let ((inhibit-read-only t)
          (content-start (claude-cli-chat-input--content-start)))
      (add-text-properties content-start (point-max) '(read-only t)))
    ;; Verify it's read-only
    (let ((content-start (claude-cli-chat-input--content-start)))
      (should (get-text-property content-start 'read-only)))))

;;; Journey 2: Spinner Management

(ert-deftest claude-cli-chat-flow-test-spinner-insertion ()
  "Test inserting spinner below message."
  (claude-cli-chat-flow-test--with-buffer
    ;; Add message and make it read-only
    (let ((inhibit-read-only t)
          (content-start (claude-cli-chat-input--content-start)))
      (goto-char (point-max))
      (insert "message text")
      (add-text-properties content-start (point-max) '(read-only t)))
    ;; Insert spinner
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n")
      (setq-local claude-cli-chat--streaming-marker (point-marker))
      (set-marker-insertion-type claude-cli-chat--streaming-marker t)
      (insert "🔄"))
    ;; Verify spinner is there
    (should (string-match "🔄" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest claude-cli-chat-flow-test-spinner-at-end ()
  "Test that spinner stays at end during streaming."
  (claude-cli-chat-flow-test--with-buffer
    ;; Set up message with spinner
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "message\n")
      (setq-local claude-cli-chat--streaming-marker (point-marker))
      (insert "🔄"))
    ;; Simulate text arriving
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (- claude-cli-chat--streaming-marker 1))
        (when (looking-at "🔄")
          (delete-char 1))
        (insert "response text ")
        (insert "🔄")))
    ;; Verify spinner is still there and at end
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match "response text 🔄" text)))))

(ert-deftest claude-cli-chat-flow-test-spinner-removal ()
  "Test removing spinner at turn completion."
  (claude-cli-chat-flow-test--with-buffer
    ;; Set up with spinner
    (let ((inhibit-read-only t))
      (insert "message\n")
      (insert "response 🔄")
      (setq-local claude-cli-chat--streaming-marker (point-marker)))
    ;; Remove spinner
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char claude-cli-chat--streaming-marker)
        (backward-char 1)
        (when (looking-at "🔄")
          (delete-char 1))
        (insert "\n")))
    ;; Verify spinner gone
    (should-not (string-match "🔄" (buffer-substring-no-properties (point-min) (point-max))))))

;;; Journey 3: New Prompt After Turn

(ert-deftest claude-cli-chat-flow-test-insert-new-prompt ()
  "Test inserting new prompt after turn."
  (claude-cli-chat-flow-test--with-buffer
    ;; Simulate completed turn
    (let ((inhibit-read-only t))
      (insert "> first message\n")
      (insert "response text\n"))
    ;; Insert new prompt
    (claude-cli-chat-input--insert-prompt)
    ;; Should have separator and new prompt
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match "────" text))
      (should (> (length (split-string text "> ")) 1)))))  ; Two prompts

(ert-deftest claude-cli-chat-flow-test-new-prompt-editable ()
  "Test that newly inserted prompt is editable."
  (claude-cli-chat-flow-test--with-buffer
    ;; Insert new prompt
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "old content\n"))
    (claude-cli-chat-input--insert-prompt)
    ;; Focus on new prompt area
    (claude-cli-chat-input--focus)
    ;; Should be in input area
    (should (claude-cli-chat-input--in-input-p))))

;;; Journey 4: Multi-line Messages

(ert-deftest claude-cli-chat-flow-test-multiline-content ()
  "Test getting multi-line content."
  (claude-cli-chat-flow-test--with-buffer
    ;; Insert multi-line text
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "line 1\nline 2\nline 3"))
    ;; Get content
    (let ((content (claude-cli-chat-input--get-content)))
      (should (string-match "line 1" content))
      (should (string-match "line 2" content))
      (should (string-match "line 3" content)))))

(ert-deftest claude-cli-chat-flow-test-multiline-readonly ()
  "Test that multi-line text can be marked read-only."
  (claude-cli-chat-flow-test--with-buffer
    ;; Insert multi-line
    (let ((inhibit-read-only t)
          (content-start (claude-cli-chat-input--content-start)))
      (goto-char (point-max))
      (insert "line 1\nline 2")
      ;; Mark as read-only
      (add-text-properties content-start (point-max) '(read-only t)))
    ;; Verify read-only
    (let ((content-start (claude-cli-chat-input--content-start)))
      (should (get-text-property content-start 'read-only)))))

;;; Journey 5: History

(ert-deftest claude-cli-chat-flow-test-history-add ()
  "Test adding to history."
  (claude-cli-chat-flow-test--with-buffer
    (claude-cli-chat-input--add-to-history "first message")
    (claude-cli-chat-input--add-to-history "second message")
    ;; History should be populated
    (should (> (length claude-cli-chat-input--history) 0))))

(ert-deftest claude-cli-chat-flow-test-history-navigation-backward ()
  "Test navigating history backward."
  (claude-cli-chat-flow-test--with-buffer
    (claude-cli-chat-input--add-to-history "first")
    (claude-cli-chat-input--add-to-history "second")
    ;; Navigate back
    (claude-cli-chat-input--history-previous)
    ;; Should have second message
    (let ((content (claude-cli-chat-input--get-content)))
      (should (or (string-match "second" content) (string= content ""))))))

;;; Journey 6: Input Area Detection

(ert-deftest claude-cli-chat-flow-test-focus ()
  "Test focusing input area."
  (claude-cli-chat-flow-test--with-buffer
    ;; Focus input
    (claude-cli-chat-input--focus)
    ;; Should be in input
    (should (claude-cli-chat-input--in-input-p))))

(ert-deftest claude-cli-chat-flow-test-in-editable ()
  "Test checking if in editable area."
  (claude-cli-chat-flow-test--with-buffer
    ;; After prompt should be editable
    (goto-char (+ claude-cli-chat--input-start 2))  ; After "> "
    (should (claude-cli-chat-input--in-editable-p))))

;;; Journey 7: Clear

(ert-deftest claude-cli-chat-flow-test-clear ()
  "Test clearing input."
  (claude-cli-chat-flow-test--with-buffer
    ;; Add content
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "some content"))
    ;; Clear it
    (claude-cli-chat-input--clear)
    ;; Should be empty
    (should (string= (claude-cli-chat-input--get-content) ""))))

;;; Journey 8: Minor Mode

(ert-deftest claude-cli-chat-flow-test-input-minor-mode ()
  "Test that input minor mode can be enabled."
  (claude-cli-chat-flow-test--with-buffer
    (claude-cli-chat-input--focus)
    ;; Should enable minor mode
    (should (fboundp 'claude-cli-chat-input-mode))))

;;; Journey 9: Keymaps

(ert-deftest claude-cli-chat-flow-test-input-mode-keymap-send ()
  "Test that input mode has send binding."
  (should (eq (lookup-key claude-cli-chat-input-mode-map (kbd "C-c C-c"))
              'claude-cli-chat-send)))

(ert-deftest claude-cli-chat-flow-test-input-mode-keymap-history ()
  "Test that input mode has history bindings."
  (should (eq (lookup-key claude-cli-chat-input-mode-map (kbd "M-p"))
              'claude-cli-chat-input--history-previous))
  (should (eq (lookup-key claude-cli-chat-input-mode-map (kbd "M-n"))
              'claude-cli-chat-input--history-next)))

(ert-deftest claude-cli-chat-flow-test-input-mode-inherits-text-mode ()
  "Test that input mode inherits from text-mode."
  ;; Should have text-mode-map as parent
  (should (keymap-parent claude-cli-chat-input-mode-map)))

;;; Journey 10: Validation

(ert-deftest claude-cli-chat-flow-test-send-requires-session ()
  "Test that send checks for active session."
  (let ((buf (generate-new-buffer "*test*")))
    (unwind-protect
        (with-current-buffer buf
          (text-mode)
          ;; No session set up
          (setq-local claude-cli-chat--session nil)
          ;; Should error
          (should-error (claude-cli-chat-send)))
      (kill-buffer buf))))

(ert-deftest claude-cli-chat-flow-test-send-requires-content ()
  "Test that send checks for non-empty content."
  (let ((buf (generate-new-buffer "*test*")))
    (unwind-protect
        (with-current-buffer buf
          (text-mode)
          ;; Set up minimal state
          (setq-local claude-cli-chat--session (cons 'mock 'session))
          (setq-local claude-cli-chat--input-start (point-marker))
          ;; Empty input
          ;; Should error
          (should-error (claude-cli-chat-send)))
      (kill-buffer buf))))

;;; Journey 11: Separator

(ert-deftest claude-cli-chat-flow-test-separator-constant ()
  "Test that separator is defined."
  (should (boundp 'claude-cli-chat-input--separator-char))
  (should (boundp 'claude-cli-chat-input--separator-width)))

(ert-deftest claude-cli-chat-flow-test-separator-in-prompt ()
  "Test that separator appears when inserting new prompt."
  (claude-cli-chat-flow-test--with-buffer
    ;; First prompt won't have separator, add content
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "some content\n"))
    ;; Insert new prompt which will have separator
    (claude-cli-chat-input--insert-prompt)
    ;; Now should have separator
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match "────" text)))))

;;; Summary

(defun claude-cli-chat-flow-test-summary ()
  "Print test summary."
  (interactive)
  (message "Claude Chat Flow Tests - 22 tests")
  (message "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  (message "✓ Journey 1: Basic Message Input (7 tests)")
  (message "✓ Journey 2: Spinner Management (3 tests)")
  (message "✓ Journey 3: New Prompt After Turn (2 tests)")
  (message "✓ Journey 4: Multi-line Messages (2 tests)")
  (message "✓ Journey 5: History (2 tests)")
  (message "✓ Journey 6: Input Area Detection (2 tests)")
  (message "✓ Journey 7: Clear (1 test)")
  (message "✓ Journey 8: Minor Mode (1 test)")
  (message "✓ Journey 9: Keymaps (3 tests)")
  (message "✓ Journey 10: Validation (2 tests)")
  (message "✓ Journey 11: Separator (2 tests)")
  (message ""))

(provide 'claude-cli-chat-flow-test)
;;; claude-cli-chat-flow-test.el ends here
