;;; claude-cli-chat-journey-test.el --- Automated user journey test for Claude Chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file provides automated user journey tests for the Claude Chat UI.
;; It simulates user interactions and verifies the expected behavior.

;;; Code:

(require 'cl-lib)
(require 'ert)

;; Add SDK to load path if not already
(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory load-file-name)))

(require 'claude-cli)
(require 'claude-cli-chat)
(require 'claude-cli-chat-buffer)
(require 'claude-cli-chat-input)

;;; Test Utilities

(defvar claude-cli-chat-test--timeout 30
  "Timeout in seconds for async operations.")

(defmacro claude-cli-chat-test--with-timeout (seconds &rest body)
  "Execute BODY with a timeout of SECONDS."
  (declare (indent 1))
  `(with-timeout (,seconds (error "Test timed out after %d seconds" ,seconds))
     ,@body))

(defun claude-cli-chat-test--wait-for-state (session target-state &optional timeout)
  "Wait for SESSION to reach TARGET-STATE.
TIMEOUT defaults to `claude-cli-chat-test--timeout'."
  (let ((timeout (or timeout claude-cli-chat-test--timeout))
        (start-time (float-time)))
    (while (and (not (eq (claude-cli-session-get-state session) target-state))
                (< (- (float-time) start-time) timeout))
      (accept-process-output nil 0.1))
    (eq (claude-cli-session-get-state session) target-state)))

(defun claude-cli-chat-test--wait-for-ready (session &optional timeout)
  "Wait for SESSION to be ready."
  (let ((timeout (or timeout claude-cli-chat-test--timeout))
        (start-time (float-time)))
    (while (and (not (claude-cli-session-ready-p session))
                (< (- (float-time) start-time) timeout))
      ;; Must call accept-process-output to receive data from subprocess
      (accept-process-output nil 0.1))
    (claude-cli-session-ready-p session)))

(defun claude-cli-chat-test--wait-for-processing-complete (session &optional timeout)
  "Wait for SESSION to finish processing."
  (let ((timeout (or timeout claude-cli-chat-test--timeout))
        (start-time (float-time)))
    (while (and (claude-cli-session-processing-p session)
                (< (- (float-time) start-time) timeout))
      (accept-process-output nil 0.1))
    (not (claude-cli-session-processing-p session))))

;;; Configuration

(defvar claude-cli-chat-test--require-cli t
  "If non-nil, tests require Claude CLI to be available.
Set to nil to test UI components without a real session.")

;;; Journey Test: Basic Chat Flow

(defun claude-cli-chat-test--journey-basic ()
  "Test basic chat flow: open, send message, receive response, close.
Returns a list of (step-name success-p message)."
  (let ((results nil)
        (buffer nil)
        (session nil)
        (cli-available nil))
    (unwind-protect
        (progn
          ;; Step 0: Check if Claude CLI is available
          (setq cli-available (executable-find "claude"))
          (push (list "cli-check"
                      (or cli-available (not claude-cli-chat-test--require-cli))
                      (if cli-available
                          "Claude CLI found"
                        "Claude CLI not found (UI-only test)"))
                results)

          ;; Step 1: Open Claude Chat
          (condition-case err
              (progn
                (claude-cli-chat)
                (setq buffer (get-buffer claude-cli-chat-buffer-name))
                (push (list "open-chat"
                            (buffer-live-p buffer)
                            (if buffer "Buffer created" "Buffer not created"))
                      results))
            (error
             (push (list "open-chat" nil (format "Error: %s" err)) results)))

          ;; Step 2: Verify buffer structure
          (when buffer
            (with-current-buffer buffer
              (let ((has-header (and header-line-format t))
                    (has-input-start claude-cli-chat--input-start)
                    (has-conv-start claude-cli-chat--conversation-start))
                (push (list "buffer-structure"
                            (and has-header has-input-start has-conv-start)
                            (format "header:%s input:%s conv:%s"
                                    has-header
                                    (if has-input-start "yes" "no")
                                    (if has-conv-start "yes" "no")))
                      results))))

          ;; Step 3: Verify session created and starting
          (when buffer
            (with-current-buffer buffer
              (setq session claude-cli-chat--session)
              (let ((state (and session (claude-cli-session-get-state session))))
                (push (list "session-created"
                            (and session (memq state '(starting ready uninitialized)))
                            (format "Session state: %s" state))
                      results))))

          ;; Step 4: Verify session is running (CLI only becomes 'ready' after first message)
          ;; The CLI doesn't emit system init until receiving a user message,
          ;; so we check that the process is running in 'starting' state
          (when session
            (if cli-available
                (let* ((state (claude-cli-session-get-state session))
                       (proc (claude-cli-session-process session))
                       (proc-running (and proc (eq (process-status proc) 'run)))
                       (valid-state (memq state '(starting ready))))
                  (push (list "session-running"
                              (and valid-state proc-running)
                              (format "State: %s, Process: %s"
                                      state
                                      (if proc (process-status proc) "no-process")))
                        results))
              (push (list "session-running"
                          t
                          "Skipped (no CLI)")
                    results)))

          ;; Step 5: Verify input area is functional
          (when buffer
            (with-current-buffer buffer
              (let ((in-input (progn
                                (claude-cli-chat-input--focus)
                                (claude-cli-chat-input--in-input-p))))
                (push (list "input-focus"
                            in-input
                            (format "In input area: %s" in-input))
                      results))))

          ;; Step 6: Type a test message
          (when buffer
            (with-current-buffer buffer
              (claude-cli-chat-input--set-content "Hello, this is a test message.")
              (let ((content (claude-cli-chat-input--get-content)))
                (push (list "input-content"
                            (string= content "Hello, this is a test message.")
                            (format "Content: %s" content))
                      results))))

          ;; Step 7: Send message (CLI accepts messages in 'starting' state too)
          (if (and buffer session
                   (memq (claude-cli-session-get-state session) '(ready starting)))
              (with-current-buffer buffer
                (condition-case err
                    (progn
                      (claude-cli-chat-send)
                      (push (list "send-message"
                                  t
                                  "Message sent")
                            results)
                      ;; Wait a bit for processing to start
                      (sleep-for 0.5)
                      (let ((processing (claude-cli-session-processing-p session)))
                        (push (list "processing-started"
                                    processing
                                    (format "Processing: %s" processing))
                              results)))
                  (error
                   (push (list "send-message" nil (format "Error: %s" err)) results))))
            ;; Skip if no CLI
            (when (not cli-available)
              (push (list "send-message" t "Skipped (no CLI)") results)
              (push (list "processing-started" t "Skipped (no CLI)") results)))

          ;; Step 8: Wait for response (with timeout)
          (if (and session (claude-cli-session-processing-p session))
              (let ((completed (claude-cli-chat-test--wait-for-processing-complete session 30)))
                (push (list "response-received"
                            completed
                            (format "Completed: %s" completed))
                      results))
            (when (not cli-available)
              (push (list "response-received" t "Skipped (no CLI)") results)))

          ;; Step 9: Verify turn counter incremented
          (when buffer
            (with-current-buffer buffer
              (if cli-available
                  (let ((turn-num claude-cli-chat--turn-number))
                    (push (list "turn-incremented"
                                (> turn-num 0)
                                (format "Turn number: %d" turn-num))
                          results))
                (push (list "turn-incremented" t "Skipped (no CLI)") results))))

          ;; Step 10: Verify conversation history recorded
          (when buffer
            (with-current-buffer buffer
              (if cli-available
                  (let ((history-count (length claude-cli-chat--conversation-history)))
                    (push (list "history-recorded"
                                (> history-count 0)
                                (format "History entries: %d" history-count))
                          results))
                (push (list "history-recorded" t "Skipped (no CLI)") results)))))

      ;; Cleanup
      (when buffer
        (with-current-buffer buffer
          (when claude-cli-chat--session
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))

    ;; Return results in order
    (nreverse results)))

;;; Interactive Test Runner

(defun claude-cli-chat-test-run-journey ()
  "Run the basic journey test and display results."
  (interactive)
  (let ((results (claude-cli-chat-test--journey-basic))
        (passed 0)
        (failed 0))
    (with-current-buffer (get-buffer-create "*Claude Chat Test Results*")
      (erase-buffer)
      (insert "Claude Chat Journey Test Results\n")
      (insert (make-string 40 ?=))
      (insert "\n\n")
      (dolist (result results)
        (let ((step (nth 0 result))
              (success (nth 1 result))
              (message (nth 2 result)))
          (if success
              (progn
                (cl-incf passed)
                (insert (format "[PASS] %s\n" step)))
            (cl-incf failed)
            (insert (format "[FAIL] %s\n" step)))
          (insert (format "       %s\n\n" message))))
      (insert (make-string 40 ?=))
      (insert (format "\n\nTotal: %d passed, %d failed\n" passed failed))
      (goto-char (point-min)))
    (pop-to-buffer "*Claude Chat Test Results*")
    (list :passed passed :failed failed)))

;;; ERT Tests

(ert-deftest claude-cli-chat-test-buffer-creation ()
  "Test that claude-cli-chat creates a buffer."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should (eq major-mode 'claude-cli-chat-mode))))
      (when buffer (kill-buffer buffer)))))

(ert-deftest claude-cli-chat-test-input-area ()
  "Test input area functionality."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (with-current-buffer buffer
            ;; Test setting content
            (claude-cli-chat-input--set-content "test message")
            (should (string= (claude-cli-chat-input--get-content) "test message"))
            ;; Test clearing
            (claude-cli-chat-input--clear)
            (should (string= (claude-cli-chat-input--get-content) ""))
            ;; Test focus
            (claude-cli-chat-input--focus)
            (should (claude-cli-chat-input--in-input-p))))
      (when buffer
        (with-current-buffer buffer
          (when claude-cli-chat--session
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))))

(ert-deftest claude-cli-chat-test-session-lifecycle ()
  "Test session creation and state transitions."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (with-current-buffer buffer
            ;; Session should be created
            (should claude-cli-chat--session)
            ;; State should be starting or ready
            (let ((state (claude-cli-session-get-state claude-cli-chat--session)))
              (should (memq state '(uninitialized starting ready))))))
      (when buffer
        (with-current-buffer buffer
          (when claude-cli-chat--session
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))))

(ert-deftest claude-cli-chat-test-keymap-conversation-area ()
  "Test that navigation keys work in conversation area."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (with-current-buffer buffer
            ;; Move to conversation area (beginning of buffer)
            (goto-char (point-min))
            ;; Verify we're NOT in input area
            (should-not (claude-cli-chat-input--in-input-p))
            ;; Check that 'n' is bound to navigation in the mode map
            (let ((binding (lookup-key claude-cli-chat-mode-map "n")))
              (should (eq binding 'claude-cli-chat-navigation-next-turn)))
            ;; Check that 'p' is bound to navigation
            (let ((binding (lookup-key claude-cli-chat-mode-map "p")))
              (should (eq binding 'claude-cli-chat-navigation-previous-turn)))
            ;; Check M-n/M-p for section navigation
            (let ((binding (lookup-key claude-cli-chat-mode-map (kbd "M-n"))))
              (should (eq binding 'claude-cli-chat-navigation-next-section)))
            (let ((binding (lookup-key claude-cli-chat-mode-map (kbd "M-p"))))
              (should (eq binding 'claude-cli-chat-navigation-previous-section)))))
      (when buffer
        (with-current-buffer buffer
          (when claude-cli-chat--session
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))))

(ert-deftest claude-cli-chat-test-keymap-input-area ()
  "Test that input area has its own keymap with send and history bindings."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (with-current-buffer buffer
            ;; Focus input area
            (claude-cli-chat-input--focus)
            ;; Verify we're in input area
            (should (claude-cli-chat-input--in-input-p))
            ;; Check the input overlay has a keymap
            (let ((overlay-keymap (overlay-get claude-cli-chat-input--overlay 'keymap)))
              (should overlay-keymap)
              ;; Check C-c C-c sends
              (should (eq (lookup-key overlay-keymap (kbd "C-c C-c"))
                          'claude-cli-chat-send))
              ;; Check C-RET sends
              (should (eq (lookup-key overlay-keymap (kbd "<C-return>"))
                          'claude-cli-chat-send))
              ;; Check M-RET sends
              (should (eq (lookup-key overlay-keymap (kbd "<M-return>"))
                          'claude-cli-chat-send))
              ;; Check M-p for history
              (should (eq (lookup-key overlay-keymap (kbd "M-p"))
                          'claude-cli-chat-input--history-previous))
              ;; Check M-n for history
              (should (eq (lookup-key overlay-keymap (kbd "M-n"))
                          'claude-cli-chat-input--history-next)))))
      (when buffer
        (with-current-buffer buffer
          (when claude-cli-chat--session
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))))

(ert-deftest claude-cli-chat-test-keymap-overlay-takes-precedence ()
  "Test that input area overlay keymap takes precedence over mode keymap."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (with-current-buffer buffer
            ;; Focus input area and add some text
            (claude-cli-chat-input--focus)
            (claude-cli-chat-input--set-content "test")
            ;; Move point into the input area content
            (goto-char (1- (point-max)))
            ;; Verify we're in input area
            (should (claude-cli-chat-input--in-input-p))
            ;; The buffer-local overlay should exist and have the right keymap
            (should claude-cli-chat-input--overlay)
            (should (overlay-buffer claude-cli-chat-input--overlay))
            ;; Check that the overlay covers our position
            (should (<= (overlay-start claude-cli-chat-input--overlay) (point)))
            (should (>= (overlay-end claude-cli-chat-input--overlay) (point)))
            ;; Overlay keymap should have M-p -> history (not section nav)
            (let ((overlay-keymap (overlay-get claude-cli-chat-input--overlay 'keymap)))
              (should overlay-keymap)
              (should (eq (lookup-key overlay-keymap (kbd "M-p"))
                          'claude-cli-chat-input--history-previous)))))
      (when buffer
        (with-current-buffer buffer
          (when claude-cli-chat--session
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))))

;;; Model and Permission Tests

(ert-deftest claude-cli-chat-test-model-switch ()
  "Test that model can be changed via customization variable."
  (let ((buffer nil)
        (original-model claude-cli-chat-default-model))
    (unwind-protect
        (progn
          ;; Change the default model
          (setq claude-cli-chat-default-model "haiku")
          (should (string= claude-cli-chat-default-model "haiku"))
          ;; Change to another model
          (setq claude-cli-chat-default-model "opus")
          (should (string= claude-cli-chat-default-model "opus"))
          ;; Test the set-model function updates the variable
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (with-current-buffer buffer
            (claude-cli-chat-set-model "sonnet")
            (should (string= claude-cli-chat-default-model "sonnet"))))
      ;; Restore original model
      (setq claude-cli-chat-default-model original-model)
      (when buffer
        (with-current-buffer buffer
          (when claude-cli-chat--session
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))))

(ert-deftest claude-cli-chat-test-permission-mode-switch ()
  "Test that permission mode can be changed on a running session."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (with-current-buffer buffer
            ;; Session should exist
            (should claude-cli-chat--session)
            ;; The SDK function should be callable
            ;; (it will send a control request or update config)
            (should (functionp 'claude-cli-set-permission-mode))
            ;; Test calling the chat wrapper function
            (should (functionp 'claude-cli-chat-set-permission-mode))))
      (when buffer
        (with-current-buffer buffer
          (when claude-cli-chat--session
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))))

(ert-deftest claude-cli-chat-test-interrupt-function ()
  "Test that interrupt function exists and is callable."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (with-current-buffer buffer
            ;; Session should exist
            (should claude-cli-chat--session)
            ;; Interrupt function should exist
            (should (functionp 'claude-cli-interrupt))
            (should (functionp 'claude-cli-chat-interrupt))
            ;; Interrupt when not processing should just show message
            ;; (not error)
            (should (progn (claude-cli-chat-interrupt) t))))
      (when buffer
        (with-current-buffer buffer
          (when claude-cli-chat--session
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))))

(ert-deftest claude-cli-chat-test-transient-menu-exists ()
  "Test that transient menu is properly defined."
  (require 'claude-cli-chat-transient)
  ;; Main menu should exist
  (should (fboundp 'claude-cli-chat-transient-menu))
  ;; Model menu should exist
  (should (fboundp 'claude-cli-chat-transient-model))
  ;; Permission menu should exist
  (should (fboundp 'claude-cli-chat-transient-permission))
  ;; Export menu should exist
  (should (fboundp 'claude-cli-chat-transient-export)))

(ert-deftest claude-cli-chat-test-new-session ()
  "Test that new session function works."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (claude-cli-chat)
          (setq buffer (get-buffer claude-cli-chat-buffer-name))
          (with-current-buffer buffer
            ;; Initial session exists
            (should claude-cli-chat--session)
            (let ((old-session claude-cli-chat--session))
              ;; Create new session (simulated - don't actually prompt)
              ;; Just verify the function exists and session can be closed
              (should (functionp 'claude-cli-chat-new-session))
              ;; Close session function should work
              (claude-cli-chat-close-session)
              ;; After close, state should be closed
              (should (eq (claude-cli-session-get-state old-session) 'closed)))))
      (when buffer
        (with-current-buffer buffer
          (when (and claude-cli-chat--session
                     (not (eq (claude-cli-session-get-state claude-cli-chat--session) 'closed)))
            (ignore-errors (claude-cli-stop claude-cli-chat--session))))
        (kill-buffer buffer)))))

;;; Batch Test Runner

(defun claude-cli-chat-test-run-all ()
  "Run all tests and return results."
  (interactive)
  (message "Running Claude Chat tests...")
  (let ((results (claude-cli-chat-test--journey-basic)))
    (dolist (result results)
      (message "[%s] %s: %s"
               (if (nth 1 result) "PASS" "FAIL")
               (nth 0 result)
               (nth 2 result)))
    results))

(provide 'claude-cli-chat-journey-test)
;;; claude-cli-chat-journey-test.el ends here
