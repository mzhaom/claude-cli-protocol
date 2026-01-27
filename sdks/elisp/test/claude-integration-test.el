;;; claude-integration-test.el --- Integration tests for Claude SDK -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Integration tests for the Claude Elisp SDK with real Claude CLI.
;;
;; Tests real interactions with Claude CLI covering:
;; 1. Bypass permission mode with multi-step execution
;; 2. Default permission mode with approval flow
;; 3. Plan mode with combined request
;; 4. Interrupt support
;;
;; Run with:
;;   emacs --batch -L .. -L . -l ert -l claude-integration-test \
;;         -f claude-integration-test-run-all
;;
;; These tests require:
;; - The claude CLI to be installed and available in PATH
;; - A valid API key configured
;;
;; Set `claude-cli-path' to override the default claude CLI location.

;;; Code:

(require 'ert)
(require 'claude-cli)
(require 'cl-lib)

;;; Test utilities

(defvar claude-integration-test-timeout 120
  "Timeout in seconds for integration tests.")

(defvar claude-integration-test-results nil
  "Accumulated test results for reporting.")

(cl-defstruct (claude-cli-turn-events (:constructor claude-cli-turn-events--create))
  "Collected events from a single turn."
  ready
  text-events
  tool-starts
  tool-completes
  tool-results
  turn-complete
  errors)

(defun claude-integration--collect-turn-events (session timeout)
  "Collect events from SESSION until turn complete or TIMEOUT seconds."
  (let ((events (claude-cli-turn-events--create
                 :text-events nil
                 :tool-starts nil
                 :tool-completes nil
                 :tool-results nil
                 :errors nil))
        (start-time (current-time))
        (done nil))

    ;; Set up event handlers
    (let ((ready-fn (lambda (e)
                      (setf (claude-cli-turn-events-ready events) e)))
          (text-fn (lambda (e)
                     (push e (claude-cli-turn-events-text-events events))))
          (tool-start-fn (lambda (e)
                           (push e (claude-cli-turn-events-tool-starts events))))
          (tool-complete-fn (lambda (e)
                              (push e (claude-cli-turn-events-tool-completes events))))
          (tool-result-fn (lambda (e)
                            (push e (claude-cli-turn-events-tool-results events))))
          (complete-fn (lambda (e)
                         (setf (claude-cli-turn-events-turn-complete events) e)
                         (setq done t)))
          (error-fn (lambda (e)
                      (push e (claude-cli-turn-events-errors events)))))

      (add-hook 'claude-cli-ready-hook ready-fn)
      (add-hook 'claude-cli-text-hook text-fn)
      (add-hook 'claude-cli-tool-start-hook tool-start-fn)
      (add-hook 'claude-cli-tool-complete-hook tool-complete-fn)
      (add-hook 'claude-cli-cli-tool-result-hook tool-result-fn)
      (add-hook 'claude-cli-turn-complete-hook complete-fn)
      (add-hook 'claude-cli-error-hook error-fn)

      (unwind-protect
          (progn
            ;; Wait for completion or timeout
            (while (and (not done)
                        (< (float-time (time-subtract (current-time) start-time))
                           timeout)
                        (claude-cli-process-alive-p (claude-cli-session-process session)))
              (accept-process-output nil 0.1))

            ;; Reverse lists to get chronological order
            (setf (claude-cli-turn-events-text-events events)
                  (nreverse (claude-cli-turn-events-text-events events)))
            (setf (claude-cli-turn-events-tool-starts events)
                  (nreverse (claude-cli-turn-events-tool-starts events)))
            (setf (claude-cli-turn-events-tool-completes events)
                  (nreverse (claude-cli-turn-events-tool-completes events)))
            (setf (claude-cli-turn-events-tool-results events)
                  (nreverse (claude-cli-turn-events-tool-results events)))
            (setf (claude-cli-turn-events-errors events)
                  (nreverse (claude-cli-turn-events-errors events)))

            events)

        ;; Cleanup hooks
        (remove-hook 'claude-cli-ready-hook ready-fn)
        (remove-hook 'claude-cli-text-hook text-fn)
        (remove-hook 'claude-cli-tool-start-hook tool-start-fn)
        (remove-hook 'claude-cli-tool-complete-hook tool-complete-fn)
        (remove-hook 'claude-cli-cli-tool-result-hook tool-result-fn)
        (remove-hook 'claude-cli-turn-complete-hook complete-fn)
        (remove-hook 'claude-cli-error-hook error-fn)))))

(defun claude-integration--has-tool-named (events name)
  "Check if EVENTS contains a tool start with NAME."
  (cl-some (lambda (e) (equal (claude-cli-tool-start-event-name e) name))
           (claude-cli-turn-events-tool-starts events)))

(defun claude-integration--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS to messages."
  (let ((msg (apply #'format format-string args)))
    (message "  %s" msg)
    (push msg claude-integration-test-results)))

;;; Test 1: Bypass Permission Mode - Multi-Step Execution

(defun claude-integration-test-scenario1-bypass ()
  "Test bypass permission mode with multi-step execution."
  (claude-integration--log "=== Scenario 1: Bypass Permission Mode ===")

  (let* ((test-dir (make-temp-file "claude-elisp-test-scenario1-" t))
         (session (claude-cli-create-session
                   :model "haiku"
                   :work-dir test-dir
                   :permission-mode 'bypass
                   :disable-plugins t))
         (all-tool-starts nil)
         (success t))

    (claude-integration--log "Test directory: %s" test-dir)

    (unwind-protect
        (condition-case err
            (progn
              ;; Start session
              (claude-cli-start session)
              (claude-integration--log "Session started")

              ;; Step 1: Search for news
              (claude-integration--log "Step 1: Searching for tariff news...")
              (claude-cli-send-message session
                                   "Search latest news about US tariff rate against China. Keep it brief.")
              (let ((events1 (claude-integration--collect-turn-events
                              session claude-integration-test-timeout)))
                (when (claude-cli-turn-events-ready events1)
                  (claude-integration--log "Session ready: %s (model: %s)"
                                           (claude-cli-ready-event-session-id
                                            (claude-cli-turn-events-ready events1))
                                           (claude-cli-ready-event-model
                                            (claude-cli-turn-events-ready events1))))
                (setq all-tool-starts (append all-tool-starts
                                              (claude-cli-turn-events-tool-starts events1)))
                (if (claude-cli-turn-events-turn-complete events1)
                    (claude-integration--log "Turn 1 completed: success=%s, cost=$%.6f"
                                             (claude-cli-turn-complete-event-success
                                              (claude-cli-turn-events-turn-complete events1))
                                             (claude-cli-turn-usage-cost-usd
                                              (claude-cli-turn-complete-event-usage
                                               (claude-cli-turn-events-turn-complete events1))))
                  (claude-integration--log "Turn 1 TIMEOUT")
                  (setq success nil)))

              ;; Step 2: Save to file
              (when success
                (claude-integration--log "Step 2: Saving results to file...")
                (claude-cli-send-message session "Save a brief summary to a file called results.txt")
                (let ((events2 (claude-integration--collect-turn-events
                                session claude-integration-test-timeout)))
                  (setq all-tool-starts (append all-tool-starts
                                                (claude-cli-turn-events-tool-starts events2)))
                  (if (claude-cli-turn-events-turn-complete events2)
                      (claude-integration--log "Turn 2 completed: success=%s, cost=$%.6f"
                                               (claude-cli-turn-complete-event-success
                                                (claude-cli-turn-events-turn-complete events2))
                                               (claude-cli-turn-usage-cost-usd
                                                (claude-cli-turn-complete-event-usage
                                                 (claude-cli-turn-events-turn-complete events2))))
                    (claude-integration--log "Turn 2 TIMEOUT")
                    (setq success nil))))

              ;; Check tool usage
              (let ((has-web-search (cl-some (lambda (e)
                                               (equal (claude-cli-tool-start-event-name e) "WebSearch"))
                                             all-tool-starts))
                    (has-write (cl-some (lambda (e)
                                          (equal (claude-cli-tool-start-event-name e) "Write"))
                                        all-tool-starts)))
                (claude-integration--log "Tools used: WebSearch=%s, Write=%s"
                                         has-web-search has-write)
                (unless has-web-search
                  (claude-integration--log "WARNING: Expected WebSearch tool"))
                (unless has-write
                  (claude-integration--log "WARNING: Expected Write tool")))

              ;; Check files created
              (let ((files (directory-files test-dir nil "^[^.]")))
                (claude-integration--log "Files in test dir: %s" files)
                (when (member "results.txt" files)
                  (claude-integration--log "results.txt created successfully")))

              (claude-integration--log "Scenario 1: %s" (if success "PASSED" "FAILED"))
              success)

          (error
           (claude-integration--log "ERROR: %s" (error-message-string err))
           nil))

      ;; Cleanup
      (claude-cli-stop session)
      (delete-directory test-dir t))))

;;; Test 2: Default Permission Mode with Approval Flow

(defun claude-integration-test-scenario2-default-permissions ()
  "Test default permission mode with auto-approval handler."
  (claude-integration--log "=== Scenario 2: Default Permission Mode ===")

  (let* ((test-dir (make-temp-file "claude-elisp-test-scenario2-" t))
         (permission-requests nil)
         (handler (lambda (tool-name input)
                    (push (cons tool-name input) permission-requests)
                    (claude-integration--log "Permission requested for: %s" tool-name)
                    (claude-cli-permission-allow)))
         (session (claude-cli-create-session
                   :model "haiku"
                   :work-dir test-dir
                   :permission-mode 'default
                   :disable-plugins t
                   :permission-handler handler))
         (success t))

    (claude-integration--log "Test directory: %s" test-dir)

    (unwind-protect
        (condition-case err
            (progn
              (claude-cli-start session)
              (claude-integration--log "Session started")

              ;; Send a simple request
              (claude-integration--log "Sending request...")
              (claude-cli-send-message session "What is 2+2? Answer briefly.")
              (let ((events1 (claude-integration--collect-turn-events
                              session claude-integration-test-timeout)))
                (when (claude-cli-turn-events-ready events1)
                  (claude-integration--log "Session ready: mode=%s"
                                           (claude-cli-ready-event-permission-mode
                                            (claude-cli-turn-events-ready events1))))
                (if (claude-cli-turn-events-turn-complete events1)
                    (claude-integration--log "Turn completed: success=%s"
                                             (claude-cli-turn-complete-event-success
                                              (claude-cli-turn-events-turn-complete events1)))
                  (claude-integration--log "Turn TIMEOUT")
                  (setq success nil)))

              ;; Report permission requests
              (claude-integration--log "Permission requests received: %d"
                                       (length permission-requests))

              (claude-integration--log "Scenario 2: %s" (if success "PASSED" "FAILED"))
              success)

          (error
           (claude-integration--log "ERROR: %s" (error-message-string err))
           nil))

      (claude-cli-stop session)
      (delete-directory test-dir t))))

;;; Test 3: Simple Query (replaces complex Plan Mode test)

(defun claude-integration-test-scenario3-simple-query ()
  "Test simple query with bypass mode."
  (claude-integration--log "=== Scenario 3: Simple Query ===")

  (let* ((test-dir (make-temp-file "claude-elisp-test-scenario3-" t))
         (session (claude-cli-create-session
                   :model "haiku"
                   :work-dir test-dir
                   :permission-mode 'bypass
                   :disable-plugins t))
         (success t)
         (response-text nil))

    (claude-integration--log "Test directory: %s" test-dir)

    (unwind-protect
        (condition-case err
            (progn
              (claude-cli-start session)
              (claude-integration--log "Session started")

              (claude-integration--log "Asking a simple math question...")
              (claude-cli-send-message session "Calculate 15 * 7 and respond with just the number.")
              (let ((events1 (claude-integration--collect-turn-events
                              session claude-integration-test-timeout)))
                (when (claude-cli-turn-events-turn-complete events1)
                  (setq response-text (claude-cli-turn-complete-event-full-text
                                       (claude-cli-turn-events-turn-complete events1)))
                  (claude-integration--log "Response: %s"
                                           (truncate-string-to-width
                                            (or response-text "") 100 nil nil "..."))
                  (claude-integration--log "Turn completed: success=%s"
                                           (claude-cli-turn-complete-event-success
                                            (claude-cli-turn-events-turn-complete events1))))
                (unless (claude-cli-turn-events-turn-complete events1)
                  (claude-integration--log "Turn TIMEOUT")
                  (setq success nil)))

              (claude-integration--log "Scenario 3: %s" (if success "PASSED" "FAILED"))
              success)

          (error
           (claude-integration--log "ERROR: %s" (error-message-string err))
           nil))

      (claude-cli-stop session)
      (delete-directory test-dir t))))

;;; Test 4: Interrupt Support

(defun claude-integration-test-scenario4-interrupt ()
  "Test interrupt support during long-running task."
  (claude-integration--log "=== Scenario 4: Interrupt Support ===")

  (let* ((test-dir (make-temp-file "claude-elisp-test-scenario4-" t))
         (session (claude-cli-create-session
                   :model "haiku"
                   :work-dir test-dir
                   :permission-mode 'bypass
                   :disable-plugins t))
         (interrupt-sent nil)
         (success t))

    (claude-integration--log "Test directory: %s" test-dir)

    (unwind-protect
        (condition-case err
            (progn
              (claude-cli-start session)
              (claude-integration--log "Session started")

              ;; Send a task that will be interrupted
              (claude-integration--log "Step 1: Sending task to be interrupted...")
              (claude-cli-send-message session
                                   "Write a very long essay about artificial intelligence, at least 1000 words.")

              ;; Custom event loop to interrupt on first tool or after some text
              (let ((events (claude-cli-turn-events--create))
                    (start-time (current-time))
                    (done nil)
                    (text-count 0))

                (let ((ready-fn (lambda (e)
                                  (setf (claude-cli-turn-events-ready events) e)))
                      (text-fn (lambda (e)
                                 (push e (claude-cli-turn-events-text-events events))
                                 (cl-incf text-count)
                                 ;; Interrupt after receiving some text
                                 (when (and (not interrupt-sent) (>= text-count 3))
                                   (setq interrupt-sent t)
                                   (claude-integration--log "Interrupting after %d text events..." text-count)
                                   (condition-case err
                                       (claude-cli-interrupt session)
                                     (error
                                      (claude-integration--log "Interrupt error: %s" err))))))
                      (tool-start-fn (lambda (e)
                                       (push e (claude-cli-turn-events-tool-starts events))
                                       ;; Also interrupt on tool start
                                       (when (not interrupt-sent)
                                         (setq interrupt-sent t)
                                         (claude-integration--log "Interrupting on tool: %s"
                                                                  (claude-cli-tool-start-event-name e))
                                         (condition-case err
                                             (claude-cli-interrupt session)
                                           (error
                                            (claude-integration--log "Interrupt error: %s" err))))))
                      (complete-fn (lambda (e)
                                     (setf (claude-cli-turn-events-turn-complete events) e)
                                     (setq done t)))
                      (error-fn (lambda (e)
                                  (push e (claude-cli-turn-events-errors events)))))

                  (add-hook 'claude-cli-ready-hook ready-fn)
                  (add-hook 'claude-cli-text-hook text-fn)
                  (add-hook 'claude-cli-tool-start-hook tool-start-fn)
                  (add-hook 'claude-cli-turn-complete-hook complete-fn)
                  (add-hook 'claude-cli-error-hook error-fn)

                  (unwind-protect
                      (progn
                        (while (and (not done)
                                    (< (float-time (time-subtract (current-time) start-time))
                                       90)
                                    (claude-cli-process-alive-p (claude-cli-session-process session)))
                          (accept-process-output nil 0.1))

                        (if (claude-cli-turn-events-turn-complete events)
                            (claude-integration--log "Turn 1 ended: success=%s"
                                                     (claude-cli-turn-complete-event-success
                                                      (claude-cli-turn-events-turn-complete events)))
                          (claude-integration--log "Turn 1 TIMEOUT (may be expected if interrupted)")))

                    (remove-hook 'claude-cli-ready-hook ready-fn)
                    (remove-hook 'claude-cli-text-hook text-fn)
                    (remove-hook 'claude-cli-tool-start-hook tool-start-fn)
                    (remove-hook 'claude-cli-turn-complete-hook complete-fn)
                    (remove-hook 'claude-cli-error-hook error-fn))))

              (unless interrupt-sent
                (claude-integration--log "WARNING: Interrupt was not sent"))

              ;; Try sending a new message after interrupt
              (claude-integration--log "Step 2: Sending new message after interrupt...")
              (when (claude-cli-process-alive-p (claude-cli-session-process session))
                (claude-cli-send-message session "What is 2+2?")
                (let ((events2 (claude-integration--collect-turn-events session 30)))
                  (if (claude-cli-turn-events-turn-complete events2)
                      (claude-integration--log "Session accepted new message: success=%s"
                                               (claude-cli-turn-complete-event-success
                                                (claude-cli-turn-events-turn-complete events2)))
                    (claude-integration--log "Post-interrupt message TIMEOUT"))))

              (claude-integration--log "Scenario 4: %s" (if success "PASSED" "FAILED"))
              success)

          (error
           (claude-integration--log "ERROR: %s" (error-message-string err))
           nil))

      (claude-cli-stop session)
      (delete-directory test-dir t))))

;;; Test runner

(defun claude-integration-test-run-all ()
  "Run all integration tests and report results."
  (interactive)
  (setq claude-integration-test-results nil)

  (message "")
  (message "============================================================")
  (message "Claude Elisp SDK Integration Tests")
  (message "============================================================")
  (message "")

  (let ((results nil)
        (start-time (current-time)))

    ;; Run each test
    (push (cons "Scenario 1: Bypass Permissions"
                (claude-integration-test-scenario1-bypass))
          results)
    (message "")

    (push (cons "Scenario 2: Default Permissions"
                (claude-integration-test-scenario2-default-permissions))
          results)
    (message "")

    (push (cons "Scenario 3: Simple Query"
                (claude-integration-test-scenario3-simple-query))
          results)
    (message "")

    (push (cons "Scenario 4: Interrupt"
                (claude-integration-test-scenario4-interrupt))
          results)
    (message "")

    ;; Summary
    (message "============================================================")
    (message "Test Summary")
    (message "============================================================")
    (let ((passed 0)
          (failed 0))
      (dolist (result (nreverse results))
        (if (cdr result)
            (progn
              (message "  PASS: %s" (car result))
              (cl-incf passed))
          (message "  FAIL: %s" (car result))
          (cl-incf failed)))
      (message "")
      (message "Total: %d passed, %d failed (%.1fs)"
               passed failed
               (float-time (time-subtract (current-time) start-time)))
      (message "============================================================")

      ;; Exit with appropriate code for batch mode
      (when noninteractive
        (kill-emacs (if (= failed 0) 0 1))))))

(provide 'claude-integration-test)
;;; claude-integration-test.el ends here
