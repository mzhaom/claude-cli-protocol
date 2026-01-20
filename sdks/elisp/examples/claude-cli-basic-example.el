;;; claude-basic-example.el --- Basic usage of Claude SDK -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This example demonstrates basic synchronous usage of the Claude SDK.
;; It shows how to:
;; - Create and configure a session
;; - Send messages and receive responses
;; - Handle multi-turn conversations
;; - Access response metadata (tokens, cost)

;;; Code:

(require 'claude-cli)

(defun claude-cli-example-basic ()
  "Demonstrate basic blocking usage of the Claude SDK."
  (interactive)
  (let ((session (claude-cli-create-session
                  :model "haiku"
                  :permission-mode 'bypass
                  :disable-plugins t)))  ; Faster startup
    (unwind-protect
        (progn
          ;; Start the session
          (message "Starting Claude session...")
          (claude-cli-start session)

          ;; Wait for ready state
          (message "Waiting for session to be ready...")
          (while (not (claude-cli-session-ready-p session))
            (accept-process-output nil 0.1))

          (let ((info (claude-cli-session-get-info session)))
            (message "Session ready: %s (model: %s)"
                     (claude-cli-session-info-session-id info)
                     (claude-cli-session-info-model info)))

          ;; First question
          (message "\nAsking: What is 2+2?")
          (let ((result (claude-cli-ask session "What is 2+2?" 30)))
            (if result
                (progn
                  (message "\nResult:")
                  (message "  Text: %s" (claude-cli-turn-result-result-text result))
                  (message "  Success: %s" (claude-cli-turn-result-success result))
                  (message "  Duration: %dms" (claude-cli-turn-result-duration-ms result))
                  (let ((usage (claude-cli-turn-result-usage result)))
                    (message "  Input tokens: %d" (claude-cli-turn-usage-input-tokens usage))
                    (message "  Output tokens: %d" (claude-cli-turn-usage-output-tokens usage))
                    (message "  Cost: $%.6f" (claude-cli-turn-usage-cost-usd usage))))
              (message "Timeout waiting for response")))

          ;; Follow-up question (multi-turn)
          (message "\nAsking follow-up: What about 3+3?")
          (let ((result (claude-cli-ask session "What about 3+3?" 30)))
            (if result
                (progn
                  (message "\nFollow-up result:")
                  (message "  Text: %s" (claude-cli-turn-result-result-text result))
                  (let ((usage (claude-cli-turn-result-usage result)))
                    (message "  Cost: $%.6f" (claude-cli-turn-usage-cost-usd usage))))
              (message "Timeout waiting for response")))

          (message "\nSession complete!"))

      ;; Always clean up
      (message "Stopping session...")
      (claude-cli-stop session))))

(defun claude-cli-example-with-hooks ()
  "Demonstrate basic usage with event hooks for logging."
  (interactive)
  (let ((session (claude-cli-create-session
                  :model "haiku"
                  :permission-mode 'bypass))
        ;; Track hook functions for cleanup
        (ready-fn (lambda (event)
                    (message "[Ready] Session: %s"
                             (claude-cli-ready-event-session-id event))))
        (text-fn (lambda (event)
                   (message "[Text chunk] %s"
                            (claude-cli-text-event-text event))))
        (complete-fn (lambda (event)
                       (message "[Turn complete] Turn %d, success: %s"
                                (claude-cli-turn-complete-event-turn-number event)
                                (claude-cli-turn-complete-event-success event)))))

    ;; Add hooks
    (add-hook 'claude-cli-ready-hook ready-fn)
    (add-hook 'claude-cli-text-hook text-fn)
    (add-hook 'claude-cli-turn-complete-hook complete-fn)

    (unwind-protect
        (progn
          (claude-cli-start session)
          (let ((result (claude-cli-ask session "Say hello in exactly 5 words." 30)))
            (when result
              (message "\nFinal text: %s"
                       (claude-cli-turn-result-result-text result)))))

      ;; Cleanup
      (remove-hook 'claude-cli-ready-hook ready-fn)
      (remove-hook 'claude-cli-text-hook text-fn)
      (remove-hook 'claude-cli-turn-complete-hook complete-fn)
      (claude-cli-stop session))))

(provide 'claude-basic-example)
;;; claude-basic-example.el ends here
