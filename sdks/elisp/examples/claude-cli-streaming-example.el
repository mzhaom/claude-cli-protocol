;;; claude-cli-streaming-example.el --- Streaming usage of Claude SDK -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This example demonstrates event-driven streaming usage of the Claude SDK.
;; It shows how to:
;; - Use hooks for real-time streaming output
;; - Handle different event types
;; - Display text as it arrives
;; - Track tool executions

;;; Code:

(require 'claude-cli)

(defvar claude-cli-streaming-example-buffer "*Claude Streaming*"
  "Buffer name for streaming output.")

(defun claude-cli-example-streaming ()
  "Demonstrate streaming with real-time text output."
  (interactive)
  (let ((session (claude-cli-create-session
                  :model "haiku"
                  :permission-mode 'bypass
                  :disable-plugins t))
        (output-buffer (get-buffer-create claude-cli-streaming-example-buffer))
        (done nil))

    ;; Prepare output buffer
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert "=== Claude Streaming Example ===\n\n"))
    (display-buffer output-buffer)

    ;; Define event handlers
    (let ((ready-fn
           (lambda (event)
             (with-current-buffer output-buffer
               (insert (format "[Ready] Session: %s, Model: %s\n\n"
                               (claude-cli-ready-event-session-id event)
                               (claude-cli-ready-event-model event))))))

          (text-fn
           (lambda (event)
             (with-current-buffer output-buffer
               (goto-char (point-max))
               (insert (claude-cli-text-event-text event)))))

          (thinking-fn
           (lambda (event)
             (with-current-buffer output-buffer
               (goto-char (point-max))
               (insert (format "\n[Thinking] %s\n"
                               (claude-cli-thinking-event-thinking event))))))

          (tool-start-fn
           (lambda (event)
             (with-current-buffer output-buffer
               (goto-char (point-max))
               (insert (format "\n[Tool Started] %s (id: %s)\n"
                               (claude-cli-tool-start-event-name event)
                               (claude-cli-tool-start-event-id event))))))

          (tool-complete-fn
           (lambda (event)
             (with-current-buffer output-buffer
               (goto-char (point-max))
               (insert (format "[Tool Input Complete] %s: %S\n"
                               (claude-cli-tool-complete-event-name event)
                               (claude-cli-tool-complete-event-input event))))))

          (cli-result-fn
           (lambda (event)
             (with-current-buffer output-buffer
               (goto-char (point-max))
               (let ((status (if (claude-cli-tool-result-event-is-error event)
                                 "error" "success")))
                 (insert (format "[Tool Result] %s: %s\n"
                                 status
                                 (truncate-string-to-width
                                  (format "%S" (claude-cli-tool-result-event-content event))
                                  80 nil nil "...")))))))

          (complete-fn
           (lambda (event)
             (with-current-buffer output-buffer
               (goto-char (point-max))
               (let ((usage (claude-cli-turn-complete-event-usage event)))
                 (insert (format "\n\n[Turn Complete] Turn %d\n"
                                 (claude-cli-turn-complete-event-turn-number event)))
                 (insert (format "  Success: %s\n"
                                 (claude-cli-turn-complete-event-success event)))
                 (insert (format "  Duration: %dms\n"
                                 (claude-cli-turn-complete-event-duration-ms event)))
                 (insert (format "  Tokens: %d in / %d out\n"
                                 (claude-cli-turn-usage-input-tokens usage)
                                 (claude-cli-turn-usage-output-tokens usage)))
                 (insert (format "  Cost: $%.6f\n"
                                 (claude-cli-turn-usage-cost-usd usage)))
                 (when (claude-cli-turn-complete-event-error event)
                   (insert (format "  Error: %s\n"
                                   (claude-cli-turn-complete-event-error event))))))
             (setq done t)))

          (error-fn
           (lambda (event)
             (with-current-buffer output-buffer
               (goto-char (point-max))
               (insert (format "\n[Error] %s: %s\n"
                               (claude-cli-error-event-context event)
                               (claude-cli-error-event-error event)))))))

      ;; Add all hooks
      (add-hook 'claude-cli-ready-hook ready-fn)
      (add-hook 'claude-cli-text-hook text-fn)
      (add-hook 'claude-cli-thinking-hook thinking-fn)
      (add-hook 'claude-cli-tool-start-hook tool-start-fn)
      (add-hook 'claude-cli-tool-complete-hook tool-complete-fn)
      (add-hook 'claude-cli-cli-tool-result-hook cli-result-fn)
      (add-hook 'claude-cli-turn-complete-hook complete-fn)
      (add-hook 'claude-cli-error-hook error-fn)

      (unwind-protect
          (progn
            ;; Start session
            (with-current-buffer output-buffer
              (insert "Starting session...\n"))
            (claude-cli-start session)

            ;; Send message
            (with-current-buffer output-buffer
              (insert "Sending message...\n\n--- Response ---\n"))
            (claude-cli-send-message session
                                 "Write a haiku about Emacs. Be creative!")

            ;; Process events until done
            (while (and (not done)
                        (claude-cli-process-alive-p (claude-cli-session-process session)))
              (accept-process-output nil 0.05))

            (with-current-buffer output-buffer
              (insert "\n--- End Response ---\n")))

        ;; Cleanup
        (remove-hook 'claude-cli-ready-hook ready-fn)
        (remove-hook 'claude-cli-text-hook text-fn)
        (remove-hook 'claude-cli-thinking-hook thinking-fn)
        (remove-hook 'claude-cli-tool-start-hook tool-start-fn)
        (remove-hook 'claude-cli-tool-complete-hook tool-complete-fn)
        (remove-hook 'claude-cli-cli-tool-result-hook cli-result-fn)
        (remove-hook 'claude-cli-turn-complete-hook complete-fn)
        (remove-hook 'claude-cli-error-hook error-fn)
        (claude-cli-stop session)))))

(defun claude-cli-example-streaming-to-buffer ()
  "Stream Claude's response directly into a buffer.
This is a simpler example focused on real-time text insertion."
  (interactive)
  (let* ((session (claude-cli-create-session
                   :model "haiku"
                   :permission-mode 'bypass))
         (output-buffer (get-buffer-create "*Claude Output*"))
         (done nil)
         (text-fn (lambda (event)
                    (with-current-buffer output-buffer
                      (goto-char (point-max))
                      (insert (claude-cli-text-event-text event)))))
         (complete-fn (lambda (_event)
                        (setq done t))))

    ;; Prepare buffer
    (with-current-buffer output-buffer
      (erase-buffer))
    (display-buffer output-buffer)

    ;; Add hooks
    (add-hook 'claude-cli-text-hook text-fn)
    (add-hook 'claude-cli-turn-complete-hook complete-fn)

    (unwind-protect
        (progn
          (claude-cli-start session)
          ;; Wait for ready
          (while (not (claude-cli-session-ready-p session))
            (accept-process-output nil 0.1))

          (claude-cli-send-message session
                               "Explain recursion in 3 sentences.")

          ;; Wait for completion
          (while (and (not done)
                      (claude-cli-process-alive-p (claude-cli-session-process session)))
            (accept-process-output nil 0.05)))

      ;; Cleanup
      (remove-hook 'claude-cli-text-hook text-fn)
      (remove-hook 'claude-cli-turn-complete-hook complete-fn)
      (claude-cli-stop session))))

(provide 'claude-cli-streaming-example)
;;; claude-cli-streaming-example.el ends here
