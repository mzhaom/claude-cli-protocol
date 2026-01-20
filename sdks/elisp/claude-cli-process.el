;;; claude-cli-process.el --- CLI process management for Claude sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude CLI Wrapper Contributors
;; Keywords: tools, processes
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module manages the Claude CLI subprocess lifecycle:
;; - Starting the CLI with appropriate arguments
;; - NDJSON line buffering in the process filter
;; - Process sentinel for termination handling
;; - Graceful shutdown sequence
;;
;; The CLI is started with:
;;   claude --print --input-format stream-json --output-format stream-json \
;;          --verbose --include-partial-messages --model MODEL --permission-mode MODE

;;; Code:

(require 'cl-lib)
(require 'claude-cli-protocol)

;;; Customization

(defcustom claude-cli-path nil
  "Path to the Claude CLI binary.
When nil, searches PATH for \"claude\"."
  :type '(choice (const nil) string)
  :group 'claude)

(defcustom claude-cli-process-shutdown-timeout 0.5
  "Seconds to wait for graceful shutdown before killing process."
  :type 'number
  :group 'claude)

;;; Process creation

(defun claude-cli-process--build-args (config)
  "Build CLI arguments from CONFIG plist.
CONFIG should have keys :model, :permission-mode, :disable-plugins."
  (let ((args (list "--print"
                    "--input-format" "stream-json"
                    "--output-format" "stream-json"
                    "--verbose"
                    "--include-partial-messages")))
    ;; Add model
    (when-let ((model (plist-get config :model)))
      (setq args (append args (list "--model" model))))
    ;; Add permission mode
    (when-let ((mode (plist-get config :permission-mode)))
      (setq args (append args
                         (list "--permission-mode"
                               (claude-cli-protocol-permission-mode-to-string mode)))))
    ;; Disable plugins
    (when (plist-get config :disable-plugins)
      (setq args (append args (list "--plugin-dir" "/dev/null"))))
    args))

(defun claude-cli-process-start (session-data filter-fn sentinel-fn)
  "Start the Claude CLI process for SESSION-DATA.
FILTER-FN is called with (session-data output) for each output chunk.
SENTINEL-FN is called with (session-data event) on process state changes.
Returns the process object."
  (let* ((config (plist-get session-data :config))
         (cli-path (or (plist-get config :cli-path)
                       claude-cli-path
                       "claude"))
         (args (claude-cli-process--build-args config))
         (default-directory (or (plist-get config :work-dir)
                                default-directory))
         (process-connection-type nil)  ; Use pipes, not pty
         (stderr-buffer (generate-new-buffer " *claude-stderr*"))
         proc)

    ;; Create the process
    (setq proc (make-process
                :name "claude-cli"
                :buffer nil
                :command (cons cli-path args)
                :connection-type 'pipe
                :noquery t  ; Don't ask before killing
                :filter (lambda (_proc output)
                          (funcall filter-fn session-data output))
                :sentinel (lambda (_proc event)
                            (funcall sentinel-fn session-data event))
                :stderr stderr-buffer))

    ;; Store stderr buffer reference
    (process-put proc 'stderr-buffer stderr-buffer)

    proc))

;;; NDJSON line buffering

(defun claude-cli-process-make-line-handler (line-callback)
  "Create a filter function that buffers and extracts complete lines.
LINE-CALLBACK is called with (session-data line) for each complete line.
Returns a function suitable for use as a process filter."
  (let ((line-buffer ""))
    (lambda (session-data output)
      ;; Append output to buffer
      (setq line-buffer (concat line-buffer output))

      ;; Process complete lines
      (while (string-match "\n" line-buffer)
        (let ((line (substring line-buffer 0 (match-beginning 0)))
              (rest (substring line-buffer (match-end 0))))
          (setq line-buffer rest)
          ;; Process non-empty lines
          (unless (string-empty-p (string-trim line))
            (funcall line-callback session-data line)))))))

;;; Process communication

(defun claude-cli-process-send (process msg)
  "Send MSG plist to PROCESS as NDJSON."
  (when (process-live-p process)
    (process-send-string process (claude-cli-protocol-encode-message msg))))

(defun claude-cli-process-send-eof (process)
  "Send EOF to PROCESS stdin to signal graceful shutdown."
  (when (process-live-p process)
    (process-send-eof process)))

;;; Process lifecycle

(defun claude-cli-process-stop (process &optional force)
  "Stop PROCESS gracefully, or FORCE kill immediately.
Returns t if process was running."
  (when (process-live-p process)
    (if force
        (progn
          (delete-process process)
          t)
      ;; Graceful shutdown: close stdin, wait, then kill
      (claude-cli-process-send-eof process)
      (run-with-timer
       claude-cli-process-shutdown-timeout nil
       (lambda ()
         (when (process-live-p process)
           ;; Still alive, send SIGTERM equivalent
           (delete-process process))))
      t)))

(defun claude-cli-process-alive-p (process)
  "Check if PROCESS is still running."
  (and process (process-live-p process)))

(defun claude-cli-process-get-stderr (process)
  "Get stderr output from PROCESS."
  (when-let ((buffer (process-get process 'stderr-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-string)))))

(defun claude-cli-process-cleanup (process)
  "Clean up resources associated with PROCESS."
  (when-let ((buffer (process-get process 'stderr-buffer)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

;;; Sentinel event parsing

(defun claude-cli-process-parse-sentinel-event (event)
  "Parse sentinel EVENT string into a status symbol.
Returns one of: finished, killed, exited, failed, or unknown."
  (cond
   ((string-match-p "finished" event) 'finished)
   ((string-match-p "killed" event) 'killed)
   ((string-match-p "exited" event) 'exited)
   ((string-match-p "failed" event) 'failed)
   ((string-match-p "connection broken" event) 'killed)
   (t 'unknown)))

(provide 'claude-cli-process)
;;; claude-cli-process.el ends here
