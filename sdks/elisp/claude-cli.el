;;; claude-cli.el --- Emacs Lisp SDK for Claude CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude CLI Wrapper Contributors
;; Version: 0.1.0
;; Keywords: tools, processes, ai
;; URL: https://github.com/anthropics/claude-code
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This package provides an Emacs Lisp SDK for interacting with the
;; Claude CLI over the NDJSON streaming protocol.
;;
;; Quick start:
;;
;;   (require 'claude)
;;
;;   ;; Create and start a session
;;   (setq my-session (claude-create-session :model "haiku"
;;                                           :permission-mode 'bypass))
;;   (claude-start my-session)
;;
;;   ;; Send a message (non-blocking)
;;   (claude-send-message my-session "What is 2+2?")
;;
;;   ;; Or use blocking API
;;   (let ((result (claude-ask my-session "What is 2+2?")))
;;     (message "Answer: %s" (claude-cli-turn-result-result-text result)))
;;
;;   ;; Clean up
;;   (claude-stop my-session)
;;
;; Event-driven usage:
;;
;;   (add-hook 'claude-text-hook
;;             (lambda (event)
;;               (insert (claude-cli-text-event-text event))))
;;
;;   (add-hook 'claude-turn-complete-hook
;;             (lambda (event)
;;               (message "Done! Cost: $%.6f"
;;                        (claude-cli-turn-usage-cost-usd
;;                         (claude-cli-turn-complete-event-usage event)))))
;;
;; See README.md for full documentation.

;;; Code:

(require 'cl-lib)
(require 'claude-cli-protocol)
(require 'claude-cli-state)
(require 'claude-cli-turn)
(require 'claude-cli-events)
(require 'claude-cli-process)
(require 'claude-cli-permission)

;;; Customization

(defgroup claude-cli nil
  "Claude CLI SDK for Emacs."
  :group 'tools
  :prefix "claude-cli-")

(defcustom claude-cli-default-model "haiku"
  "Default model to use for new sessions."
  :type 'string
  :group 'claude-cli)

(defcustom claude-cli-default-permission-mode 'default
  "Default permission mode for new sessions.
Options: default, accept-edits, plan, bypass."
  :type '(choice (const default)
                 (const accept-edits)
                 (const plan)
                 (const bypass))
  :group 'claude-cli)

(defcustom claude-cli-default-work-dir nil
  "Default working directory for sessions.
When nil, uses `default-directory'."
  :type '(choice (const nil) directory)
  :group 'claude-cli)

(defcustom claude-cli-ask-timeout 60
  "Default timeout in seconds for `claude-cli-ask'."
  :type 'number
  :group 'claude-cli)

;;; Error types

(define-error 'claude-cli-error "Claude SDK error")
(define-error 'claude-cli-session-error "Claude session error" 'claude-cli-error)
(define-error 'claude-cli-timeout-error "Claude timeout error" 'claude-cli-error)

;;; Session info struct

(cl-defstruct (claude-cli-session-info (:constructor claude-cli-session-info--create)
                                   (:copier nil))
  "Session metadata received from CLI init message."
  session-id
  model
  work-dir
  tools
  permission-mode)

;;; Session struct

(cl-defstruct (claude-cli-session (:constructor claude-cli-session--create)
                              (:copier nil))
  "A Claude CLI session."
  ;; Configuration
  config
  ;; Runtime state
  process
  state
  turn-manager
  info
  ;; Internal
  (line-handler nil)
  (pending-completions nil :type list)
  (started nil :type boolean)
  (stopping nil :type boolean))

;;; Session creation

;;;###autoload
(defun claude-cli-create-session (&rest options)
  "Create a new Claude session with OPTIONS.

OPTIONS is a plist with keys:
  :model - Model name (default: `claude-cli-default-model')
  :work-dir - Working directory (default: `default-directory')
  :permission-mode - Permission mode symbol (default: `claude-cli-default-permission-mode')
  :cli-path - Custom CLI path (default: searches PATH)
  :disable-plugins - Disable plugins (boolean)
  :permission-handler - Function to handle permission requests

Returns a `claude-cli-session' struct.

Example:
  (claude-create-session :model \"sonnet\"
                         :permission-mode \\='bypass
                         :work-dir \"/path/to/project\")"
  (let ((config (list :model (or (plist-get options :model)
                                 claude-cli-default-model)
                      :work-dir (or (plist-get options :work-dir)
                                    claude-cli-default-work-dir)
                      :permission-mode (or (plist-get options :permission-mode)
                                           claude-cli-default-permission-mode)
                      :cli-path (plist-get options :cli-path)
                      :disable-plugins (plist-get options :disable-plugins)
                      :permission-handler (plist-get options :permission-handler))))
    (claude-cli-session--create
     :config config
     :state (claude-cli-state-create)
     :turn-manager (claude-cli-turn-manager-create))))

;;; Session lifecycle

;;;###autoload
(defun claude-cli-start (session)
  "Start SESSION, spawning the CLI process.
Returns SESSION on success, signals error on failure."
  (when (claude-cli-session-started session)
    (signal 'claude-cli-session-error '("Session already started")))

  ;; Transition to starting state
  (claude-cli-state-transition (claude-cli-session-state session) 'start)
  (claude-cli--dispatch-state-change session 'uninitialized 'starting)

  ;; Create line handler
  (setf (claude-cli-session-line-handler session)
        (claude-cli-process-make-line-handler #'claude-cli--handle-line))

  ;; Build session data for callbacks
  (let ((session-data (list :session session)))
    ;; Start process
    (setf (claude-cli-session-process session)
          (claude-cli-process-start
           session-data
           (claude-cli-session-line-handler session)
           #'claude-cli--handle-sentinel)))

  (setf (claude-cli-session-started session) t)
  session)

;;;###autoload
(defun claude-cli-stop (session)
  "Stop SESSION gracefully."
  (unless (claude-cli-session-stopping session)
    (setf (claude-cli-session-stopping session) t)
    (when-let ((proc (claude-cli-session-process session)))
      (claude-cli-process-stop proc)
      (claude-cli-process-cleanup proc))
    (let ((old-state (claude-cli-state-current (claude-cli-session-state session))))
      (when (claude-cli-state-can-transition-p (claude-cli-session-state session) 'close)
        (claude-cli-state-transition (claude-cli-session-state session) 'close)
        (claude-cli--dispatch-state-change session old-state 'closed)))))

;;; Messaging

;;;###autoload
(defun claude-cli-send-message (session content)
  "Send CONTENT message to SESSION (non-blocking).
Returns the turn number.

The response will arrive via events on `claude-cli-text-hook',
`claude-cli-tool-start-hook', etc., with `claude-cli-turn-complete-hook'
fired when the turn finishes."
  (claude-cli--check-can-send session)

  ;; Start new turn
  (let* ((turn-manager (claude-cli-session-turn-manager session))
         (turn (claude-cli-turn-manager-start-turn turn-manager content))
         (old-state (claude-cli-session-get-state session)))

    ;; Send message to CLI
    (claude-cli-process-send
     (claude-cli-session-process session)
     (claude-cli-protocol-user-message content))

    ;; Transition to processing
    (claude-cli-state-transition (claude-cli-session-state session) 'message-sent)
    (claude-cli--dispatch-state-change session old-state 'processing)

    (claude-cli-turn-state-number turn)))

;;;###autoload
(defun claude-cli-ask (session content &optional timeout)
  "Send CONTENT to SESSION and wait for response (blocking).
Optional TIMEOUT in seconds (default `claude-cli-ask-timeout').
Returns a `claude-cli-turn-result' struct, or nil on timeout.

Example:
  (let ((result (claude-ask session \"What is 2+2?\")))
    (when result
      (message \"Answer: %s\" (claude-cli-turn-result-result-text result))))"
  (let ((turn-number (claude-send-message session content))
        (timeout-secs (or timeout claude-cli-ask-timeout))
        (result nil)
        (timed-out nil)
        (start-time (current-time)))

    ;; Register completion callback
    (push (cons turn-number
                (lambda (r) (setq result r)))
          (claude-cli-session-pending-completions session))

    ;; Wait for completion, processing events
    (while (and (null result)
                (not timed-out)
                (claude-cli-process-alive-p (claude-cli-session-process session)))
      (accept-process-output nil 0.1)
      (when (> (float-time (time-subtract (current-time) start-time))
               timeout-secs)
        (setq timed-out t)))

    ;; Clean up pending if timed out
    (when timed-out
      (setf (claude-cli-session-pending-completions session)
            (cl-remove-if (lambda (entry) (= (car entry) turn-number))
                          (claude-cli-session-pending-completions session))))

    result))

;;; Control operations

;;;###autoload
(defun claude-cli-set-permission-mode (session mode)
  "Set permission MODE for SESSION.
MODE should be one of: default, accept-edits, plan, bypass.

If session is running, sends a control request.
Otherwise, updates the configuration for next start."
  (if (claude-cli-session-started session)
      ;; Send control request to running session
      (claude-cli-process-send
       (claude-cli-session-process session)
       (claude-cli-protocol-control-request
        (claude-cli-protocol--generate-request-id)
        (claude-cli-protocol-set-permission-mode-request mode)))
    ;; Just update config
    (plist-put (claude-cli-session-config session) :permission-mode mode)))

;;;###autoload
(defun claude-cli-interrupt (session)
  "Send interrupt signal to SESSION to stop current operation."
  (claude-cli--check-started session)
  (claude-cli-process-send
   (claude-cli-session-process session)
   (claude-cli-protocol-control-request
    (claude-cli-protocol--generate-request-id)
    (claude-cli-protocol-interrupt-request))))

;;; Query methods

;;;###autoload
(defun claude-cli-session-get-info (session)
  "Return session info for SESSION (available after ready event)."
  (claude-cli-session-info session))

;;;###autoload
(defun claude-cli-session-get-state (session)
  "Return current state symbol for SESSION."
  (claude-cli-state-current (claude-cli-session-state session)))

;;;###autoload
(defun claude-cli-session-ready-p (session)
  "Check if SESSION is ready to accept messages."
  (claude-cli-state-ready-p (claude-cli-session-state session)))

;;;###autoload
(defun claude-cli-session-processing-p (session)
  "Check if SESSION is currently processing a message."
  (claude-cli-state-processing-p (claude-cli-session-state session)))

;;;###autoload
(defun claude-cli-current-turn-number (session)
  "Return current turn number for SESSION."
  (claude-cli-turn-manager-current-number
   (claude-cli-session-turn-manager session)))

;;; Internal: Validation

(defun claude-cli--check-started (session)
  "Signal error if SESSION is not started."
  (unless (claude-cli-session-started session)
    (signal 'claude-cli-session-error '("Session not started"))))

(defun claude-cli--check-can-send (session)
  "Signal error if SESSION cannot accept messages."
  (claude-cli--check-started session)
  (when (claude-cli-session-stopping session)
    (signal 'claude-cli-session-error '("Session is stopping")))
  (unless (or (claude-cli-state-ready-p (claude-cli-session-state session))
              (claude-cli-state-is-p (claude-cli-session-state session) 'starting))
    (signal 'claude-cli-session-error
            (list (format "Cannot send in state: %s"
                          (claude-cli-session-get-state session))))))

;;; Internal: Message handling

(defun claude-cli--handle-line (session-data line)
  "Handle a single JSON LINE from the CLI.
SESSION-DATA is a plist with :session key."
  (let ((session (plist-get session-data :session)))
    (condition-case err
        (let ((msg (claude-cli-protocol-parse-message line)))
          (pcase (claude-cli-protocol-message-type msg)
            ("system" (claude-cli--handle-system session msg))
            ("stream_event" (claude-cli--handle-stream-event session msg))
            ("assistant" (claude-cli--handle-assistant session msg))
            ("user" (claude-cli--handle-user session msg))
            ("result" (claude-cli--handle-result session msg))
            ("control_request" (claude-cli--handle-control-request session msg))))
      (claude-cli-protocol-error
       (claude-cli-events-dispatch
        (claude-cli-error-event--create
         :turn-number (claude-cli-current-turn-number session)
         :error (format "Protocol error: %s" (cdr err))
         :context "parse_message"))))))

(defun claude-cli--handle-sentinel (session-data event)
  "Handle process sentinel EVENT.
SESSION-DATA is a plist with :session key."
  (let* ((session (plist-get session-data :session))
         (status (claude-cli-process-parse-sentinel-event event))
         (old-state (claude-cli-session-get-state session)))
    (when (claude-cli-state-can-transition-p (claude-cli-session-state session) 'close)
      (claude-cli-state-transition (claude-cli-session-state session) 'close)
      (claude-cli--dispatch-state-change session old-state 'closed))

    ;; Dispatch error for unexpected termination
    (unless (eq status 'finished)
      (claude-cli-events-dispatch
       (claude-cli-error-event--create
        :turn-number (claude-cli-current-turn-number session)
        :error (format "Process terminated: %s" event)
        :context "process_sentinel")))))

(defun claude-cli--handle-system (session msg)
  "Handle system MSG."
  (when (equal (plist-get msg :subtype) "init")
    ;; Extract init data - it may be nested under :data or at top level
    (let ((data (or (plist-get msg :data) msg)))
      (setf (claude-cli-session-info session)
            (claude-cli-session-info--create
             :session-id (plist-get data :session_id)
             :model (plist-get data :model)
             :work-dir (plist-get data :cwd)
             :tools (plist-get data :tools)
             :permission-mode (when-let ((pm (plist-get data :permissionMode)))
                                (claude-cli-protocol-permission-mode-to-symbol pm)))))

    ;; Transition to ready
    (let ((old-state (claude-cli-session-get-state session)))
      (claude-cli-state-transition (claude-cli-session-state session) 'init-received)
      (claude-cli--dispatch-state-change session old-state 'ready))

    ;; Dispatch ready event
    (claude-cli-events-dispatch
     (claude-cli-ready-event--create
      :session-id (claude-cli-session-info-session-id (claude-cli-session-info session))
      :model (claude-cli-session-info-model (claude-cli-session-info session))
      :work-dir (claude-cli-session-info-work-dir (claude-cli-session-info session))
      :tools (claude-cli-session-info-tools (claude-cli-session-info session))
      :permission-mode (claude-cli-session-info-permission-mode (claude-cli-session-info session))))))

(defun claude-cli--handle-stream-event (session msg)
  "Handle stream_event MSG."
  (let* ((event (claude-cli-protocol-get-event msg))
         (event-type (claude-cli-protocol-get-event-type event)))
    (pcase event-type
      ("content_block_start" (claude-cli--handle-block-start session event))
      ("content_block_delta" (claude-cli--handle-block-delta session event))
      ("content_block_stop" (claude-cli--handle-block-stop session event))
      ;; message_start/stop don't need special handling
      (_ nil))))

(defun claude-cli--handle-block-start (session event)
  "Handle content_block_start EVENT."
  (let* ((index (claude-cli-protocol-get-index event))
         (block (claude-cli-protocol-get-content-block event))
         (block-type (plist-get block :type))
         (turn-manager (claude-cli-session-turn-manager session)))

    ;; Store block state
    (claude-cli-turn-manager-set-block-state
     turn-manager index
     (list :type block-type
           :id (plist-get block :id)
           :name (plist-get block :name)
           :partial-input ""))

    ;; Register tool if tool_use block
    (when (equal block-type "tool_use")
      (claude-cli-turn-manager-start-tool
       turn-manager
       (plist-get block :id)
       (plist-get block :name))

      ;; Dispatch tool start event
      (claude-cli-events-dispatch
       (claude-cli-tool-start-event--create
        :turn-number (claude-cli-current-turn-number session)
        :id (plist-get block :id)
        :name (plist-get block :name)
        :timestamp (current-time))))))

(defun claude-cli--handle-block-delta (session event)
  "Handle content_block_delta EVENT."
  (let* ((index (claude-cli-protocol-get-index event))
         (delta (claude-cli-protocol-get-delta event))
         (delta-type (plist-get delta :type))
         (turn-manager (claude-cli-session-turn-manager session)))

    (pcase delta-type
      ("text_delta"
       (let* ((text (plist-get delta :text))
              (full-text (claude-cli-turn-manager-append-text turn-manager text)))
         (claude-cli-events-dispatch
          (claude-cli-text-event--create
           :turn-number (claude-cli-current-turn-number session)
           :text text
           :full-text full-text))))

      ("thinking_delta"
       (let* ((thinking (plist-get delta :thinking))
              (full-thinking (claude-cli-turn-manager-append-thinking turn-manager thinking)))
         (claude-cli-events-dispatch
          (claude-cli-thinking-event--create
           :turn-number (claude-cli-current-turn-number session)
           :thinking thinking
           :full-thinking full-thinking))))

      ("input_json_delta"
       (let* ((partial (plist-get delta :partial_json))
              (block-state (claude-cli-turn-manager-get-block-state turn-manager index))
              (tool-id (plist-get block-state :id))
              (accumulated (claude-cli-turn-manager-append-block-input turn-manager index partial)))

         ;; Also update tool state
         (when tool-id
           (claude-cli-turn-manager-append-tool-input turn-manager tool-id partial))

         (claude-cli-events-dispatch
          (claude-cli-tool-progress-event--create
           :turn-number (claude-cli-current-turn-number session)
           :id tool-id
           :name (plist-get block-state :name)
           :partial-input accumulated
           :input-chunk partial)))))))

(defun claude-cli--handle-block-stop (session event)
  "Handle content_block_stop EVENT."
  (let* ((index (claude-cli-protocol-get-index event))
         (turn-manager (claude-cli-session-turn-manager session))
         (block-state (claude-cli-turn-manager-get-block-state turn-manager index)))

    ;; For tool_use blocks, emit tool complete
    (when (equal (plist-get block-state :type) "tool_use")
      (let* ((tool-id (plist-get block-state :id))
             (tool (claude-cli-turn-manager-complete-tool turn-manager tool-id)))
        (claude-cli-events-dispatch
         (claude-cli-tool-complete-event--create
          :turn-number (claude-cli-current-turn-number session)
          :id tool-id
          :name (plist-get block-state :name)
          :input (when tool (claude-cli-tool-state-input tool))
          :timestamp (current-time)))))

    ;; Clear block state
    (claude-cli-turn-manager-clear-block-state turn-manager index)))

(defun claude-cli--handle-assistant (_session _msg)
  "Handle assistant MSG (complete message, not streamed)."
  ;; Most content comes via streaming, this is for complete messages
  nil)

(defun claude-cli--handle-user (session msg)
  "Handle user MSG (tool results echoed back)."
  (let ((message (plist-get msg :message)))
    (when message
      (let ((content (plist-get message :content)))
        (when (listp content)
          (dolist (block content)
            (when (equal (plist-get block :type) "tool_result")
              (claude-cli-events-dispatch
               (claude-cli-tool-result-event--create
                :turn-number (claude-cli-current-turn-number session)
                :tool-use-id (plist-get block :tool_use_id)
                :tool-name nil  ; Not always available in result
                :content (plist-get block :content)
                :is-error (plist-get block :is_error))))))))))

(defun claude-cli--handle-result (session msg)
  "Handle result MSG (turn completion)."
  (let* ((turn-manager (claude-cli-session-turn-manager session))
         (turn-number (claude-cli-turn-manager-current-number turn-manager))
         (result (claude-cli-turn-manager-complete-turn turn-manager msg))
         (full-text (claude-cli-turn-manager-get-full-text turn-manager))
         (full-thinking (claude-cli-turn-manager-get-full-thinking turn-manager)))

    ;; Transition back to ready
    (let ((old-state (claude-cli-session-get-state session)))
      (when (claude-cli-state-can-transition-p (claude-cli-session-state session) 'result-received)
        (claude-cli-state-transition (claude-cli-session-state session) 'result-received)
        (claude-cli--dispatch-state-change session old-state 'ready)))

    ;; Dispatch turn complete event
    (claude-cli-events-dispatch
     (claude-cli-turn-complete-event--create
      :turn-number turn-number
      :success (claude-cli-turn-result-success result)
      :duration-ms (claude-cli-turn-result-duration-ms result)
      :usage (claude-cli-turn-result-usage result)
      :error (claude-cli-turn-result-error result)
      :full-text full-text
      :full-thinking full-thinking))

    ;; Notify pending completions (for blocking API)
    (claude-cli--notify-completion session turn-number result)))

(defun claude-cli--handle-control-request (session msg)
  "Handle control_request MSG (permission prompts)."
  (let* ((request-id (plist-get msg :request_id))
         (request (plist-get msg :request))
         (subtype (plist-get request :subtype)))
    (when (equal subtype "can_use_tool")
      (let* ((handler (plist-get (claude-cli-session-config session) :permission-handler))
             (response (claude-cli-permission-handle-request request-id request handler)))
        (claude-cli-process-send (claude-cli-session-process session) response)))))

;;; Internal: Completion notification

(defun claude-cli--notify-completion (session turn-number result)
  "Notify completion waiters for TURN-NUMBER with RESULT."
  (let ((pending (claude-cli-session-pending-completions session)))
    (when-let ((entry (cl-assoc turn-number pending)))
      (setf (claude-cli-session-pending-completions session)
            (cl-remove-if (lambda (e) (= (car e) turn-number)) pending))
      (funcall (cdr entry) result))))

;;; Internal: State change dispatch

(defun claude-cli--dispatch-state-change (session from to)
  "Dispatch state change event for SESSION from FROM to TO."
  (ignore session)  ; Session reference available if needed
  (claude-cli-events-dispatch
   (claude-cli-state-change-event--create :from from :to to)))

(provide 'claude-cli)
;;; claude-cli.el ends here
