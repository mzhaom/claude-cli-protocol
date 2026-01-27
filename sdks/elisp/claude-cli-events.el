;;; claude-cli-events.el --- Event types and hook system for Claude sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude CLI Wrapper Contributors
;; Keywords: tools, processes
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module defines event types and the hook-based dispatch system
;; for Claude CLI sessions.
;;
;; Events are dispatched via Emacs hook variables, following the
;; idiomatic pattern for event notification in Emacs packages.
;;
;; Event types:
;; - Ready: Session initialized
;; - Text: Streaming text chunk
;; - Thinking: Extended thinking chunk
;; - ToolStart: Tool execution began
;; - ToolProgress: Partial tool input
;; - ToolComplete: Tool input parsed
;; - CLIToolResult: CLI-executed tool result
;; - TurnComplete: Turn finished
;; - Error: Session error
;; - StateChange: State transition

;;; Code:

(require 'cl-lib)
(require 'claude-cli-turn)

;;; Hook variables

(defvar claude-cli-event-hook nil
  "Hook run for all Claude events.
Functions receive one argument: an event struct.
Use `claude-event-type' to discriminate event types.")

(defvar claude-cli-ready-hook nil
  "Hook run when a Claude session becomes ready.
Functions receive one argument: a `claude-ready-event' struct.")

(defvar claude-cli-text-hook nil
  "Hook run for each streaming text chunk.
Functions receive one argument: a `claude-text-event' struct.")

(defvar claude-cli-thinking-hook nil
  "Hook run for each extended thinking chunk.
Functions receive one argument: a `claude-thinking-event' struct.")

(defvar claude-cli-tool-start-hook nil
  "Hook run when a tool begins execution.
Functions receive one argument: a `claude-tool-start-event' struct.")

(defvar claude-cli-tool-progress-hook nil
  "Hook run for partial tool input.
Functions receive one argument: a `claude-tool-progress-event' struct.")

(defvar claude-cli-tool-complete-hook nil
  "Hook run when tool input is fully parsed.
Functions receive one argument: a `claude-tool-complete-event' struct.")

(defvar claude-cli-tool-result-hook nil
  "Hook run when CLI sends back tool results.
Functions receive one argument: a `claude-cli-tool-result-event' struct.")

(defvar claude-cli-turn-complete-hook nil
  "Hook run when a turn finishes.
Functions receive one argument: a `claude-cli-turn-complete-event' struct.")

(defvar claude-cli-error-hook nil
  "Hook run on session errors.
Functions receive one argument: a `claude-error-event' struct.")

(defvar claude-cli-state-change-hook nil
  "Hook run on state transitions.
Functions receive one argument: a `claude-cli-state-change-event' struct.")

;;; Event struct definitions

(cl-defstruct (claude-cli-ready-event (:constructor claude-cli-ready-event--create)
                                  (:copier nil))
  "Event fired when session is initialized and ready."
  (type 'ready :read-only t)
  session-id
  model
  work-dir
  tools
  permission-mode)

(cl-defstruct (claude-cli-text-event (:constructor claude-cli-text-event--create)
                                 (:copier nil))
  "Event for streaming text chunks."
  (type 'text :read-only t)
  (turn-number 0 :type integer)
  text          ; New chunk
  full-text)    ; Accumulated

(cl-defstruct (claude-cli-thinking-event (:constructor claude-cli-thinking-event--create)
                                     (:copier nil))
  "Event for extended thinking chunks."
  (type 'thinking :read-only t)
  (turn-number 0 :type integer)
  thinking         ; New chunk
  full-thinking)   ; Accumulated

(cl-defstruct (claude-cli-tool-start-event (:constructor claude-cli-tool-start-event--create)
                                       (:copier nil))
  "Event when tool execution starts."
  (type 'tool-start :read-only t)
  (turn-number 0 :type integer)
  id
  name
  timestamp)

(cl-defstruct (claude-cli-tool-progress-event (:constructor claude-cli-tool-progress-event--create)
                                          (:copier nil))
  "Event for partial tool input during streaming."
  (type 'tool-progress :read-only t)
  (turn-number 0 :type integer)
  id
  name
  partial-input   ; Accumulated so far
  input-chunk)    ; New chunk

(cl-defstruct (claude-cli-tool-complete-event (:constructor claude-cli-tool-complete-event--create)
                                          (:copier nil))
  "Event when tool input is fully parsed."
  (type 'tool-complete :read-only t)
  (turn-number 0 :type integer)
  id
  name
  input       ; Parsed JSON input
  timestamp)

(cl-defstruct (claude-cli-tool-result-event (:constructor claude-cli-tool-result-event--create)
                                            (:copier nil))
  "Event when CLI auto-executes a tool and returns result."
  (type 'cli-tool-result :read-only t)
  (turn-number 0 :type integer)
  tool-use-id
  tool-name
  content
  (is-error nil :type boolean))

(cl-defstruct (claude-cli-turn-complete-event (:constructor claude-cli-turn-complete-event--create)
                                          (:copier nil))
  "Event when a turn finishes."
  (type 'turn-complete :read-only t)
  (turn-number 0 :type integer)
  (success t :type boolean)
  (duration-ms 0 :type integer)
  usage       ; claude-cli-turn-usage struct
  error
  full-text
  full-thinking)

(cl-defstruct (claude-cli-error-event (:constructor claude-cli-error-event--create)
                                  (:copier nil))
  "Event for session errors."
  (type 'error :read-only t)
  (turn-number 0 :type integer)
  error
  context)

(cl-defstruct (claude-cli-state-change-event (:constructor claude-cli-state-change-event--create)
                                         (:copier nil))
  "Event for state machine transitions."
  (type 'state-change :read-only t)
  from
  to)

;;; Event type accessor

(defun claude-cli-event-type (event)
  "Get the type symbol from EVENT."
  (cond
   ((claude-cli-ready-event-p event) 'ready)
   ((claude-cli-text-event-p event) 'text)
   ((claude-cli-thinking-event-p event) 'thinking)
   ((claude-cli-tool-start-event-p event) 'tool-start)
   ((claude-cli-tool-progress-event-p event) 'tool-progress)
   ((claude-cli-tool-complete-event-p event) 'tool-complete)
   ((claude-cli-tool-result-event-p event) 'cli-tool-result)
   ((claude-cli-turn-complete-event-p event) 'turn-complete)
   ((claude-cli-error-event-p event) 'error)
   ((claude-cli-state-change-event-p event) 'state-change)
   (t nil)))

;;; Event dispatch

(defun claude-cli-events-dispatch (event)
  "Dispatch EVENT to appropriate hooks."
  ;; Run unified hook first
  (run-hook-with-args 'claude-cli-event-hook event)
  ;; Then type-specific hook
  (pcase (claude-cli-event-type event)
    ('ready (run-hook-with-args 'claude-cli-ready-hook event))
    ('text (run-hook-with-args 'claude-cli-text-hook event))
    ('thinking (run-hook-with-args 'claude-cli-thinking-hook event))
    ('tool-start (run-hook-with-args 'claude-cli-tool-start-hook event))
    ('tool-progress (run-hook-with-args 'claude-cli-tool-progress-hook event))
    ('tool-complete (run-hook-with-args 'claude-cli-tool-complete-hook event))
    ('cli-tool-result (run-hook-with-args 'claude-cli-tool-result-hook event))
    ('turn-complete (run-hook-with-args 'claude-cli-turn-complete-hook event))
    ('error (run-hook-with-args 'claude-cli-error-hook event))
    ('state-change (run-hook-with-args 'claude-cli-state-change-hook event))))

;;; Convenience constructors from protocol messages

(defun claude-cli-events-make-ready (init-msg)
  "Create a ready event from system INIT-MSG."
  (claude-cli-ready-event--create
   :session-id (plist-get init-msg :session_id)
   :model (plist-get init-msg :model)
   :work-dir (plist-get init-msg :cwd)
   :tools (plist-get init-msg :tools)
   :permission-mode (plist-get init-msg :permissionMode)))

(defun claude-cli-events-make-turn-complete (turn-number result usage full-text full-thinking)
  "Create a turn complete event.
TURN-NUMBER is the turn that completed.
RESULT is the claude-cli-turn-result struct.
USAGE is the claude-cli-turn-usage struct.
FULL-TEXT and FULL-THINKING are accumulated strings."
  (claude-cli-turn-complete-event--create
   :turn-number turn-number
   :success (claude-cli-turn-result-success result)
   :duration-ms (claude-cli-turn-result-duration-ms result)
   :usage usage
   :error (claude-cli-turn-result-error result)
   :full-text full-text
   :full-thinking full-thinking))

(provide 'claude-cli-events)
;;; claude-cli-events.el ends here
