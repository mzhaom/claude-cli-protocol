;;; claude-cli-turn.el --- Turn management for Claude sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude CLI Wrapper Contributors
;; Keywords: tools, processes
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module manages conversation turns in Claude sessions.
;;
;; A turn represents a single user message and Claude's response.
;; The turn manager tracks:
;; - Turn numbers (incrementing)
;; - Accumulated text and thinking content
;; - Tool states within a turn
;; - Content block states during streaming
;;
;; This mirrors the accumulator pattern from the Go SDK.

;;; Code:

(require 'cl-lib)

;;; Data structures

(cl-defstruct (claude-cli-turn-usage (:constructor claude-cli-turn-usage--create)
                                 (:copier nil))
  "Token usage statistics for a turn."
  (input-tokens 0 :type integer)
  (output-tokens 0 :type integer)
  (cache-read-tokens 0 :type integer)
  (cache-creation-tokens 0 :type integer)
  (cost-usd 0.0 :type float))

(cl-defstruct (claude-cli-turn-result (:constructor claude-cli-turn-result--create)
                                  (:copier nil))
  "Result of a completed turn."
  (turn-number 0 :type integer)
  (success t :type boolean)
  (duration-ms 0 :type integer)
  (duration-api-ms 0 :type integer)
  usage
  error
  result-text)

(cl-defstruct (claude-cli-tool-state (:constructor claude-cli-tool-state--create)
                                 (:copier nil))
  "State of a tool invocation within a turn."
  (id "" :type string)
  (name "" :type string)
  (partial-input "" :type string)
  input
  (start-time nil))

(cl-defstruct (claude-cli-turn-state (:constructor claude-cli-turn-state--create)
                                 (:copier nil))
  "State of a single conversation turn."
  (number 0 :type integer)
  (user-message "" :type string)
  (start-time nil)
  (full-text "" :type string)
  (full-thinking "" :type string)
  (tools (make-hash-table :test 'equal) :type hash-table)
  ;; Block states for streaming accumulation (keyed by index)
  (block-states (make-hash-table :test 'eql) :type hash-table))

(cl-defstruct (claude-cli-turn-manager (:constructor claude-cli-turn-manager--create)
                                   (:copier nil))
  "Manages turns for a session."
  (current-number 0 :type integer)
  current-turn
  (turns (make-hash-table :test 'eql) :type hash-table))

;;; Turn manager functions

(defun claude-cli-turn-manager-create ()
  "Create a new turn manager."
  (claude-cli-turn-manager--create))

(defun claude-cli-turn-manager-start-turn (manager user-message)
  "Start a new turn in MANAGER with USER-MESSAGE.
Returns the new turn state."
  (let* ((turn-number (cl-incf (claude-cli-turn-manager-current-number manager)))
         (turn (claude-cli-turn-state--create
                :number turn-number
                :user-message user-message
                :start-time (current-time))))
    (setf (claude-cli-turn-manager-current-turn manager) turn)
    (puthash turn-number turn (claude-cli-turn-manager-turns manager))
    turn))

(defun claude-cli-turn-manager-get-turn (manager turn-number)
  "Get turn by TURN-NUMBER from MANAGER."
  (gethash turn-number (claude-cli-turn-manager-turns manager)))

(defun claude-cli-turn-manager-current (manager)
  "Get the current turn from MANAGER."
  (claude-cli-turn-manager-current-turn manager))

;;; Text accumulation

(defun claude-cli-turn-manager-append-text (manager text)
  "Append TEXT to current turn in MANAGER.
Returns the full accumulated text."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (setf (claude-cli-turn-state-full-text turn)
          (concat (claude-cli-turn-state-full-text turn) text))
    (claude-cli-turn-state-full-text turn)))

(defun claude-cli-turn-manager-get-full-text (manager)
  "Get the full accumulated text from current turn in MANAGER."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (claude-cli-turn-state-full-text turn)))

;;; Thinking accumulation

(defun claude-cli-turn-manager-append-thinking (manager thinking)
  "Append THINKING to current turn in MANAGER.
Returns the full accumulated thinking."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (setf (claude-cli-turn-state-full-thinking turn)
          (concat (claude-cli-turn-state-full-thinking turn) thinking))
    (claude-cli-turn-state-full-thinking turn)))

(defun claude-cli-turn-manager-get-full-thinking (manager)
  "Get the full accumulated thinking from current turn in MANAGER."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (claude-cli-turn-state-full-thinking turn)))

;;; Tool state management

(defun claude-cli-turn-manager-start-tool (manager tool-id tool-name)
  "Register tool start in MANAGER with TOOL-ID and TOOL-NAME."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (let ((tool-state (claude-cli-tool-state--create
                       :id tool-id
                       :name tool-name
                       :start-time (current-time))))
      (puthash tool-id tool-state (claude-cli-turn-state-tools turn))
      tool-state)))

(defun claude-cli-turn-manager-get-tool (manager tool-id)
  "Get tool state by TOOL-ID from current turn in MANAGER."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (gethash tool-id (claude-cli-turn-state-tools turn))))

(defun claude-cli-turn-manager-append-tool-input (manager tool-id partial)
  "Append PARTIAL input to tool TOOL-ID in MANAGER.
Returns the accumulated partial input."
  (when-let ((tool (claude-cli-turn-manager-get-tool manager tool-id)))
    (setf (claude-cli-tool-state-partial-input tool)
          (concat (claude-cli-tool-state-partial-input tool) partial))
    (claude-cli-tool-state-partial-input tool)))

(defun claude-cli-turn-manager-complete-tool (manager tool-id)
  "Mark tool TOOL-ID as complete in MANAGER.
Parses the accumulated partial input as JSON."
  (when-let ((tool (claude-cli-turn-manager-get-tool manager tool-id)))
    (let ((partial (claude-cli-tool-state-partial-input tool)))
      (when (and partial (> (length partial) 0))
        (condition-case nil
            (setf (claude-cli-tool-state-input tool)
                  (json-read-from-string partial))
          (json-error nil))))
    tool))

;;; Block state management (for streaming accumulation)

(defun claude-cli-turn-manager-set-block-state (manager index state)
  "Set block STATE at INDEX in current turn of MANAGER."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (puthash index state (claude-cli-turn-state-block-states turn))))

(defun claude-cli-turn-manager-get-block-state (manager index)
  "Get block state at INDEX from current turn in MANAGER."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (gethash index (claude-cli-turn-state-block-states turn))))

(defun claude-cli-turn-manager-clear-block-state (manager index)
  "Clear block state at INDEX from current turn in MANAGER."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (remhash index (claude-cli-turn-state-block-states turn))))

(defun claude-cli-turn-manager-append-block-input (manager index partial)
  "Append PARTIAL input to block at INDEX in MANAGER.
Returns the accumulated input string."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (let* ((states (claude-cli-turn-state-block-states turn))
           (state (gethash index states)))
      (when state
        (let ((current (or (plist-get state :partial-input) "")))
          (plist-put state :partial-input (concat current partial))
          (puthash index state states)
          (plist-get state :partial-input))))))

(defun claude-cli-turn-manager-get-block-input (manager index)
  "Get accumulated input for block at INDEX in MANAGER."
  (when-let ((state (claude-cli-turn-manager-get-block-state manager index)))
    (plist-get state :partial-input)))

;;; Turn completion

(defun claude-cli-turn-manager-complete-turn (manager result-msg)
  "Complete current turn in MANAGER with RESULT-MSG from CLI.
Returns a `claude-cli-turn-result' struct."
  (when-let ((turn (claude-cli-turn-manager-current manager)))
    (let* ((usage-data (plist-get result-msg :usage))
           (usage (claude-cli-turn-usage--create
                   :input-tokens (or (plist-get usage-data :input_tokens) 0)
                   :output-tokens (or (plist-get usage-data :output_tokens) 0)
                   :cache-read-tokens (or (plist-get usage-data :cache_read_input_tokens) 0)
                   :cache-creation-tokens (or (plist-get usage-data :cache_creation_input_tokens) 0)
                   :cost-usd (or (plist-get result-msg :total_cost_usd) 0.0))))
      (claude-cli-turn-result--create
       :turn-number (claude-cli-turn-state-number turn)
       :success (not (plist-get result-msg :is_error))
       :duration-ms (or (plist-get result-msg :duration_ms) 0)
       :duration-api-ms (or (plist-get result-msg :duration_api_ms) 0)
       :usage usage
       :error (when (plist-get result-msg :is_error)
                (plist-get result-msg :result))
       :result-text (claude-cli-turn-state-full-text turn)))))

(provide 'claude-cli-turn)
;;; claude-cli-turn.el ends here
