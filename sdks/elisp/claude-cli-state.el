;;; claude-cli-state.el --- State machine for Claude sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude CLI Wrapper Contributors
;; Keywords: tools, processes
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module implements the session state machine for Claude CLI sessions.
;;
;; State transitions:
;;   uninitialized --[start]--> starting
;;   starting --[init-received]--> ready
;;   ready --[message-sent]--> processing
;;   processing --[result-received]--> ready
;;   any --[close]--> closed
;;
;; The state machine ensures valid state transitions and provides
;; hooks for state change notifications.

;;; Code:

(require 'cl-lib)

;;; State constants

(defconst claude-cli-state-uninitialized 'uninitialized
  "Initial state before session is started.")

(defconst claude-cli-state-starting 'starting
  "State after start is called, waiting for init message.")

(defconst claude-cli-state-ready 'ready
  "State when session is ready to accept messages.")

(defconst claude-cli-state-processing 'processing
  "State when processing a user message.")

(defconst claude-cli-state-closed 'closed
  "Terminal state after session is closed.")

;;; State transition table

(defconst claude-cli-state-transitions
  '((uninitialized . ((start . starting)
                      (close . closed)))
    ;; Note: CLI sends init message after first user message,
    ;; so we allow message-sent from starting state
    (starting . ((init-received . ready)
                 (message-sent . processing)
                 (close . closed)))
    (ready . ((message-sent . processing)
              (close . closed)))
    (processing . ((result-received . ready)
                   (init-received . processing)  ; init can arrive during first turn
                   (close . closed)))
    (closed . ()))
  "Valid state transitions as alist of alists.
Each entry maps a state to an alist of (trigger . new-state) pairs.")

;;; Error type

(define-error 'claude-cli-state-error "Claude state error")

;;; State struct

(cl-defstruct (claude-cli-state (:constructor claude-cli-state--create)
                            (:copier nil))
  "Session state machine."
  (current claude-cli-state-uninitialized :type symbol))

;;; State functions

(defun claude-cli-state-create ()
  "Create a new state machine in uninitialized state."
  (claude-cli-state--create))

(defun claude-cli-state-can-transition-p (state trigger)
  "Check if STATE can transition via TRIGGER."
  (let* ((current (claude-cli-state-current state))
         (transitions (alist-get current claude-cli-state-transitions)))
    (not (null (alist-get trigger transitions)))))

(defun claude-cli-state-transition (state trigger)
  "Attempt to transition STATE via TRIGGER.
Returns the new state symbol on success.
Signals `claude-cli-state-error' if transition is invalid."
  (let* ((current (claude-cli-state-current state))
         (transitions (alist-get current claude-cli-state-transitions))
         (new-state (alist-get trigger transitions)))
    (if new-state
        (progn
          (setf (claude-cli-state-current state) new-state)
          new-state)
      (signal 'claude-cli-state-error
              (list (format "Invalid transition '%s' from state '%s'"
                            trigger current))))))

(defun claude-cli-state-is-p (state expected)
  "Check if STATE's current state equals EXPECTED."
  (eq (claude-cli-state-current state) expected))

(defun claude-cli-state-ready-p (state)
  "Check if STATE is ready."
  (claude-cli-state-is-p state claude-cli-state-ready))

(defun claude-cli-state-processing-p (state)
  "Check if STATE is processing."
  (claude-cli-state-is-p state claude-cli-state-processing))

(defun claude-cli-state-closed-p (state)
  "Check if STATE is closed."
  (claude-cli-state-is-p state claude-cli-state-closed))

(defun claude-cli-state-active-p (state)
  "Check if STATE is active (not closed or uninitialized)."
  (memq (claude-cli-state-current state)
        (list claude-cli-state-starting
              claude-cli-state-ready
              claude-cli-state-processing)))

(provide 'claude-cli-state)
;;; claude-cli-state.el ends here
