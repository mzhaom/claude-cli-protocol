;;; claude-test.el --- Tests for Claude SDK -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the Claude Elisp SDK.

;;; Code:

(require 'ert)
(require 'claude-cli)
(require 'claude-cli-protocol)
(require 'claude-cli-state)
(require 'claude-cli-turn)
(require 'claude-cli-events)
(require 'claude-cli-permission)

;;; Protocol tests

(ert-deftest claude-test-protocol-parse-message ()
  "Test JSON message parsing."
  (let ((msg (claude-cli-protocol-parse-message "{\"type\":\"system\",\"subtype\":\"init\"}")))
    (should (equal (plist-get msg :type) "system"))
    (should (equal (plist-get msg :subtype) "init"))))

(ert-deftest claude-test-protocol-encode-message ()
  "Test JSON message encoding."
  (let ((encoded (claude-cli-protocol-encode-message '(:type "user" :content "hello"))))
    (should (string-match-p "\"type\":\"user\"" encoded))
    (should (string-match-p "\"content\":\"hello\"" encoded))
    (should (string-suffix-p "\n" encoded))))

(ert-deftest claude-test-protocol-user-message ()
  "Test user message construction."
  (let ((msg (claude-cli-protocol-user-message "test content")))
    (should (equal (plist-get msg :type) "user"))
    (should (equal (plist-get (plist-get msg :message) :role) "user"))
    (should (equal (plist-get (plist-get msg :message) :content) "test content"))))

(ert-deftest claude-test-protocol-permission-mode-conversion ()
  "Test permission mode conversion."
  (should (equal (claude-cli-protocol-permission-mode-to-string 'default) "default"))
  (should (equal (claude-cli-protocol-permission-mode-to-string 'bypass) "bypassPermissions"))
  (should (equal (claude-cli-protocol-permission-mode-to-symbol "acceptEdits") 'accept-edits)))

;;; State machine tests

(ert-deftest claude-test-state-initial ()
  "Test initial state."
  (let ((state (claude-cli-state-create)))
    (should (claude-cli-state-is-p state 'uninitialized))))

(ert-deftest claude-test-state-transitions ()
  "Test valid state transitions."
  (let ((state (claude-cli-state-create)))
    ;; uninitialized -> starting
    (should (claude-cli-state-can-transition-p state 'start))
    (claude-cli-state-transition state 'start)
    (should (claude-cli-state-is-p state 'starting))

    ;; starting -> ready
    (should (claude-cli-state-can-transition-p state 'init-received))
    (claude-cli-state-transition state 'init-received)
    (should (claude-cli-state-ready-p state))

    ;; ready -> processing
    (claude-cli-state-transition state 'message-sent)
    (should (claude-cli-state-processing-p state))

    ;; processing -> ready
    (claude-cli-state-transition state 'result-received)
    (should (claude-cli-state-ready-p state))

    ;; ready -> closed
    (claude-cli-state-transition state 'close)
    (should (claude-cli-state-closed-p state))))

(ert-deftest claude-test-state-invalid-transition ()
  "Test invalid state transition signals error."
  (let ((state (claude-cli-state-create)))
    (should-error (claude-cli-state-transition state 'init-received)
                  :type 'claude-cli-state-error)))

;;; Turn manager tests

(ert-deftest claude-test-turn-manager-create ()
  "Test turn manager creation."
  (let ((manager (claude-cli-turn-manager-create)))
    (should (= (claude-cli-turn-manager-current-number manager) 0))
    (should (null (claude-cli-turn-manager-current manager)))))

(ert-deftest claude-test-turn-manager-start-turn ()
  "Test starting a turn."
  (let ((manager (claude-cli-turn-manager-create)))
    (claude-cli-turn-manager-start-turn manager "hello")
    (should (= (claude-cli-turn-manager-current-number manager) 1))
    (let ((turn (claude-cli-turn-manager-current manager)))
      (should turn)
      (should (= (claude-cli-turn-state-number turn) 1))
      (should (equal (claude-cli-turn-state-user-message turn) "hello")))))

(ert-deftest claude-test-turn-manager-text-accumulation ()
  "Test text accumulation."
  (let ((manager (claude-cli-turn-manager-create)))
    (claude-cli-turn-manager-start-turn manager "test")
    (should (equal (claude-cli-turn-manager-append-text manager "Hello") "Hello"))
    (should (equal (claude-cli-turn-manager-append-text manager " World") "Hello World"))
    (should (equal (claude-cli-turn-manager-get-full-text manager) "Hello World"))))

(ert-deftest claude-test-turn-manager-tool-tracking ()
  "Test tool state tracking."
  (let ((manager (claude-cli-turn-manager-create)))
    (claude-cli-turn-manager-start-turn manager "test")
    (claude-cli-turn-manager-start-tool manager "tool_123" "Read")
    (claude-cli-turn-manager-append-tool-input manager "tool_123" "{\"file\":")
    (claude-cli-turn-manager-append-tool-input manager "tool_123" "\"test.txt\"}")
    (let ((tool (claude-cli-turn-manager-complete-tool manager "tool_123")))
      (should tool)
      (should (equal (claude-cli-tool-state-name tool) "Read"))
      (should (equal (claude-cli-tool-state-input tool) '((file . "test.txt")))))))

;;; Events tests

(ert-deftest claude-test-event-type ()
  "Test event type detection."
  (should (eq (claude-cli-event-type (claude-cli-ready-event--create)) 'ready))
  (should (eq (claude-cli-event-type (claude-cli-text-event--create)) 'text))
  (should (eq (claude-cli-event-type (claude-cli-error-event--create)) 'error)))

(ert-deftest claude-test-event-dispatch ()
  "Test event dispatch to hooks."
  (let ((received nil))
    (add-hook 'claude-cli-text-hook (lambda (e) (push e received)))
    (unwind-protect
        (progn
          (claude-cli-events-dispatch (claude-cli-text-event--create :text "test"))
          (should (= (length received) 1))
          (should (equal (claude-cli-text-event-text (car received)) "test")))
      (remove-hook 'claude-cli-text-hook (lambda (e) (push e received))))))

;;; Permission tests

(ert-deftest claude-test-permission-allow ()
  "Test allow permission creation."
  (let ((decision (claude-cli-permission-allow)))
    (should (claude-cli-permission-decision-allow-p decision))
    (should (null (claude-cli-permission-decision-updated-input decision)))))

(ert-deftest claude-test-permission-allow-with-input ()
  "Test allow with updated input."
  (let ((decision (claude-cli-permission-allow '((modified . t)))))
    (should (claude-cli-permission-decision-allow-p decision))
    (should (equal (claude-cli-permission-decision-updated-input decision)
                   '((modified . t))))))

(ert-deftest claude-test-permission-deny ()
  "Test deny permission creation."
  (let ((decision (claude-cli-permission-deny "not allowed")))
    (should (claude-cli-permission-decision-deny-p decision))
    (should (equal (claude-cli-permission-decision-message decision) "not allowed"))
    (should (not (claude-cli-permission-decision-interrupt-p decision)))))

(ert-deftest claude-test-permission-deny-interrupt ()
  "Test deny with interrupt."
  (let ((decision (claude-cli-permission-deny "stopped" t)))
    (should (claude-cli-permission-decision-deny-p decision))
    (should (claude-cli-permission-decision-interrupt-p decision))))

(ert-deftest claude-test-permission-handlers ()
  "Test built-in permission handlers."
  (should (claude-cli-permission-decision-allow-p
           (claude-cli-permission-allow-all "Read" nil)))
  (should (claude-cli-permission-decision-deny-p
           (claude-cli-permission-deny-all "Write" nil))))

;;; Session tests

(ert-deftest claude-test-session-create ()
  "Test session creation with options."
  (let ((session (claude-cli-create-session :model "sonnet"
                                        :permission-mode 'bypass)))
    (should session)
    (let ((config (claude-cli-session-config session)))
      (should (equal (plist-get config :model) "sonnet"))
      (should (eq (plist-get config :permission-mode) 'bypass)))))

(ert-deftest claude-test-session-initial-state ()
  "Test session initial state."
  (let ((session (claude-cli-create-session)))
    (should (eq (claude-cli-session-get-state session) 'uninitialized))
    (should (not (claude-cli-session-ready-p session)))
    (should (not (claude-cli-session-started session)))))

(ert-deftest claude-test-session-cannot-send-before-start ()
  "Test that sending before start signals error."
  (let ((session (claude-cli-create-session)))
    (should-error (claude-cli-send-message session "test")
                  :type 'claude-cli-session-error)))

(provide 'claude-test)
;;; claude-test.el ends here
