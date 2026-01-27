/**
 * Unit tests for SessionState state machine
 */

import { describe, it, expect, beforeEach } from 'bun:test'
import {
  SessionState,
  SessionStateMachine,
  STATE_TRANSITIONS,
  ProcessingContext
} from '../SessionState'

describe('SessionState', () => {
  describe('State Enum', () => {
    it('should have all required states', () => {
      expect(SessionState.STARTING).toBe('STARTING')
      expect(SessionState.READY).toBe('READY')
      expect(SessionState.PROCESSING).toBe('PROCESSING')
      expect(SessionState.CLOSED).toBe('CLOSED')
    })
  })

  describe('STATE_TRANSITIONS', () => {
    it('should have all valid transitions defined', () => {
      expect(STATE_TRANSITIONS.length).toBeGreaterThan(0)

      // Check specific transitions exist
      const transitions = STATE_TRANSITIONS.map((t) => `${t.from}->${t.to}:${t.on}`)

      expect(transitions).toContain('STARTING->READY:system_init_received')
      expect(transitions).toContain('STARTING->CLOSED:init_error')
      expect(transitions).toContain('READY->PROCESSING:user_message_sent')
      expect(transitions).toContain('PROCESSING->READY:result_received')
      expect(transitions).toContain('*->CLOSED:cli_exit')
    })
  })
})

describe('SessionStateMachine', () => {
  let stateMachine: SessionStateMachine

  beforeEach(() => {
    stateMachine = new SessionStateMachine()
  })

  describe('Initialization', () => {
    it('should start in STARTING state by default', () => {
      expect(stateMachine.getState()).toBe(SessionState.STARTING)
    })

    it('should accept custom initial state', () => {
      const machine = new SessionStateMachine(SessionState.READY)
      expect(machine.getState()).toBe(SessionState.READY)
    })

    it('should have empty processing context initially', () => {
      const context = stateMachine.getProcessingContext()
      expect(context.isStreaming).toBe(false)
      expect(context.pendingPermission).toBeUndefined()
      expect(context.currentTool).toBeUndefined()
    })
  })

  describe('Valid State Transitions', () => {
    it('should transition from STARTING to READY on system_init_received', () => {
      expect(stateMachine.getState()).toBe(SessionState.STARTING)

      const success = stateMachine.transition('system_init_received')
      expect(success).toBe(true)
      expect(stateMachine.getState()).toBe(SessionState.READY)
    })

    it('should transition from STARTING to CLOSED on init_error', () => {
      expect(stateMachine.getState()).toBe(SessionState.STARTING)

      const success = stateMachine.transition('init_error')
      expect(success).toBe(true)
      expect(stateMachine.getState()).toBe(SessionState.CLOSED)
    })

    it('should transition from READY to PROCESSING on user_message_sent', () => {
      stateMachine = new SessionStateMachine(SessionState.READY)

      const success = stateMachine.transition('user_message_sent')
      expect(success).toBe(true)
      expect(stateMachine.getState()).toBe(SessionState.PROCESSING)
    })

    it('should transition from PROCESSING to READY on result_received', () => {
      stateMachine = new SessionStateMachine(SessionState.PROCESSING)

      const success = stateMachine.transition('result_received')
      expect(success).toBe(true)
      expect(stateMachine.getState()).toBe(SessionState.READY)
    })

    it('should transition from any state to CLOSED on cli_exit', () => {
      const states = [
        SessionState.STARTING,
        SessionState.READY,
        SessionState.PROCESSING
      ]

      for (const state of states) {
        const machine = new SessionStateMachine(state)
        const success = machine.transition('cli_exit')
        expect(success).toBe(true)
        expect(machine.getState()).toBe(SessionState.CLOSED)
      }
    })
  })

  describe('Invalid State Transitions', () => {
    it('should reject user_message_sent from STARTING state', () => {
      expect(stateMachine.getState()).toBe(SessionState.STARTING)

      const success = stateMachine.transition('user_message_sent')
      expect(success).toBe(false)
      expect(stateMachine.getState()).toBe(SessionState.STARTING) // State unchanged
    })

    it('should reject result_received from READY state', () => {
      stateMachine = new SessionStateMachine(SessionState.READY)

      const success = stateMachine.transition('result_received')
      expect(success).toBe(false)
      expect(stateMachine.getState()).toBe(SessionState.READY)
    })

    it('should reject system_init_received from READY state', () => {
      stateMachine = new SessionStateMachine(SessionState.READY)

      const success = stateMachine.transition('system_init_received')
      expect(success).toBe(false)
      expect(stateMachine.getState()).toBe(SessionState.READY)
    })

    it('should reject transitions from CLOSED state (except cli_exit)', () => {
      stateMachine = new SessionStateMachine(SessionState.CLOSED)

      const events = ['system_init_received', 'user_message_sent', 'result_received', 'init_error']

      for (const event of events) {
        const success = stateMachine.transition(event as any)
        expect(success).toBe(false)
        expect(stateMachine.getState()).toBe(SessionState.CLOSED)
      }
    })
  })

  describe('Processing Context Management', () => {
    beforeEach(() => {
      stateMachine = new SessionStateMachine(SessionState.PROCESSING)
    })

    it('should update processing context', () => {
      stateMachine.updateProcessingContext({
        isStreaming: true
      })

      const context = stateMachine.getProcessingContext()
      expect(context.isStreaming).toBe(true)
    })

    it('should update pending permission', () => {
      stateMachine.updateProcessingContext({
        pendingPermission: {
          requestId: 'req_123',
          toolName: 'Bash',
          input: { command: 'ls' }
        }
      })

      const context = stateMachine.getProcessingContext()
      expect(context.pendingPermission).toBeDefined()
      expect(context.pendingPermission?.requestId).toBe('req_123')
      expect(context.pendingPermission?.toolName).toBe('Bash')
    })

    it('should update current tool', () => {
      stateMachine.updateProcessingContext({
        currentTool: {
          id: 'tool_123',
          name: 'Read'
        }
      })

      const context = stateMachine.getProcessingContext()
      expect(context.currentTool).toBeDefined()
      expect(context.currentTool?.id).toBe('tool_123')
      expect(context.currentTool?.name).toBe('Read')
    })

    it('should merge context updates', () => {
      stateMachine.updateProcessingContext({
        isStreaming: true
      })

      stateMachine.updateProcessingContext({
        currentTool: {
          id: 'tool_123',
          name: 'Read'
        }
      })

      const context = stateMachine.getProcessingContext()
      expect(context.isStreaming).toBe(true)
      expect(context.currentTool?.id).toBe('tool_123')
    })

    it('should reset processing context when leaving PROCESSING state', () => {
      stateMachine.updateProcessingContext({
        isStreaming: true,
        currentTool: { id: 'tool_123', name: 'Read' },
        pendingPermission: {
          requestId: 'req_123',
          toolName: 'Bash',
          input: {}
        }
      })

      // Transition to READY
      stateMachine.transition('result_received')

      const context = stateMachine.getProcessingContext()
      expect(context.isStreaming).toBe(false)
      expect(context.currentTool).toBeUndefined()
      expect(context.pendingPermission).toBeUndefined()
    })

    it('should return a copy of context (not reference)', () => {
      const context1 = stateMachine.getProcessingContext()
      context1.isStreaming = true

      const context2 = stateMachine.getProcessingContext()
      expect(context2.isStreaming).toBe(false) // Original unchanged
    })
  })

  describe('canTransition', () => {
    it('should check if transition is valid without executing it', () => {
      expect(stateMachine.getState()).toBe(SessionState.STARTING)

      expect(stateMachine.canTransition('system_init_received')).toBe(true)
      expect(stateMachine.canTransition('user_message_sent')).toBe(false)

      // State should not change
      expect(stateMachine.getState()).toBe(SessionState.STARTING)
    })

    it('should check wildcard transitions', () => {
      const states = [
        SessionState.STARTING,
        SessionState.READY,
        SessionState.PROCESSING,
        SessionState.CLOSED
      ]

      for (const state of states) {
        const machine = new SessionStateMachine(state)
        expect(machine.canTransition('cli_exit')).toBe(true)
      }
    })
  })

  describe('reset', () => {
    it('should reset to STARTING state', () => {
      stateMachine = new SessionStateMachine(SessionState.READY)
      stateMachine.updateProcessingContext({ isStreaming: true })

      stateMachine.reset()

      expect(stateMachine.getState()).toBe(SessionState.STARTING)
    })

    it('should clear processing context on reset', () => {
      stateMachine = new SessionStateMachine(SessionState.PROCESSING)
      stateMachine.updateProcessingContext({
        isStreaming: true,
        currentTool: { id: 'tool_123', name: 'Read' }
      })

      stateMachine.reset()

      const context = stateMachine.getProcessingContext()
      expect(context.isStreaming).toBe(false)
      expect(context.currentTool).toBeUndefined()
    })
  })

  describe('Complete State Flow', () => {
    it('should execute full happy path flow', () => {
      // Start
      expect(stateMachine.getState()).toBe(SessionState.STARTING)

      // Init received
      stateMachine.transition('system_init_received')
      expect(stateMachine.getState()).toBe(SessionState.READY)

      // User sends message
      stateMachine.transition('user_message_sent')
      expect(stateMachine.getState()).toBe(SessionState.PROCESSING)

      // Update context during processing
      stateMachine.updateProcessingContext({ isStreaming: true })
      expect(stateMachine.getProcessingContext().isStreaming).toBe(true)

      // Result received
      stateMachine.transition('result_received')
      expect(stateMachine.getState()).toBe(SessionState.READY)
      expect(stateMachine.getProcessingContext().isStreaming).toBe(false)

      // Another turn
      stateMachine.transition('user_message_sent')
      expect(stateMachine.getState()).toBe(SessionState.PROCESSING)

      stateMachine.transition('result_received')
      expect(stateMachine.getState()).toBe(SessionState.READY)

      // Exit
      stateMachine.transition('cli_exit')
      expect(stateMachine.getState()).toBe(SessionState.CLOSED)
    })

    it('should handle init error path', () => {
      expect(stateMachine.getState()).toBe(SessionState.STARTING)

      stateMachine.transition('init_error')
      expect(stateMachine.getState()).toBe(SessionState.CLOSED)

      // Should not be able to transition from CLOSED
      stateMachine.transition('system_init_received')
      expect(stateMachine.getState()).toBe(SessionState.CLOSED)
    })
  })
})
