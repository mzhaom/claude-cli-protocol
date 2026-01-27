/**
 * SessionState - State machine for session lifecycle
 * Based on SESSION_API_DESIGN.md Section 5
 */

/**
 * Session lifecycle states
 */
export enum SessionState {
  STARTING = 'STARTING', // CLI process spawned, waiting for init
  READY = 'READY', // Idle, waiting for user input
  PROCESSING = 'PROCESSING', // Claude is working (streaming, tool execution, etc.)
  CLOSED = 'CLOSED' // Session ended
}

/**
 * Context information during PROCESSING state
 */
export interface ProcessingContext {
  isStreaming: boolean // Currently receiving stream events
  pendingPermission?: {
    // Blocked on permission decision
    requestId: string
    toolName: string
    input: Record<string, unknown>
  }
  currentTool?: {
    // Tool currently executing
    id: string
    name: string
  }
}

/**
 * State transition events
 */
export type StateTransitionEvent =
  | 'system_init_received'
  | 'init_error'
  | 'user_message_sent'
  | 'result_received'
  | 'cli_exit'

/**
 * State transition definition
 */
export interface StateTransition {
  from: SessionState | '*'
  to: SessionState
  on: StateTransitionEvent
}

/**
 * Valid state transitions per spec Section 5.2
 */
export const STATE_TRANSITIONS: StateTransition[] = [
  { from: SessionState.STARTING, to: SessionState.READY, on: 'system_init_received' },
  { from: SessionState.STARTING, to: SessionState.CLOSED, on: 'init_error' },
  { from: SessionState.READY, to: SessionState.PROCESSING, on: 'user_message_sent' },
  { from: SessionState.PROCESSING, to: SessionState.READY, on: 'result_received' },
  { from: '*', to: SessionState.CLOSED, on: 'cli_exit' }
]

/**
 * State machine for session lifecycle
 */
export class SessionStateMachine {
  private currentState: SessionState
  private processingContext: ProcessingContext

  constructor(initialState: SessionState = SessionState.STARTING) {
    this.currentState = initialState
    this.processingContext = {
      isStreaming: false
    }
  }

  /**
   * Get current state
   */
  getState(): SessionState {
    return this.currentState
  }

  /**
   * Get processing context (only valid in PROCESSING state)
   */
  getProcessingContext(): ProcessingContext {
    return { ...this.processingContext }
  }

  /**
   * Update processing context
   */
  updateProcessingContext(updates: Partial<ProcessingContext>): void {
    this.processingContext = { ...this.processingContext, ...updates }
  }

  /**
   * Attempt state transition
   * @returns true if transition was valid, false otherwise
   */
  transition(event: StateTransitionEvent): boolean {
    const validTransition = STATE_TRANSITIONS.find((t) => {
      return (t.from === '*' || t.from === this.currentState) && t.on === event
    })

    if (validTransition) {
      const oldState = this.currentState
      this.currentState = validTransition.to

      // Reset processing context when leaving PROCESSING state
      if (oldState === SessionState.PROCESSING && validTransition.to !== SessionState.PROCESSING) {
        this.processingContext = {
          isStreaming: false
        }
      }

      return true
    }

    return false
  }

  /**
   * Check if a transition is valid
   */
  canTransition(event: StateTransitionEvent): boolean {
    return STATE_TRANSITIONS.some((t) => {
      return (t.from === '*' || t.from === this.currentState) && t.on === event
    })
  }

  /**
   * Reset to initial state
   */
  reset(): void {
    this.currentState = SessionState.STARTING
    this.processingContext = {
      isStreaming: false
    }
  }
}
