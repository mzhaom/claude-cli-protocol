/**
 * Claude Code CLI Session API
 *
 * High-level API for building UIs on top of Claude Code CLI.
 * Handles multi-turn conversations and provides detailed tool lifecycle events.
 *
 * Quick Start:
 * ```typescript
 * import { ClaudeSession } from './protocol'
 *
 * const session = new ClaudeSession({
 *   onText: (chunk) => updateUI(chunk.text),
 *   onToolComplete: (tool) => executeTool(tool),
 *   onTurnComplete: (turn) => console.log('Done!')
 * })
 *
 * session.start()
 * session.sendMessage('Hello!')
 * ```
 */

// ============================================================================
// High-Level Session API (Recommended for all users)
// ============================================================================

export { ClaudeSession } from './ClaudeSession'
export type {
  // Configuration
  SessionConfig,
  SessionInfo,

  // Turn lifecycle
  TurnComplete,

  // Message streaming
  TextChunk,
  ThinkingChunk,

  // Tool lifecycle
  ToolStart,
  ToolProgress,
  CliToolResult,

  // Errors
  SessionError
} from './ClaudeSession'

// Recording
export { SessionRecorder } from './SessionRecorder'
export type {
  RecordedMessage,
  SessionRecording,
  TurnSummary
} from './SessionRecorder'

// ============================================================================
// Protocol Types (for advanced usage)
// ============================================================================

export type {
  // Message types
  SystemMessage,
  AssistantMessage,
  UserMessage,
  ResultMessage,
  StreamEvent,
  ProtocolMessage,
  MessageType,

  // Content blocks
  TextBlock,
  ThinkingBlock,
  ToolUseBlock,
  ToolResultBlock,
  ContentBlock,

  // Stream events
  StreamEventData,
  MessageStartEvent,
  ContentBlockStartEvent,
  ContentBlockDeltaEvent,
  ContentBlockStopEvent,
  MessageDeltaEvent,
  MessageStopEvent,

  // Control protocol
  ControlRequest,
  ControlResponse,
  ControlRequestData,
  CanUseToolRequest,
  SetPermissionModeRequest,
  InterruptRequest,

  // Messages to send
  UserMessageToSend
} from './types'

// ============================================================================
// Low-Level Protocol Components (Advanced)
// ============================================================================

// Only needed if you're building custom protocol handlers
// Most users should use ClaudeSession instead

export { MessageDiscriminator } from './MessageDiscriminator'

export { StreamAccumulator } from './StreamAccumulator'
export type {
  StreamAccumulatorEvents,
  StreamAccumulatorEventType,
  StreamAccumulatorEventHandler
} from './StreamAccumulator'

export { SessionState, SessionStateMachine, STATE_TRANSITIONS } from './SessionState'
export type { ProcessingContext, StateTransitionEvent, StateTransition } from './SessionState'

export { PermissionHandler } from './PermissionHandler'
export type { PermissionDecision, PermissionRequestData } from './PermissionHandler'

// Modular components for custom implementations
export { TurnManager } from './TurnManager'
export type { TurnState } from './TurnManager'

export { ProcessManager } from './ProcessManager'
export type { ProcessConfig } from './ProcessManager'

export { EventCoordinator } from './EventCoordinator'
export type { EventCallbacks } from './EventCoordinator'
