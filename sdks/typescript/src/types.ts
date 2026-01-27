/**
 * Protocol message types for Claude Code CLI - Live CLI Format
 * Based on actual CLI output, not theoretical spec
 */

// ============================================================================
// Live CLI Message Format - ALL messages have "type" field at top level
// ============================================================================

/**
 * SystemMessage - Session initialization and system events
 * Discriminated by: type === "system"
 */
export interface SystemMessage {
  type: 'system'
  subtype: 'init' | 'hook_response' | 'setting_response'
  session_id: string
  uuid: string

  // Init-specific fields (when subtype === 'init')
  cwd?: string
  tools?: string[]
  mcp_servers?: string[]
  model?: string
  permissionMode?: string
  slash_commands?: string[]
  agents?: string[]
  skills?: string[]
  plugins?: string[]
  apiKeySource?: string
  output_style?: string
  claude_code_version?: string

  // Hook response fields (when subtype === 'hook_response')
  hook_name?: string
  hook_event?: string
  stdout?: string
  stderr?: string
  exit_code?: number
}

/**
 * AssistantMessage - Complete message from Claude
 * Discriminated by: type === "assistant"
 * Note: Content is wrapped in a "message" field!
 */
export interface AssistantMessage {
  type: 'assistant'
  message: {
    model: string
    id: string
    type: 'message'
    role: 'assistant'
    content: ContentBlock[]
    stop_reason: string | null
    stop_sequence: string | null
    usage: {
      input_tokens: number
      cache_creation_input_tokens: number
      cache_read_input_tokens: number
      cache_creation: {
        ephemeral_5m_input_tokens: number
        ephemeral_1h_input_tokens: number
      }
      output_tokens: number
      service_tier: string
    }
    context_management: unknown | null
  }
  parent_tool_use_id: string | null
  session_id: string
  uuid: string
}

/**
 * UserMessage - Tool results echoed back
 * Discriminated by: type === "user"
 */
export interface UserMessage {
  type: 'user'
  message: {
    role: 'user'
    content: ContentBlock[]
  }
  session_id: string
  uuid: string
  parent_tool_use_id: string | null
}

/**
 * ResultMessage - Turn completion with metrics
 * Discriminated by: type === "result"
 */
export interface ResultMessage {
  type: 'result'
  subtype: 'success' | 'error'
  is_error: boolean
  duration_ms: number
  duration_api_ms: number
  num_turns: number
  result: string
  session_id: string
  total_cost_usd: number
  usage: {
    input_tokens: number
    cache_creation_input_tokens: number
    cache_read_input_tokens: number
    output_tokens: number
    server_tool_use: {
      web_search_requests: number
      web_fetch_requests: number
    }
    service_tier: string
    cache_creation: {
      ephemeral_1h_input_tokens: number
      ephemeral_5m_input_tokens: number
    }
  }
  modelUsage: Record<string, {
    inputTokens: number
    outputTokens: number
    cacheReadInputTokens: number
    cacheCreationInputTokens: number
    webSearchRequests: number
    costUSD: number
    contextWindow: number
    maxOutputTokens: number
  }>
  permission_denials: unknown[]
  uuid: string
}

/**
 * StreamEvent - Real-time streaming updates
 * Discriminated by: type === "stream_event"
 */
export interface StreamEvent {
  type: 'stream_event'
  event: StreamEventData
  session_id: string
  uuid: string
  parent_tool_use_id: string | null
}

export type StreamEventData =
  | MessageStartEvent
  | ContentBlockStartEvent
  | ContentBlockDeltaEvent
  | ContentBlockStopEvent
  | MessageDeltaEvent
  | MessageStopEvent

export interface MessageStartEvent {
  type: 'message_start'
  message: {
    model: string
    id: string
    type: 'message'
    role: 'assistant'
    content: ContentBlock[]
    stop_reason: string | null
    stop_sequence: string | null
    usage: {
      input_tokens: number
      cache_creation_input_tokens: number
      cache_read_input_tokens: number
      cache_creation: {
        ephemeral_5m_input_tokens: number
        ephemeral_1h_input_tokens: number
      }
      output_tokens: number
      service_tier: string
    }
  }
}

export interface ContentBlockStartEvent {
  type: 'content_block_start'
  index: number
  content_block:
    | { type: 'text'; text: string }
    | { type: 'thinking'; thinking: string }
    | { type: 'tool_use'; id: string; name: string; input?: Record<string, unknown> }
}

export interface ContentBlockDeltaEvent {
  type: 'content_block_delta'
  index: number
  delta:
    | { type: 'text_delta'; text: string }
    | { type: 'thinking_delta'; thinking: string }
    | { type: 'input_json_delta'; partial_json: string }
}

export interface ContentBlockStopEvent {
  type: 'content_block_stop'
  index: number
}

export interface MessageDeltaEvent {
  type: 'message_delta'
  delta: {
    stop_reason: string | null
    stop_sequence: string | null
  }
  usage: {
    input_tokens: number
    cache_creation_input_tokens: number
    cache_read_input_tokens: number
    output_tokens: number
  }
}

export interface MessageStopEvent {
  type: 'message_stop'
}

// ============================================================================
// Content Blocks
// ============================================================================

export interface TextBlock {
  type: 'text'
  text: string
}

export interface ThinkingBlock {
  type: 'thinking'
  thinking: string
  signature?: string
}

export interface ToolUseBlock {
  type: 'tool_use'
  id: string
  name: string
  input: Record<string, unknown>
}

export interface ToolResultBlock {
  type: 'tool_result'
  tool_use_id: string
  content: string | ContentBlock[]
  is_error: boolean | null
}

export type ContentBlock = TextBlock | ThinkingBlock | ToolUseBlock | ToolResultBlock

// ============================================================================
// Control Protocol (NOT YET SEEN IN LIVE TRACES)
// ============================================================================

export interface ControlRequest {
  type: 'control_request'
  request_id: string
  request: ControlRequestData
}

export type ControlRequestData =
  | CanUseToolRequest
  | SetPermissionModeRequest
  | InterruptRequest

export interface CanUseToolRequest {
  subtype: 'can_use_tool'
  tool_name: string
  input: Record<string, unknown>
  permission_suggestions?: unknown[]
  blocked_path?: string | null
}

export interface SetPermissionModeRequest {
  subtype: 'set_permission_mode'
  mode: 'default' | 'acceptEdits' | 'plan' | 'bypassPermissions'
}

export interface InterruptRequest {
  subtype: 'interrupt'
}

export interface ControlResponse {
  type: 'control_response'
  response: {
    subtype: 'success' | 'error'
    request_id: string
    response?: unknown
    error?: string
  }
}

// ============================================================================
// Messages TO CLI (what we send)
// ============================================================================

export interface UserMessageToSend {
  type: 'user'
  message: {
    role: 'user'
    content: string | ContentBlock[]
  }
}

// ============================================================================
// Discriminated Union
// ============================================================================

export type ProtocolMessage =
  | SystemMessage
  | AssistantMessage
  | UserMessage
  | ResultMessage
  | StreamEvent
  | ControlRequest
  | ControlResponse

export type MessageType =
  | 'system'
  | 'assistant'
  | 'user'
  | 'result'
  | 'stream_event'
  | 'control_request'
  | 'control_response'
  | 'unknown'
