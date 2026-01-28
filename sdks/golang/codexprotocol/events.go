package codexprotocol

// Event is a message from Codex to the client.
// Source: codex-rs/protocol/src/protocol.rs:614-621
type Event struct {
	// ID is the correlated submission ID.
	ID string `json:"id"`
	// Msg is the parsed event payload.
	Msg EventMsg `json:"-"`
}

// EventMsgType discriminates between event kinds.
type EventMsgType string

const (
	// Session events
	EventTypeSessionConfigured EventMsgType = "session_configured"
	EventTypeShutdownComplete  EventMsgType = "shutdown_complete"

	// Turn lifecycle - note: v1 uses task_* names
	EventTypeTurnStarted EventMsgType = "task_started"
	EventTypeTurnComplete EventMsgType = "task_complete"
	EventTypeTurnAborted EventMsgType = "turn_aborted"

	// Agent output
	EventTypeAgentMessage                  EventMsgType = "agent_message"
	EventTypeAgentMessageDelta             EventMsgType = "agent_message_delta"
	EventTypeAgentReasoning                EventMsgType = "agent_reasoning"
	EventTypeAgentReasoningDelta           EventMsgType = "agent_reasoning_delta"
	EventTypeAgentReasoningRawContent      EventMsgType = "agent_reasoning_raw_content"
	EventTypeAgentReasoningRawContentDelta EventMsgType = "agent_reasoning_raw_content_delta"
	EventTypeAgentReasoningSectionBreak    EventMsgType = "agent_reasoning_section_break"
	EventTypeUserMessage                   EventMsgType = "user_message"

	// Approval requests
	EventTypeExecApprovalRequest      EventMsgType = "exec_approval_request"
	EventTypeApplyPatchApprovalRequest EventMsgType = "apply_patch_approval_request"
	EventTypeElicitationRequest       EventMsgType = "elicitation_request"

	// Command execution
	EventTypeExecCommandBegin       EventMsgType = "exec_command_begin"
	EventTypeExecCommandOutputDelta EventMsgType = "exec_command_output_delta"
	EventTypeExecCommandEnd         EventMsgType = "exec_command_end"
	EventTypeTerminalInteraction    EventMsgType = "terminal_interaction"

	// Patch application
	EventTypePatchApplyBegin EventMsgType = "patch_apply_begin"
	EventTypePatchApplyEnd   EventMsgType = "patch_apply_end"

	// MCP events
	EventTypeMcpStartupUpdate    EventMsgType = "mcp_startup_update"
	EventTypeMcpStartupComplete  EventMsgType = "mcp_startup_complete"
	EventTypeMcpToolCallBegin    EventMsgType = "mcp_tool_call_begin"
	EventTypeMcpToolCallEnd      EventMsgType = "mcp_tool_call_end"
	EventTypeMcpListToolsResponse EventMsgType = "mcp_list_tools_response"

	// Web search
	EventTypeWebSearchBegin EventMsgType = "web_search_begin"
	EventTypeWebSearchEnd   EventMsgType = "web_search_end"

	// Status events
	EventTypeError           EventMsgType = "error"
	EventTypeWarning         EventMsgType = "warning"
	EventTypeTokenCount      EventMsgType = "token_count"
	EventTypeBackgroundEvent EventMsgType = "background_event"
	EventTypeStreamError     EventMsgType = "stream_error"

	// Context events
	EventTypeContextCompacted  EventMsgType = "context_compacted"
	EventTypeThreadRolledBack  EventMsgType = "thread_rolled_back"

	// Other events
	EventTypeDeprecationNotice         EventMsgType = "deprecation_notice"
	EventTypeUndoStarted               EventMsgType = "undo_started"
	EventTypeUndoCompleted             EventMsgType = "undo_completed"
	EventTypeListCustomPromptsResponse EventMsgType = "list_custom_prompts_response"
	EventTypeListSkillsResponse        EventMsgType = "list_skills_response"
	EventTypeSkillsUpdateAvailable     EventMsgType = "skills_update_available"
	EventTypeTurnDiff                  EventMsgType = "turn_diff"
	EventTypeGetHistoryEntryResponse   EventMsgType = "get_history_entry_response"
	EventTypeEnteredReviewMode         EventMsgType = "entered_review_mode"
	EventTypeExitedReviewMode          EventMsgType = "exited_review_mode"
	EventTypeViewImageToolCall         EventMsgType = "view_image_tool_call"
	EventTypePlanUpdate                EventMsgType = "plan_update"
)

// EventMsg is the interface for all events from Codex.
type EventMsg interface {
	eventMsgType() EventMsgType
}

// SessionConfiguredEvent acknowledges session initialization.
type SessionConfiguredEvent struct {
	Type            EventMsgType `json:"type"`
	SessionID       string       `json:"session_id"`
	Model           string       `json:"model"`
	ModelProviderID string       `json:"model_provider_id,omitempty"`
	ApprovalPolicy  string       `json:"approval_policy"`
	SandboxPolicy   interface{}  `json:"sandbox_policy"` // Can be string or object
	CWD             string       `json:"cwd"`
	ReasoningEffort string       `json:"reasoning_effort,omitempty"`
	HistoryLogID    uint64       `json:"history_log_id,omitempty"`
	HistoryEntryCount int        `json:"history_entry_count,omitempty"`
	Tools           []string     `json:"tools,omitempty"`
}

func (e SessionConfiguredEvent) eventMsgType() EventMsgType { return EventTypeSessionConfigured }

// ShutdownCompleteEvent signals session termination.
type ShutdownCompleteEvent struct {
	Type EventMsgType `json:"type"`
}

func (e ShutdownCompleteEvent) eventMsgType() EventMsgType { return EventTypeShutdownComplete }

// TurnStartedEvent signals turn start.
type TurnStartedEvent struct {
	Type               EventMsgType `json:"type"`
	TurnID             string       `json:"turn_id,omitempty"`
	ModelContextWindow *int64       `json:"model_context_window,omitempty"`
}

func (e TurnStartedEvent) eventMsgType() EventMsgType { return EventTypeTurnStarted }

// TurnCompleteEvent signals turn completion.
type TurnCompleteEvent struct {
	Type             EventMsgType `json:"type"`
	TurnID           string       `json:"turn_id,omitempty"`
	LastAgentMessage string       `json:"last_agent_message,omitempty"`
}

func (e TurnCompleteEvent) eventMsgType() EventMsgType { return EventTypeTurnComplete }

// TurnAbortedEvent signals turn interruption.
type TurnAbortedEvent struct {
	Type    EventMsgType `json:"type"`
	TurnID  string       `json:"turn_id,omitempty"`
	Message string       `json:"message,omitempty"`
}

func (e TurnAbortedEvent) eventMsgType() EventMsgType { return EventTypeTurnAborted }

// AgentMessageEvent contains complete agent text.
type AgentMessageEvent struct {
	Type    EventMsgType `json:"type"`
	Message string       `json:"message"`
}

func (e AgentMessageEvent) eventMsgType() EventMsgType { return EventTypeAgentMessage }

// AgentMessageDeltaEvent contains streaming text chunks.
type AgentMessageDeltaEvent struct {
	Type  EventMsgType `json:"type"`
	Delta string       `json:"delta"`
}

func (e AgentMessageDeltaEvent) eventMsgType() EventMsgType { return EventTypeAgentMessageDelta }

// AgentReasoningEvent contains complete reasoning.
type AgentReasoningEvent struct {
	Type EventMsgType `json:"type"`
	Text string       `json:"text"`
}

func (e AgentReasoningEvent) eventMsgType() EventMsgType { return EventTypeAgentReasoning }

// AgentReasoningDeltaEvent contains streaming reasoning chunks.
type AgentReasoningDeltaEvent struct {
	Type  EventMsgType `json:"type"`
	Delta string       `json:"delta"`
}

func (e AgentReasoningDeltaEvent) eventMsgType() EventMsgType { return EventTypeAgentReasoningDelta }

// AgentReasoningRawContentEvent contains raw chain-of-thought.
type AgentReasoningRawContentEvent struct {
	Type EventMsgType `json:"type"`
	Text string       `json:"text"`
}

func (e AgentReasoningRawContentEvent) eventMsgType() EventMsgType { return EventTypeAgentReasoningRawContent }

// AgentReasoningRawContentDeltaEvent contains streaming raw reasoning.
type AgentReasoningRawContentDeltaEvent struct {
	Type  EventMsgType `json:"type"`
	Delta string       `json:"delta"`
}

func (e AgentReasoningRawContentDeltaEvent) eventMsgType() EventMsgType { return EventTypeAgentReasoningRawContentDelta }

// UserMessageEvent contains user input echoed.
type UserMessageEvent struct {
	Type    EventMsgType `json:"type"`
	Message string       `json:"message"`
	Images  []string     `json:"images,omitempty"`
}

func (e UserMessageEvent) eventMsgType() EventMsgType { return EventTypeUserMessage }

// ExecApprovalRequestEvent requests command approval.
type ExecApprovalRequestEvent struct {
	Type                       EventMsgType         `json:"type"`
	CallID                     string               `json:"call_id"`
	TurnID                     string               `json:"turn_id"`
	Command                    []string             `json:"command"`
	CWD                        string               `json:"cwd"`
	Reason                     *string              `json:"reason,omitempty"`
	ProposedExecpolicyAmendment *ExecPolicyAmendment `json:"proposed_execpolicy_amendment,omitempty"`
	ParsedCmd                  []ParsedCommand      `json:"parsed_cmd"`
}

func (e ExecApprovalRequestEvent) eventMsgType() EventMsgType { return EventTypeExecApprovalRequest }

// FileChange represents a file modification in a patch.
type FileChange struct {
	Type        string  `json:"type"` // "add", "update", "delete"
	UnifiedDiff string  `json:"unified_diff,omitempty"`
	Content     string  `json:"content,omitempty"`
	MovePath    *string `json:"move_path,omitempty"`
}

// ApplyPatchApprovalRequestEvent requests patch approval.
type ApplyPatchApprovalRequestEvent struct {
	Type      EventMsgType          `json:"type"`
	CallID    string                `json:"call_id"`
	TurnID    string                `json:"turn_id"`
	Changes   map[string]FileChange `json:"changes"`
	Reason    *string               `json:"reason,omitempty"`
	GrantRoot *string               `json:"grant_root,omitempty"`
}

func (e ApplyPatchApprovalRequestEvent) eventMsgType() EventMsgType { return EventTypeApplyPatchApprovalRequest }

// ElicitationRequestEvent contains an MCP elicitation request.
type ElicitationRequestEvent struct {
	Type       EventMsgType `json:"type"`
	ServerName string       `json:"server_name"`
	ID         interface{}  `json:"id"`
	Message    string       `json:"message"`
}

func (e ElicitationRequestEvent) eventMsgType() EventMsgType { return EventTypeElicitationRequest }

// ExecCommandBeginEvent signals command execution start.
type ExecCommandBeginEvent struct {
	Type             EventMsgType    `json:"type"`
	CallID           string          `json:"call_id"`
	ProcessID        *string         `json:"process_id,omitempty"`
	TurnID           string          `json:"turn_id"`
	Command          []string        `json:"command"`
	CWD              string          `json:"cwd"`
	ParsedCmd        []ParsedCommand `json:"parsed_cmd"`
	Source           string          `json:"source"`
	InteractionInput *string         `json:"interaction_input,omitempty"`
}

func (e ExecCommandBeginEvent) eventMsgType() EventMsgType { return EventTypeExecCommandBegin }

// ExecCommandOutputDeltaEvent contains streaming command output.
type ExecCommandOutputDeltaEvent struct {
	Type   EventMsgType `json:"type"`
	CallID string       `json:"call_id"`
	Stdout *string      `json:"stdout,omitempty"` // base64 encoded
	Stderr *string      `json:"stderr,omitempty"` // base64 encoded
}

func (e ExecCommandOutputDeltaEvent) eventMsgType() EventMsgType { return EventTypeExecCommandOutputDelta }

// ExecCommandEndEvent signals command execution end.
type ExecCommandEndEvent struct {
	Type      EventMsgType `json:"type"`
	CallID    string       `json:"call_id"`
	ProcessID *string      `json:"process_id,omitempty"`
	TurnID    string       `json:"turn_id"`
	Command   []string     `json:"command"`
	CWD       string       `json:"cwd"`
	ExitCode  int          `json:"exit_code"`
	TimedOut  bool         `json:"timed_out"`
}

func (e ExecCommandEndEvent) eventMsgType() EventMsgType { return EventTypeExecCommandEnd }

// PatchApplyBeginEvent signals patch application start.
type PatchApplyBeginEvent struct {
	Type    EventMsgType          `json:"type"`
	CallID  string                `json:"call_id"`
	TurnID  string                `json:"turn_id"`
	Changes map[string]FileChange `json:"changes"`
	Reason  *string               `json:"reason,omitempty"`
}

func (e PatchApplyBeginEvent) eventMsgType() EventMsgType { return EventTypePatchApplyBegin }

// PatchApplyEndEvent signals patch application end.
type PatchApplyEndEvent struct {
	Type    EventMsgType          `json:"type"`
	CallID  string                `json:"call_id"`
	TurnID  string                `json:"turn_id"`
	Changes map[string]FileChange `json:"changes"`
	Success bool                  `json:"success"`
	Error   *string               `json:"error,omitempty"`
}

func (e PatchApplyEndEvent) eventMsgType() EventMsgType { return EventTypePatchApplyEnd }

// ErrorEvent contains error information.
type ErrorEvent struct {
	Type          EventMsgType    `json:"type"`
	Message       string          `json:"message"`
	CodexErrorInfo *CodexErrorInfo `json:"codex_error_info,omitempty"`
}

func (e ErrorEvent) eventMsgType() EventMsgType { return EventTypeError }

// CodexErrorInfo contains detailed error information.
type CodexErrorInfo struct {
	Type           string `json:"type"`
	HTTPStatusCode *int   `json:"http_status_code,omitempty"`
}

// WarningEvent contains warning information.
type WarningEvent struct {
	Type    EventMsgType `json:"type"`
	Message string       `json:"message"`
}

func (e WarningEvent) eventMsgType() EventMsgType { return EventTypeWarning }

// TokenUsage tracks token consumption.
type TokenUsage struct {
	InputTokens          int64 `json:"input_tokens"`
	CachedInputTokens    int64 `json:"cached_input_tokens"`
	OutputTokens         int64 `json:"output_tokens"`
	ReasoningOutputTokens int64 `json:"reasoning_output_tokens"`
	TotalTokens          int64 `json:"total_tokens"`
}

// TokenUsageInfo contains complete token usage information.
type TokenUsageInfo struct {
	TotalTokenUsage    TokenUsage `json:"total_token_usage"`
	LastTokenUsage     TokenUsage `json:"last_token_usage"`
	ModelContextWindow *int64     `json:"model_context_window,omitempty"`
}

// RateLimitInfo contains rate limit information.
type RateLimitInfo struct {
	RequestsLimit     int64 `json:"requests_limit"`
	RequestsRemaining int64 `json:"requests_remaining"`
	RequestsReset     int64 `json:"requests_reset"`
	TokensLimit       int64 `json:"tokens_limit"`
	TokensRemaining   int64 `json:"tokens_remaining"`
	TokensReset       int64 `json:"tokens_reset"`
}

// TokenCountEvent reports token usage.
type TokenCountEvent struct {
	Type       EventMsgType    `json:"type"`
	Info       *TokenUsageInfo `json:"info,omitempty"`
	RateLimits *RateLimitInfo  `json:"rate_limits,omitempty"`
}

func (e TokenCountEvent) eventMsgType() EventMsgType { return EventTypeTokenCount }

// BackgroundEvent contains informational messages.
type BackgroundEvent struct {
	Type    EventMsgType `json:"type"`
	Message string       `json:"message"`
}

func (e BackgroundEvent) eventMsgType() EventMsgType { return EventTypeBackgroundEvent }

// StreamErrorEvent contains connection/retry info.
type StreamErrorEvent struct {
	Type       EventMsgType `json:"type"`
	Message    string       `json:"message"`
	RetryInMs  *int64       `json:"retry_in_ms,omitempty"`
}

func (e StreamErrorEvent) eventMsgType() EventMsgType { return EventTypeStreamError }

// ContextCompactedEvent signals history was summarized.
type ContextCompactedEvent struct {
	Type EventMsgType `json:"type"`
}

func (e ContextCompactedEvent) eventMsgType() EventMsgType { return EventTypeContextCompacted }

// ThreadRolledBackEvent signals turns were removed.
type ThreadRolledBackEvent struct {
	Type     EventMsgType `json:"type"`
	NumTurns int          `json:"num_turns"`
	Success  bool         `json:"success"`
	Error    *string      `json:"error,omitempty"`
}

func (e ThreadRolledBackEvent) eventMsgType() EventMsgType { return EventTypeThreadRolledBack }

// McpToolCallBeginEvent signals MCP tool execution start.
type McpToolCallBeginEvent struct {
	Type       EventMsgType  `json:"type"`
	CallID     string        `json:"call_id"`
	Invocation McpInvocation `json:"invocation"`
}

func (e McpToolCallBeginEvent) eventMsgType() EventMsgType { return EventTypeMcpToolCallBegin }

// McpInvocation contains MCP tool call details.
type McpInvocation struct {
	Server    string      `json:"server"`
	Tool      string      `json:"tool"`
	Arguments interface{} `json:"arguments,omitempty"`
}

// McpToolCallEndEvent signals MCP tool execution end.
type McpToolCallEndEvent struct {
	Type       EventMsgType  `json:"type"`
	CallID     string        `json:"call_id"`
	Invocation McpInvocation `json:"invocation"`
	Duration   string        `json:"duration"`
	Result     interface{}   `json:"result"`
}

func (e McpToolCallEndEvent) eventMsgType() EventMsgType { return EventTypeMcpToolCallEnd }

// UndoCompletedEvent signals undo completion.
type UndoCompletedEvent struct {
	Type    EventMsgType `json:"type"`
	Success bool         `json:"success"`
	Error   *string      `json:"error,omitempty"`
}

func (e UndoCompletedEvent) eventMsgType() EventMsgType { return EventTypeUndoCompleted }
