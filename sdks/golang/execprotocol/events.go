package execprotocol

import "encoding/json"

// EventType represents the type of a thread event.
type EventType string

const (
	EventTypeThreadStarted  EventType = "thread.started"
	EventTypeTurnStarted    EventType = "turn.started"
	EventTypeTurnCompleted  EventType = "turn.completed"
	EventTypeTurnFailed     EventType = "turn.failed"
	EventTypeItemStarted    EventType = "item.started"
	EventTypeItemUpdated    EventType = "item.updated"
	EventTypeItemCompleted  EventType = "item.completed"
	EventTypeError          EventType = "error"
)

// ThreadEvent is the top-level event type emitted by codex exec --json.
type ThreadEvent struct {
	Type EventType `json:"type"`

	// For thread.started events
	ThreadID string `json:"thread_id,omitempty"`

	// For turn.completed events
	Usage *Usage `json:"usage,omitempty"`

	// For turn.failed events
	Error *ErrorInfo `json:"error,omitempty"`

	// For item.* events
	Item *ThreadItem `json:"item,omitempty"`

	// For error events
	Message string `json:"message,omitempty"`
}

// Usage describes token usage during a turn.
type Usage struct {
	InputTokens       int64 `json:"input_tokens"`
	CachedInputTokens int64 `json:"cached_input_tokens"`
	OutputTokens      int64 `json:"output_tokens"`
}

// ErrorInfo contains error details.
type ErrorInfo struct {
	Message string `json:"message"`
}

// ThreadItem represents an item in a thread.
type ThreadItem struct {
	ID   string         `json:"id"`
	Type ItemType       `json:"type"`
	Raw  json.RawMessage `json:"-"` // Raw JSON for custom parsing
}

// ItemType represents the type of a thread item.
type ItemType string

const (
	ItemTypeAgentMessage     ItemType = "agent_message"
	ItemTypeReasoning        ItemType = "reasoning"
	ItemTypeCommandExecution ItemType = "command_execution"
	ItemTypeFileChange       ItemType = "file_change"
	ItemTypeMcpToolCall      ItemType = "mcp_tool_call"
	ItemTypeWebSearch        ItemType = "web_search"
	ItemTypeTodoList         ItemType = "todo_list"
	ItemTypeError            ItemType = "error"
)

// AgentMessageItem represents an agent message.
type AgentMessageItem struct {
	ID   string `json:"id"`
	Type ItemType `json:"type"`
	Text string `json:"text"`
}

// ReasoningItem represents agent reasoning.
type ReasoningItem struct {
	ID   string `json:"id"`
	Type ItemType `json:"type"`
	Text string `json:"text"`
}

// CommandExecutionStatus represents the status of command execution.
type CommandExecutionStatus string

const (
	CommandStatusInProgress CommandExecutionStatus = "in_progress"
	CommandStatusCompleted  CommandExecutionStatus = "completed"
	CommandStatusFailed     CommandExecutionStatus = "failed"
	CommandStatusDeclined   CommandExecutionStatus = "declined"
)

// CommandExecutionItem represents a command execution.
type CommandExecutionItem struct {
	ID               string                 `json:"id"`
	Type             ItemType               `json:"type"`
	Command          string                 `json:"command"`
	AggregatedOutput string                 `json:"aggregated_output"`
	ExitCode         *int                   `json:"exit_code,omitempty"`
	Status           CommandExecutionStatus `json:"status"`
}

// PatchChangeKind represents the type of file change.
type PatchChangeKind string

const (
	PatchChangeAdd    PatchChangeKind = "add"
	PatchChangeDelete PatchChangeKind = "delete"
	PatchChangeUpdate PatchChangeKind = "update"
)

// PatchApplyStatus represents the status of patch application.
type PatchApplyStatus string

const (
	PatchStatusInProgress PatchApplyStatus = "in_progress"
	PatchStatusCompleted  PatchApplyStatus = "completed"
	PatchStatusFailed     PatchApplyStatus = "failed"
)

// FileUpdateChange represents a single file change.
type FileUpdateChange struct {
	Path string          `json:"path"`
	Kind PatchChangeKind `json:"kind"`
}

// FileChangeItem represents a set of file changes.
type FileChangeItem struct {
	ID      string             `json:"id"`
	Type    ItemType           `json:"type"`
	Changes []FileUpdateChange `json:"changes"`
	Status  PatchApplyStatus   `json:"status"`
}

// McpToolCallStatus represents the status of an MCP tool call.
type McpToolCallStatus string

const (
	McpToolStatusInProgress McpToolCallStatus = "in_progress"
	McpToolStatusCompleted  McpToolCallStatus = "completed"
	McpToolStatusFailed     McpToolCallStatus = "failed"
)

// McpToolCallResult contains the result of an MCP tool call.
type McpToolCallResult struct {
	Content           []interface{}          `json:"content"`
	StructuredContent map[string]interface{} `json:"structured_content,omitempty"`
}

// McpToolCallError contains error details from an MCP tool call.
type McpToolCallError struct {
	Message string `json:"message"`
}

// McpToolCallItem represents an MCP tool call.
type McpToolCallItem struct {
	ID        string                 `json:"id"`
	Type      ItemType               `json:"type"`
	Server    string                 `json:"server"`
	Tool      string                 `json:"tool"`
	Arguments map[string]interface{} `json:"arguments,omitempty"`
	Result    *McpToolCallResult     `json:"result,omitempty"`
	Error     *McpToolCallError      `json:"error,omitempty"`
	Status    McpToolCallStatus      `json:"status"`
}

// WebSearchItem represents a web search.
type WebSearchItem struct {
	ID    string   `json:"id"`
	Type  ItemType `json:"type"`
	Query string   `json:"query"`
}

// ErrorItem represents an error notification.
type ErrorItem struct {
	ID      string   `json:"id"`
	Type    ItemType `json:"type"`
	Message string   `json:"message"`
}

// TodoItem represents a single to-do item.
type TodoItem struct {
	Text      string `json:"text"`
	Completed bool   `json:"completed"`
}

// TodoListItem represents the agent's to-do list.
type TodoListItem struct {
	ID    string     `json:"id"`
	Type  ItemType   `json:"type"`
	Items []TodoItem `json:"items"`
}
