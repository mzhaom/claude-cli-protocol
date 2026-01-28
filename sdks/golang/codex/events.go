package codex

import (
	"time"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/codexprotocol"
)

// EventType discriminates between event kinds.
type EventType int

const (
	// EventTypeReady fires when the session is initialized.
	EventTypeReady EventType = iota
	// EventTypeText fires for streaming text chunks.
	EventTypeText
	// EventTypeReasoning fires for reasoning chunks.
	EventTypeReasoning
	// EventTypeExecApprovalRequest fires when command approval is needed.
	EventTypeExecApprovalRequest
	// EventTypePatchApprovalRequest fires when patch approval is needed.
	EventTypePatchApprovalRequest
	// EventTypeCommandStart fires when a command begins execution.
	EventTypeCommandStart
	// EventTypeCommandOutput fires for command output chunks.
	EventTypeCommandOutput
	// EventTypeCommandEnd fires when a command finishes.
	EventTypeCommandEnd
	// EventTypePatchStart fires when a patch begins application.
	EventTypePatchStart
	// EventTypePatchEnd fires when a patch finishes application.
	EventTypePatchEnd
	// EventTypeTurnComplete fires when a turn finishes.
	EventTypeTurnComplete
	// EventTypeError fires on session errors.
	EventTypeError
	// EventTypeWarning fires on non-fatal warnings.
	EventTypeWarning
	// EventTypeStateChange fires on session state transitions.
	EventTypeStateChange
	// EventTypeMCPToolStart fires when an MCP tool begins.
	EventTypeMCPToolStart
	// EventTypeMCPToolEnd fires when an MCP tool finishes.
	EventTypeMCPToolEnd
	// EventTypeTokenCount fires with token usage updates.
	EventTypeTokenCount
)

// Event is the interface for all events.
type Event interface {
	Type() EventType
}

// ReadyEvent fires when the session is initialized.
type ReadyEvent struct {
	Info SessionInfo
}

// Type returns the event type.
func (e ReadyEvent) Type() EventType { return EventTypeReady }

// TextEvent contains streaming text chunks.
type TextEvent struct {
	TurnNumber int
	Text       string // New text chunk
	FullText   string // Accumulated text
}

// Type returns the event type.
func (e TextEvent) Type() EventType { return EventTypeText }

// ReasoningEvent contains reasoning chunks.
type ReasoningEvent struct {
	TurnNumber    int
	Reasoning     string // New reasoning chunk
	FullReasoning string // Accumulated reasoning
}

// Type returns the event type.
func (e ReasoningEvent) Type() EventType { return EventTypeReasoning }

// ExecApprovalRequestEvent fires when command approval is needed.
type ExecApprovalRequestEvent struct {
	TurnNumber                   int
	CallID                       string
	TurnID                       string
	Command                      []string
	CWD                          string
	Reason                       string
	ProposedExecpolicyAmendment  *codexprotocol.ExecPolicyAmendment
	ParsedCommand                []codexprotocol.ParsedCommand
}

// Type returns the event type.
func (e ExecApprovalRequestEvent) Type() EventType { return EventTypeExecApprovalRequest }

// PatchApprovalRequestEvent fires when patch approval is needed.
type PatchApprovalRequestEvent struct {
	TurnNumber int
	CallID     string
	TurnID     string
	Changes    map[string]codexprotocol.FileChange
	Reason     string
	GrantRoot  *string
}

// Type returns the event type.
func (e PatchApprovalRequestEvent) Type() EventType { return EventTypePatchApprovalRequest }

// CommandStartEvent fires when a command begins execution.
type CommandStartEvent struct {
	TurnNumber int
	CallID     string
	TurnID     string
	Command    []string
	CWD        string
	ProcessID  *string
	Timestamp  time.Time
}

// Type returns the event type.
func (e CommandStartEvent) Type() EventType { return EventTypeCommandStart }

// CommandOutputEvent contains command output chunks.
// Stdout and Stderr are base64-decoded from the wire format.
type CommandOutputEvent struct {
	TurnNumber int
	CallID     string
	Stdout     string // base64 encoded in wire format, decoded here
	Stderr     string // base64 encoded in wire format, decoded here
}

// Type returns the event type.
func (e CommandOutputEvent) Type() EventType { return EventTypeCommandOutput }

// CommandEndEvent fires when a command finishes.
type CommandEndEvent struct {
	TurnNumber int
	CallID     string
	TurnID     string
	Command    []string
	CWD        string
	ProcessID  *string
	ExitCode   int
	TimedOut   bool
	Timestamp  time.Time
}

// Type returns the event type.
func (e CommandEndEvent) Type() EventType { return EventTypeCommandEnd }

// PatchStartEvent fires when a patch begins application.
type PatchStartEvent struct {
	TurnNumber int
	CallID     string
	TurnID     string
	Changes    map[string]codexprotocol.FileChange
	Reason     string
}

// Type returns the event type.
func (e PatchStartEvent) Type() EventType { return EventTypePatchStart }

// PatchEndEvent fires when a patch finishes application.
type PatchEndEvent struct {
	TurnNumber int
	CallID     string
	TurnID     string
	Changes    map[string]codexprotocol.FileChange
	Success    bool
	Error      string
}

// Type returns the event type.
func (e PatchEndEvent) Type() EventType { return EventTypePatchEnd }

// MCPToolStartEvent fires when an MCP tool begins.
type MCPToolStartEvent struct {
	TurnNumber int
	CallID     string
	ServerName string
	ToolName   string
	Arguments  map[string]interface{}
	Timestamp  time.Time
}

// Type returns the event type.
func (e MCPToolStartEvent) Type() EventType { return EventTypeMCPToolStart }

// MCPToolEndEvent fires when an MCP tool finishes.
type MCPToolEndEvent struct {
	TurnNumber int
	CallID     string
	ServerName string
	ToolName   string
	Duration   float64
	Result     interface{}
	Timestamp  time.Time
}

// Type returns the event type.
func (e MCPToolEndEvent) Type() EventType { return EventTypeMCPToolEnd }

// TurnCompleteEvent fires when a turn finishes.
type TurnCompleteEvent struct {
	TurnNumber       int
	Success          bool
	DurationMs       int64
	Usage            TurnUsage
	Error            error
	LastAgentMessage string
}

// Type returns the event type.
func (e TurnCompleteEvent) Type() EventType { return EventTypeTurnComplete }

// ErrorEvent contains session errors.
type ErrorEvent struct {
	TurnNumber int
	Error      error
	Context    string
	ErrorInfo  *codexprotocol.CodexErrorInfo
}

// Type returns the event type.
func (e ErrorEvent) Type() EventType { return EventTypeError }

// WarningEvent contains non-fatal warnings.
type WarningEvent struct {
	TurnNumber int
	Message    string
}

// Type returns the event type.
func (e WarningEvent) Type() EventType { return EventTypeWarning }

// StateChangeEvent fires on session state transitions.
type StateChangeEvent struct {
	From SessionState
	To   SessionState
}

// Type returns the event type.
func (e StateChangeEvent) Type() EventType { return EventTypeStateChange }

// TokenCountEvent fires with token usage updates.
type TokenCountEvent struct {
	TurnNumber int
	Info       *codexprotocol.TokenUsageInfo
	RateLimits interface{}
}

// Type returns the event type.
func (e TokenCountEvent) Type() EventType { return EventTypeTokenCount }
