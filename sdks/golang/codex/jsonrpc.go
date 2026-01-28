package codex

import (
	"encoding/json"
	"sync/atomic"
)

// JSON-RPC types for app-server communication

// JSONRPCRequest represents a JSON-RPC request.
type JSONRPCRequest struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      int64           `json:"id"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params"`
}

// JSONRPCResponse represents a JSON-RPC response.
type JSONRPCResponse struct {
	JSONRPC string          `json:"jsonrpc,omitempty"`
	ID      int64           `json:"id,omitempty"`
	Result  json.RawMessage `json:"result,omitempty"`
	Error   *JSONRPCError   `json:"error,omitempty"`
}

// JSONRPCNotification represents a JSON-RPC notification (no id).
type JSONRPCNotification struct {
	JSONRPC string          `json:"jsonrpc,omitempty"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params"`
}

// JSONRPCError represents a JSON-RPC error.
type JSONRPCError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

// idGenerator generates unique request IDs.
type idGenerator struct {
	next atomic.Int64
}

func (g *idGenerator) Next() int64 {
	return g.next.Add(1)
}

// newRequest creates a new JSON-RPC request.
func newRequest(id int64, method string, params interface{}) (*JSONRPCRequest, error) {
	paramsData, err := json.Marshal(params)
	if err != nil {
		return nil, err
	}
	return &JSONRPCRequest{
		JSONRPC: "2.0",
		ID:      id,
		Method:  method,
		Params:  paramsData,
	}, nil
}

// App-server request params

// InitializeParams for the initialize request.
type InitializeParams struct {
	ClientInfo ClientInfo `json:"clientInfo"`
}

// ClientInfo identifies the client.
type ClientInfo struct {
	Name    string `json:"name"`
	Title   string `json:"title,omitempty"`
	Version string `json:"version"`
}

// ThreadStartParams for starting a new thread.
type ThreadStartParams struct {
	Model          string                 `json:"model,omitempty"`
	ModelProvider  string                 `json:"modelProvider,omitempty"`
	Profile        string                 `json:"profile,omitempty"`
	CWD            string                 `json:"cwd,omitempty"`
	ApprovalPolicy string                 `json:"approvalPolicy,omitempty"`
	Sandbox        *SandboxConfig         `json:"sandbox,omitempty"`
	Config         map[string]interface{} `json:"config,omitempty"`
}

// SandboxConfig for sandbox settings.
type SandboxConfig struct {
	Type           string   `json:"type,omitempty"`
	WritableRoots  []string `json:"writableRoots,omitempty"`
	NetworkAccess  bool     `json:"networkAccess,omitempty"`
}

// TurnStartParams for starting a turn.
type TurnStartParams struct {
	ThreadID       string      `json:"threadId"`
	Input          []UserInput `json:"input"`
	CWD            string      `json:"cwd,omitempty"`
	ApprovalPolicy string      `json:"approvalPolicy,omitempty"`
	SandboxPolicy  interface{} `json:"sandboxPolicy,omitempty"`
	Model          string      `json:"model,omitempty"`
	Effort         string      `json:"effort,omitempty"`
	Summary        string      `json:"summary,omitempty"`
	OutputSchema   interface{} `json:"outputSchema,omitempty"`
}

// UserInput represents user input (text, image, etc.)
type UserInput struct {
	Type string `json:"type"` // "text", "image", etc.
	Text string `json:"text,omitempty"`
}

// TurnInterruptParams for interrupting a turn.
type TurnInterruptParams struct {
	ThreadID string `json:"threadId"`
}

// App-server response types

// InitializeResponse from initialize request.
type InitializeResponse struct {
	UserAgent string `json:"userAgent"`
}

// ThreadStartResponse from thread/start request.
type ThreadStartResponse struct {
	Thread         ThreadInfo `json:"thread"`
	Model          string     `json:"model"`
	ModelProvider  string     `json:"modelProvider"`
	CWD            string     `json:"cwd"`
	ApprovalPolicy string     `json:"approvalPolicy"`
	Sandbox        struct {
		Type          string   `json:"type"`
		WritableRoots []string `json:"writableRoots"`
		NetworkAccess bool     `json:"networkAccess"`
	} `json:"sandbox"`
	ReasoningEffort string `json:"reasoningEffort,omitempty"`
}

// ThreadInfo contains thread metadata.
type ThreadInfo struct {
	ID            string   `json:"id"`
	Preview       string   `json:"preview"`
	ModelProvider string   `json:"modelProvider"`
	CreatedAt     int64    `json:"createdAt"`
	UpdatedAt     int64    `json:"updatedAt"`
	Path          string   `json:"path"`
	CWD           string   `json:"cwd"`
	CLIVersion    string   `json:"cliVersion"`
	Source        string   `json:"source"`
	GitInfo       *GitInfo `json:"gitInfo,omitempty"`
	Turns         []Turn   `json:"turns"`
}

// GitInfo contains git repository information.
type GitInfo struct {
	SHA       string `json:"sha"`
	Branch    string `json:"branch"`
	OriginURL string `json:"originUrl"`
}

// Turn represents a turn in a thread.
type Turn struct {
	ID     string      `json:"id"`
	Items  []TurnItem  `json:"items"`
	Status string      `json:"status"`
	Error  interface{} `json:"error"`
}

// TurnItem represents an item in a turn.
type TurnItem struct {
	Type string `json:"type"`
	ID   string `json:"id"`
	Text string `json:"text,omitempty"`
}

// TurnStartResponse from turn/start request.
type TurnStartResponse struct {
	Turn Turn `json:"turn"`
}

// App-server notification types (methods)
const (
	NotifyThreadStarted             = "thread/started"
	NotifyTurnStarted               = "turn/started"
	NotifyTurnCompleted             = "turn/completed"
	NotifyItemStarted               = "item/started"
	NotifyItemCompleted             = "item/completed"
	NotifyAgentMessageDelta         = "item/agentMessage/delta"
	NotifyCodexEventTaskStarted     = "codex/event/task_started"
	NotifyCodexEventTaskComplete    = "codex/event/task_complete"
	NotifyCodexEventAgentMessage    = "codex/event/agent_message"
	NotifyCodexEventAgentDelta      = "codex/event/agent_message_delta"
	NotifyCodexEventError           = "codex/event/error"
	NotifyCodexEventTokenCount      = "codex/event/token_count"
	NotifyCodexEventMCPStartup      = "codex/event/mcp_startup_complete"
)

// Notification params

// ThreadStartedNotification params.
type ThreadStartedNotification struct {
	Thread ThreadInfo `json:"thread"`
}

// TurnStartedNotification params.
type TurnStartedNotification struct {
	ThreadID string `json:"threadId"`
	Turn     Turn   `json:"turn"`
}

// TurnCompletedNotification params.
type TurnCompletedNotification struct {
	ThreadID string `json:"threadId"`
	Turn     Turn   `json:"turn"`
}

// ItemNotification params for item/started, item/completed.
type ItemNotification struct {
	ThreadID string          `json:"threadId"`
	TurnID   string          `json:"turnId"`
	Item     json.RawMessage `json:"item"`
}

// AgentMessageDeltaNotification params.
type AgentMessageDeltaNotification struct {
	ThreadID string `json:"threadId"`
	TurnID   string `json:"turnId"`
	ItemID   string `json:"itemId"`
	Delta    string `json:"delta"`
}

// CodexEventNotification wraps codex/event/* notifications.
type CodexEventNotification struct {
	ID             string          `json:"id"`
	Msg            json.RawMessage `json:"msg"`
	ConversationID string          `json:"conversationId"`
}

// TaskStartedMsg from codex/event/task_started.
type TaskStartedMsg struct {
	Type               string `json:"type"`
	ModelContextWindow int    `json:"model_context_window"`
}

// TaskCompleteMsg from codex/event/task_complete.
type TaskCompleteMsg struct {
	Type             string `json:"type"`
	LastAgentMessage string `json:"last_agent_message"`
}

// AgentMessageMsg from codex/event/agent_message.
type AgentMessageMsg struct {
	Type    string `json:"type"`
	Message string `json:"message"`
}

// AgentMessageDeltaMsg from codex/event/agent_message_delta.
type AgentMessageDeltaMsg struct {
	Type  string `json:"type"`
	Delta string `json:"delta"`
}

// TokenCountMsg from codex/event/token_count.
type TokenCountMsg struct {
	Type       string          `json:"type"`
	Info       *TokenUsageInfo `json:"info"`
	RateLimits *RateLimits     `json:"rate_limits"`
}

// TokenUsageInfo contains token usage details.
type TokenUsageInfo struct {
	TotalTokenUsage *TokenUsage `json:"total_token_usage"`
	LastTokenUsage  *TokenUsage `json:"last_token_usage"`
	ModelContextWindow int      `json:"model_context_window"`
}

// TokenUsage contains token counts.
type TokenUsage struct {
	InputTokens          int64 `json:"input_tokens"`
	CachedInputTokens    int64 `json:"cached_input_tokens"`
	OutputTokens         int64 `json:"output_tokens"`
	ReasoningOutputTokens int64 `json:"reasoning_output_tokens"`
	TotalTokens          int64 `json:"total_tokens"`
}

// RateLimits contains rate limit information.
type RateLimits struct {
	Primary   *RateLimitInfo `json:"primary"`
	Secondary *RateLimitInfo `json:"secondary"`
	Credits   *CreditsInfo   `json:"credits"`
	PlanType  *string        `json:"plan_type"`
}

// RateLimitInfo contains rate limit details.
type RateLimitInfo struct {
	UsedPercent   float64 `json:"used_percent"`
	WindowMinutes int     `json:"window_minutes"`
	ResetsAt      int64   `json:"resets_at"`
}

// CreditsInfo contains credits information.
type CreditsInfo struct {
	HasCredits bool     `json:"has_credits"`
	Unlimited  bool     `json:"unlimited"`
	Balance    *float64 `json:"balance"`
}
