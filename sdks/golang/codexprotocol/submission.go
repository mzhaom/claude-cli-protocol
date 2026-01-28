package codexprotocol

import "encoding/json"

// Submission is a request from the client to Codex.
// Source: codex-rs/protocol/src/protocol.rs:56-62
type Submission struct {
	// ID is a unique submission ID for correlation with events.
	ID string `json:"id"`
	// Op is the operation payload (set Type field before marshaling).
	Op Op `json:"-"`
}

// MarshalJSON implements custom JSON marshaling for Submission.
func (s Submission) MarshalJSON() ([]byte, error) {
	opData, err := json.Marshal(s.Op)
	if err != nil {
		return nil, err
	}
	type alias struct {
		ID string          `json:"id"`
		Op json.RawMessage `json:"op"`
	}
	return json.Marshal(alias{
		ID: s.ID,
		Op: opData,
	})
}

// OpType discriminates between operation kinds.
type OpType string

const (
	OpTypeInterrupt            OpType = "interrupt"
	OpTypeUserInput            OpType = "user_input"
	OpTypeUserTurn             OpType = "user_turn"
	OpTypeOverrideTurnContext  OpType = "override_turn_context"
	OpTypeExecApproval         OpType = "exec_approval"
	OpTypePatchApproval        OpType = "patch_approval"
	OpTypeResolveElicitation   OpType = "resolve_elicitation"
	OpTypeAddToHistory         OpType = "add_to_history"
	OpTypeGetHistoryEntry      OpType = "get_history_entry_request"
	OpTypeListMcpTools         OpType = "list_mcp_tools"
	OpTypeRefreshMcpServers    OpType = "refresh_mcp_servers"
	OpTypeListCustomPrompts    OpType = "list_custom_prompts"
	OpTypeListSkills           OpType = "list_skills"
	OpTypeCompact              OpType = "compact"
	OpTypeUndo                 OpType = "undo"
	OpTypeThreadRollback       OpType = "thread_rollback"
	OpTypeReview               OpType = "review"
	OpTypeShutdown             OpType = "shutdown"
	OpTypeRunUserShellCommand  OpType = "run_user_shell_command"
	OpTypeListModels           OpType = "list_models"
)

// Op is the interface for all operations sent to Codex.
type Op interface {
	opType() OpType
}

// InterruptOp aborts the current task.
type InterruptOp struct {
	Type OpType `json:"type"`
}

func (o *InterruptOp) opType() OpType { return OpTypeInterrupt }

// MarshalJSON ensures Type is set.
func (o InterruptOp) MarshalJSON() ([]byte, error) {
	type alias InterruptOp
	o.Type = OpTypeInterrupt
	return json.Marshal(alias(o))
}

// UserInputOp sends basic user input.
type UserInputOp struct {
	Type                  OpType      `json:"type"`
	Items                 []UserInput `json:"items"`
	FinalOutputJSONSchema interface{} `json:"final_output_json_schema,omitempty"`
}

func (o *UserInputOp) opType() OpType { return OpTypeUserInput }

// MarshalJSON ensures Type is set and Items are properly marshaled.
func (o UserInputOp) MarshalJSON() ([]byte, error) {
	type alias struct {
		Type                  OpType        `json:"type"`
		Items                 []interface{} `json:"items"`
		FinalOutputJSONSchema interface{}   `json:"final_output_json_schema,omitempty"`
	}
	items := make([]interface{}, len(o.Items))
	for i, item := range o.Items {
		items[i] = item
	}
	return json.Marshal(alias{
		Type:                  OpTypeUserInput,
		Items:                 items,
		FinalOutputJSONSchema: o.FinalOutputJSONSchema,
	})
}

// UserTurnOp sends a full turn with configuration.
type UserTurnOp struct {
	Type                  OpType         `json:"type"`
	Items                 []UserInput    `json:"items"`
	CWD                   string         `json:"cwd,omitempty"`
	ApprovalPolicy        AskForApproval `json:"approval_policy,omitempty"`
	SandboxPolicy         SandboxPolicy  `json:"sandbox_policy,omitempty"`
	Model                 string         `json:"model,omitempty"`
	Effort                string         `json:"effort,omitempty"`
	Summary               string         `json:"summary,omitempty"`
	FinalOutputJSONSchema interface{}    `json:"final_output_json_schema,omitempty"`
}

func (o *UserTurnOp) opType() OpType { return OpTypeUserTurn }

// MarshalJSON ensures Type is set and Items are properly marshaled.
func (o UserTurnOp) MarshalJSON() ([]byte, error) {
	type alias struct {
		Type                  OpType         `json:"type"`
		Items                 []interface{}  `json:"items"`
		CWD                   string         `json:"cwd,omitempty"`
		ApprovalPolicy        AskForApproval `json:"approval_policy,omitempty"`
		SandboxPolicy         SandboxPolicy  `json:"sandbox_policy,omitempty"`
		Model                 string         `json:"model,omitempty"`
		Effort                string         `json:"effort,omitempty"`
		Summary               string         `json:"summary,omitempty"`
		FinalOutputJSONSchema interface{}    `json:"final_output_json_schema,omitempty"`
	}
	items := make([]interface{}, len(o.Items))
	for i, item := range o.Items {
		items[i] = item
	}
	return json.Marshal(alias{
		Type:                  OpTypeUserTurn,
		Items:                 items,
		CWD:                   o.CWD,
		ApprovalPolicy:        o.ApprovalPolicy,
		SandboxPolicy:         o.SandboxPolicy,
		Model:                 o.Model,
		Effort:                o.Effort,
		Summary:               o.Summary,
		FinalOutputJSONSchema: o.FinalOutputJSONSchema,
	})
}

// OverrideTurnContextOp updates session defaults.
type OverrideTurnContextOp struct {
	Type           OpType          `json:"type"`
	CWD            *string         `json:"cwd,omitempty"`
	ApprovalPolicy *AskForApproval `json:"approval_policy,omitempty"`
	SandboxPolicy  *SandboxPolicy  `json:"sandbox_policy,omitempty"`
	Model          *string         `json:"model,omitempty"`
	Effort         *string         `json:"effort,omitempty"`
	Summary        *string         `json:"summary,omitempty"`
}

func (o OverrideTurnContextOp) opType() OpType { return OpTypeOverrideTurnContext }

// ExecApprovalOp approves/denies command execution.
type ExecApprovalOp struct {
	Type     OpType         `json:"type"`
	ID       string         `json:"id"`
	Decision ReviewDecision `json:"decision"`
}

func (o *ExecApprovalOp) opType() OpType { return OpTypeExecApproval }

// MarshalJSON ensures Type is set.
func (o ExecApprovalOp) MarshalJSON() ([]byte, error) {
	type alias ExecApprovalOp
	o.Type = OpTypeExecApproval
	return json.Marshal(alias(o))
}

// PatchApprovalOp approves/denies patch application.
type PatchApprovalOp struct {
	Type     OpType         `json:"type"`
	ID       string         `json:"id"`
	Decision ReviewDecision `json:"decision"`
}

func (o *PatchApprovalOp) opType() OpType { return OpTypePatchApproval }

// MarshalJSON ensures Type is set.
func (o PatchApprovalOp) MarshalJSON() ([]byte, error) {
	type alias PatchApprovalOp
	o.Type = OpTypePatchApproval
	return json.Marshal(alias(o))
}

// ResolveElicitationOp resolves an MCP elicitation request.
type ResolveElicitationOp struct {
	Type       OpType            `json:"type"`
	ServerName string            `json:"server_name"`
	RequestID  interface{}       `json:"request_id"`
	Decision   ElicitationAction `json:"decision"`
}

func (o ResolveElicitationOp) opType() OpType { return OpTypeResolveElicitation }

// AddToHistoryOp adds an entry to the persistent message history.
type AddToHistoryOp struct {
	Type OpType `json:"type"`
	Text string `json:"text"`
}

func (o AddToHistoryOp) opType() OpType { return OpTypeAddToHistory }

// GetHistoryEntryOp requests a single history entry.
type GetHistoryEntryOp struct {
	Type   OpType `json:"type"`
	Offset int    `json:"offset"`
	LogID  uint64 `json:"log_id"`
}

func (o GetHistoryEntryOp) opType() OpType { return OpTypeGetHistoryEntry }

// ListMcpToolsOp requests the list of MCP tools.
type ListMcpToolsOp struct {
	Type OpType `json:"type"`
}

func (o ListMcpToolsOp) opType() OpType { return OpTypeListMcpTools }

// RefreshMcpServersOp requests MCP servers to reinitialize.
type RefreshMcpServersOp struct {
	Type   OpType      `json:"type"`
	Config interface{} `json:"config"`
}

func (o RefreshMcpServersOp) opType() OpType { return OpTypeRefreshMcpServers }

// ListCustomPromptsOp requests the list of custom prompts.
type ListCustomPromptsOp struct {
	Type OpType `json:"type"`
}

func (o ListCustomPromptsOp) opType() OpType { return OpTypeListCustomPrompts }

// ListSkillsOp requests the list of skills.
type ListSkillsOp struct {
	Type        OpType   `json:"type"`
	CWDs        []string `json:"cwds,omitempty"`
	ForceReload bool     `json:"force_reload,omitempty"`
}

func (o ListSkillsOp) opType() OpType { return OpTypeListSkills }

// CompactOp requests the agent to summarize the conversation.
type CompactOp struct {
	Type OpType `json:"type"`
}

func (o *CompactOp) opType() OpType { return OpTypeCompact }

// MarshalJSON ensures Type is set.
func (o CompactOp) MarshalJSON() ([]byte, error) {
	type alias CompactOp
	o.Type = OpTypeCompact
	return json.Marshal(alias(o))
}

// UndoOp undoes the last turn.
type UndoOp struct {
	Type OpType `json:"type"`
}

func (o *UndoOp) opType() OpType { return OpTypeUndo }

// MarshalJSON ensures Type is set.
func (o UndoOp) MarshalJSON() ([]byte, error) {
	type alias UndoOp
	o.Type = OpTypeUndo
	return json.Marshal(alias(o))
}

// ThreadRollbackOp drops the last N user turns.
type ThreadRollbackOp struct {
	Type     OpType `json:"type"`
	NumTurns uint32 `json:"num_turns"`
}

func (o ThreadRollbackOp) opType() OpType { return OpTypeThreadRollback }

// ReviewOp requests a code review.
type ReviewOp struct {
	Type          OpType        `json:"type"`
	ReviewRequest ReviewRequest `json:"review_request"`
}

func (o ReviewOp) opType() OpType { return OpTypeReview }

// ShutdownOp terminates the session.
type ShutdownOp struct {
	Type OpType `json:"type"`
}

func (o *ShutdownOp) opType() OpType { return OpTypeShutdown }

// MarshalJSON ensures Type is set.
func (o ShutdownOp) MarshalJSON() ([]byte, error) {
	type alias ShutdownOp
	o.Type = OpTypeShutdown
	return json.Marshal(alias(o))
}

// RunUserShellCommandOp executes a user-initiated shell command.
type RunUserShellCommandOp struct {
	Type    OpType `json:"type"`
	Command string `json:"command"`
}

func (o RunUserShellCommandOp) opType() OpType { return OpTypeRunUserShellCommand }

// ListModelsOp requests the list of available models.
type ListModelsOp struct {
	Type OpType `json:"type"`
}

func (o ListModelsOp) opType() OpType { return OpTypeListModels }

// ReviewRequest contains parameters for a code review request.
type ReviewRequest struct {
	Target         ReviewTarget `json:"target"`
	UserFacingHint *string      `json:"user_facing_hint,omitempty"`
}

// ReviewTarget specifies what to review.
type ReviewTarget struct {
	Type         string  `json:"type"`
	Branch       string  `json:"branch,omitempty"`
	SHA          string  `json:"sha,omitempty"`
	Title        *string `json:"title,omitempty"`
	Instructions string  `json:"instructions,omitempty"`
}

// NewInterruptOp creates an interrupt operation.
func NewInterruptOp() InterruptOp {
	return InterruptOp{Type: OpTypeInterrupt}
}

// NewUserInputOp creates a user input operation with text.
func NewUserInputOp(text string) UserInputOp {
	return UserInputOp{
		Type:  OpTypeUserInput,
		Items: []UserInput{NewTextInput(text)},
	}
}

// NewUserInputOpWithItems creates a user input operation with custom items.
func NewUserInputOpWithItems(items []UserInput) UserInputOp {
	return UserInputOp{
		Type:  OpTypeUserInput,
		Items: items,
	}
}

// NewShutdownOp creates a shutdown operation.
func NewShutdownOp() ShutdownOp {
	return ShutdownOp{Type: OpTypeShutdown}
}

// NewCompactOp creates a compact operation.
func NewCompactOp() CompactOp {
	return CompactOp{Type: OpTypeCompact}
}

// NewUndoOp creates an undo operation.
func NewUndoOp() UndoOp {
	return UndoOp{Type: OpTypeUndo}
}

// NewExecApprovalOp creates an exec approval operation.
func NewExecApprovalOp(callID string, decision ReviewDecision) ExecApprovalOp {
	return ExecApprovalOp{
		Type:     OpTypeExecApproval,
		ID:       callID,
		Decision: decision,
	}
}

// NewPatchApprovalOp creates a patch approval operation.
func NewPatchApprovalOp(callID string, decision ReviewDecision) PatchApprovalOp {
	return PatchApprovalOp{
		Type:     OpTypePatchApproval,
		ID:       callID,
		Decision: decision,
	}
}
