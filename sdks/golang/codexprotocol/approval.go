package codexprotocol

// AskForApproval determines when the user is consulted to approve operations.
// Source: codex-rs/protocol/src/protocol.rs:250-289
type AskForApproval string

const (
	// ApprovalPolicyUntrusted requires approval for all operations except known safe read-only commands.
	ApprovalPolicyUntrusted AskForApproval = "untrusted"

	// ApprovalPolicyOnFailure auto-approves commands but asks if they fail.
	ApprovalPolicyOnFailure AskForApproval = "on-failure"

	// ApprovalPolicyOnRequest asks the user for approval (default).
	ApprovalPolicyOnRequest AskForApproval = "on-request"

	// ApprovalPolicyNever never asks for approval, returns errors immediately.
	ApprovalPolicyNever AskForApproval = "never"
)

// SandboxPolicy determines execution restrictions for model shell commands.
// Source: codex-rs/protocol/src/protocol.rs:310-356
type SandboxPolicy string

const (
	// SandboxPolicyDangerFullAccess provides no restrictions.
	SandboxPolicyDangerFullAccess SandboxPolicy = "danger-full-access"

	// SandboxPolicyReadOnly provides read-only access to the filesystem.
	SandboxPolicyReadOnly SandboxPolicy = "read-only"

	// SandboxPolicyExternalSandbox indicates the process is already in a container.
	SandboxPolicyExternalSandbox SandboxPolicy = "external-sandbox"

	// SandboxPolicyWorkspaceWrite allows reads everywhere and writes to cwd + /tmp.
	SandboxPolicyWorkspaceWrite SandboxPolicy = "workspace-write"
)

// ReviewDecision represents the user's decision on an approval request.
type ReviewDecision string

const (
	// ReviewDecisionApproved allows execution.
	ReviewDecisionApproved ReviewDecision = "approved"

	// ReviewDecisionApprovedForSession allows execution for the session duration.
	ReviewDecisionApprovedForSession ReviewDecision = "approved_for_session"

	// ReviewDecisionApprovedWithAmendment allows execution and updates execpolicy rules.
	ReviewDecisionApprovedWithAmendment ReviewDecision = "approved_with_amendment"

	// ReviewDecisionDenied rejects execution but continues the session.
	ReviewDecisionDenied ReviewDecision = "denied"

	// ReviewDecisionAbort rejects execution and stops the session.
	ReviewDecisionAbort ReviewDecision = "abort"
)

// ElicitationAction represents the user's decision on an MCP elicitation request.
type ElicitationAction string

const (
	ElicitationActionAccept  ElicitationAction = "accept"
	ElicitationActionDecline ElicitationAction = "decline"
	ElicitationActionCancel  ElicitationAction = "cancel"
)

// NetworkAccess represents whether outbound network access is available.
type NetworkAccess string

const (
	NetworkAccessRestricted NetworkAccess = "restricted"
	NetworkAccessEnabled    NetworkAccess = "enabled"
)

// SandboxPolicyConfig contains the full sandbox policy configuration.
// This is used for workspace-write and external-sandbox policies.
type SandboxPolicyConfig struct {
	Type SandboxPolicy `json:"type"`

	// For workspace-write policy
	WritableRoots       []string `json:"writable_roots,omitempty"`
	NetworkAccess       bool     `json:"network_access,omitempty"`
	ExcludeTmpdirEnvVar bool     `json:"exclude_tmpdir_env_var,omitempty"`
	ExcludeSlashTmp     bool     `json:"exclude_slash_tmp,omitempty"`

	// For external-sandbox policy
	NetworkAccessMode NetworkAccess `json:"network_access_mode,omitempty"`
}

// ExecPolicyAmendment is a proposed execpolicy change to allow commands starting with a prefix.
type ExecPolicyAmendment struct {
	Command []string `json:"command"`
}

// ParsedCommand represents a parsed command structure.
type ParsedCommand struct {
	Program string   `json:"program"`
	Args    []string `json:"args,omitempty"`
}
