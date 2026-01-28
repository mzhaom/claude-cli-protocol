package codexprotocol

// TurnContext contains turn-level configuration.
type TurnContext struct {
	// CWD is the working directory for the turn.
	CWD string `json:"cwd,omitempty"`

	// ApprovalPolicy controls when approval is required.
	ApprovalPolicy AskForApproval `json:"approval_policy,omitempty"`

	// SandboxPolicy controls sandboxing behavior.
	SandboxPolicy SandboxPolicy `json:"sandbox_policy,omitempty"`

	// Model is the model to use for the turn.
	Model string `json:"model,omitempty"`

	// Effort controls reasoning effort level (for reasoning models).
	Effort string `json:"effort,omitempty"`

	// Summary controls reasoning summary mode (for reasoning models).
	Summary string `json:"summary,omitempty"`

	// BaseInstructions overrides the default base instructions.
	BaseInstructions string `json:"base_instructions,omitempty"`

	// UserInstructions provides additional user instructions.
	UserInstructions string `json:"user_instructions,omitempty"`

	// DeveloperInstructions provides additional developer instructions.
	DeveloperInstructions string `json:"developer_instructions,omitempty"`

	// FinalOutputJSONSchema constrains the final assistant message.
	FinalOutputJSONSchema interface{} `json:"final_output_json_schema,omitempty"`
}

// SessionConfiguration contains session-level settings.
type SessionConfiguration struct {
	// Provider is the model provider (e.g., "openai", "openrouter").
	Provider string `json:"provider,omitempty"`

	// Model is the default model to use.
	Model string `json:"model,omitempty"`

	// CWD is the default working directory.
	CWD string `json:"cwd,omitempty"`

	// ApprovalPolicy is the default approval policy.
	ApprovalPolicy AskForApproval `json:"approval_policy,omitempty"`

	// SandboxPolicy is the default sandbox policy.
	SandboxPolicy SandboxPolicy `json:"sandbox_policy,omitempty"`

	// DeveloperInstructions are instructions for the developer.
	DeveloperInstructions string `json:"developer_instructions,omitempty"`

	// UserInstructions are instructions from the user.
	UserInstructions string `json:"user_instructions,omitempty"`

	// BaseInstructions are the base system instructions.
	BaseInstructions string `json:"base_instructions,omitempty"`

	// ReasoningEffort controls reasoning verbosity.
	ReasoningEffort string `json:"model_reasoning_effort,omitempty"`

	// ReasoningSummary controls reasoning summary output.
	ReasoningSummary string `json:"model_reasoning_summary,omitempty"`
}

// ReasoningEffort values for reasoning models.
const (
	ReasoningEffortMinimal = "minimal"
	ReasoningEffortLow     = "low"
	ReasoningEffortMedium  = "medium"
	ReasoningEffortHigh    = "high"
)

// ReasoningSummary values for reasoning models.
const (
	ReasoningSummaryHidden  = "hidden"
	ReasoningSummaryVisible = "visible"
)
