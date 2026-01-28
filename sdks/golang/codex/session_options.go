package codex

import (
	"github.com/mzhaom/claude-cli-protocol/sdks/golang/codexprotocol"
)

// SessionConfig holds session configuration.
type SessionConfig struct {
	// Model to use (e.g., "o3", "o3-mini", "o4-mini")
	Model string

	// WorkDir is the working directory for file operations.
	WorkDir string

	// ApprovalPolicy controls when approval is required.
	ApprovalPolicy codexprotocol.AskForApproval

	// SandboxPolicy controls sandboxing behavior.
	SandboxPolicy codexprotocol.SandboxPolicy

	// CLIPath is the path to the Codex CLI binary (uses "codex" in PATH if empty).
	CLIPath string

	// RecordMessages enables session recording.
	RecordMessages bool

	// RecordingDir is the directory for recordings (default: .codex-sessions).
	RecordingDir string

	// ApprovalHandler handles approval requests.
	ApprovalHandler ApprovalHandler

	// EventBufferSize is the event channel buffer size (default: 100).
	EventBufferSize int

	// StderrHandler is an optional handler for CLI stderr output.
	StderrHandler func([]byte)

	// DeveloperInstructions are instructions for the developer.
	DeveloperInstructions string

	// UserInstructions are instructions from the user.
	UserInstructions string

	// BaseInstructions are the base system instructions.
	BaseInstructions string

	// ReasoningEffort controls reasoning effort level.
	ReasoningEffort string

	// ReasoningSummary controls reasoning summary mode.
	ReasoningSummary string
}

// SessionOption is a functional option for configuring a Session.
type SessionOption func(*SessionConfig)

// WithModel sets the model to use.
func WithModel(model string) SessionOption {
	return func(c *SessionConfig) {
		c.Model = model
	}
}

// WithWorkDir sets the working directory.
func WithWorkDir(dir string) SessionOption {
	return func(c *SessionConfig) {
		c.WorkDir = dir
	}
}

// WithApprovalPolicy sets the approval policy.
func WithApprovalPolicy(policy codexprotocol.AskForApproval) SessionOption {
	return func(c *SessionConfig) {
		c.ApprovalPolicy = policy
	}
}

// WithSandboxPolicy sets the sandbox policy.
func WithSandboxPolicy(policy codexprotocol.SandboxPolicy) SessionOption {
	return func(c *SessionConfig) {
		c.SandboxPolicy = policy
	}
}

// WithCLIPath sets a custom CLI binary path.
func WithCLIPath(path string) SessionOption {
	return func(c *SessionConfig) {
		c.CLIPath = path
	}
}

// WithRecording enables session recording.
func WithRecording(dir string) SessionOption {
	return func(c *SessionConfig) {
		c.RecordMessages = true
		if dir != "" {
			c.RecordingDir = dir
		}
	}
}

// WithApprovalHandler sets a custom approval handler.
func WithApprovalHandler(h ApprovalHandler) SessionOption {
	return func(c *SessionConfig) {
		c.ApprovalHandler = h
	}
}

// WithEventBufferSize sets the event channel buffer size.
func WithEventBufferSize(size int) SessionOption {
	return func(c *SessionConfig) {
		c.EventBufferSize = size
	}
}

// WithStderrHandler sets a handler for CLI stderr output.
func WithStderrHandler(h func([]byte)) SessionOption {
	return func(c *SessionConfig) {
		c.StderrHandler = h
	}
}

// WithDeveloperInstructions sets developer instructions.
func WithDeveloperInstructions(instructions string) SessionOption {
	return func(c *SessionConfig) {
		c.DeveloperInstructions = instructions
	}
}

// WithUserInstructions sets user instructions.
func WithUserInstructions(instructions string) SessionOption {
	return func(c *SessionConfig) {
		c.UserInstructions = instructions
	}
}

// WithBaseInstructions sets base instructions.
func WithBaseInstructions(instructions string) SessionOption {
	return func(c *SessionConfig) {
		c.BaseInstructions = instructions
	}
}

// WithReasoningEffort sets reasoning effort level.
func WithReasoningEffort(effort string) SessionOption {
	return func(c *SessionConfig) {
		c.ReasoningEffort = effort
	}
}

// WithReasoningSummary sets reasoning summary mode.
func WithReasoningSummary(summary string) SessionOption {
	return func(c *SessionConfig) {
		c.ReasoningSummary = summary
	}
}

// defaultConfig returns the default configuration.
func defaultConfig() SessionConfig {
	return SessionConfig{
		Model:           "o3-mini",
		ApprovalPolicy:  codexprotocol.ApprovalPolicyOnRequest,
		SandboxPolicy:   codexprotocol.SandboxPolicyWorkspaceWrite,
		EventBufferSize: 100,
		RecordingDir:    ".codex-sessions",
	}
}
