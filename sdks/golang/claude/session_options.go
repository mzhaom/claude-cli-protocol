package claude

// PermissionMode controls tool execution approval.
type PermissionMode string

const (
	// PermissionModeDefault prompts the user for each dangerous operation.
	PermissionModeDefault PermissionMode = "default"
	// PermissionModeAcceptEdits auto-approves file modifications.
	PermissionModeAcceptEdits PermissionMode = "acceptEdits"
	// PermissionModePlan reviews plan before execution.
	PermissionModePlan PermissionMode = "plan"
	// PermissionModeBypass auto-approves all tools (use with caution).
	PermissionModeBypass PermissionMode = "bypassPermissions"
)

// SessionConfig holds session configuration.
type SessionConfig struct {
	// Model to use: "haiku", "sonnet", "opus"
	Model string

	// WorkDir is the working directory for file operations.
	WorkDir string

	// PermissionMode controls tool execution approval.
	PermissionMode PermissionMode

	// CLIPath is the path to the Claude CLI binary (uses "claude" in PATH if empty).
	CLIPath string

	// DisablePlugins disables CLI plugins for faster startup.
	DisablePlugins bool

	// RecordMessages enables session recording.
	RecordMessages bool

	// RecordingDir is the directory for recordings (default: .claude-sessions).
	RecordingDir string

	// PermissionHandler handles permission requests in default mode.
	PermissionHandler PermissionHandler

	// EventBufferSize is the event channel buffer size (default: 100).
	EventBufferSize int

	// StderrHandler is an optional handler for CLI stderr output.
	StderrHandler func([]byte)
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

// WithPermissionMode sets the permission mode.
func WithPermissionMode(mode PermissionMode) SessionOption {
	return func(c *SessionConfig) {
		c.PermissionMode = mode
	}
}

// WithCLIPath sets a custom CLI binary path.
func WithCLIPath(path string) SessionOption {
	return func(c *SessionConfig) {
		c.CLIPath = path
	}
}

// WithDisablePlugins disables CLI plugins.
func WithDisablePlugins() SessionOption {
	return func(c *SessionConfig) {
		c.DisablePlugins = true
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

// WithPermissionHandler sets a custom permission handler.
func WithPermissionHandler(h PermissionHandler) SessionOption {
	return func(c *SessionConfig) {
		c.PermissionHandler = h
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

// defaultConfig returns the default configuration.
func defaultConfig() SessionConfig {
	return SessionConfig{
		Model:           "haiku",
		PermissionMode:  PermissionModeDefault,
		EventBufferSize: 100,
		RecordingDir:    ".claude-sessions",
	}
}
