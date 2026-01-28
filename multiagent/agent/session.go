package agent

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"sync/atomic"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude"
)

// taskCounter is used to generate unique task IDs for ephemeral sessions.
var taskCounter uint64

// nextTaskID returns a unique task ID.
func nextTaskID() string {
	id := atomic.AddUint64(&taskCounter, 1)
	return fmt.Sprintf("task-%03d", id)
}

// LongRunningSession wraps a claude.Session for long-running agents (Orchestrator, Planner).
// It maintains a persistent session across multiple turns.
type LongRunningSession struct {
	mu         sync.Mutex
	session    *claude.Session
	config     AgentConfig
	sessionDir string
	totalCost  float64
	turnCount  int
	started    bool

	// Additional session options (set before Start)
	extraOptions []claude.SessionOption
}

// NewLongRunningSession creates a new long-running session.
func NewLongRunningSession(config AgentConfig, swarmSessionID string) *LongRunningSession {
	sessionDir := filepath.Join(config.SessionDir, swarmSessionID, config.Role.String())
	return &LongRunningSession{
		config:     config,
		sessionDir: sessionDir,
	}
}

// SetSessionOptions sets additional Claude session options.
// Must be called before Start().
func (s *LongRunningSession) SetSessionOptions(opts ...claude.SessionOption) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.extraOptions = append(s.extraOptions, opts...)
}

// Start marks the session as ready to be used.
// The actual Claude session is started lazily on first message to avoid
// creating empty session directories for agents that don't send messages.
func (s *LongRunningSession) Start(ctx context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if s.started {
		return fmt.Errorf("session already started")
	}

	s.started = true
	return nil
}

// ensureSession starts the underlying Claude session if not already started.
// This implements lazy initialization to avoid creating empty directories.
func (s *LongRunningSession) ensureSession(ctx context.Context) error {
	if s.session != nil {
		return nil
	}

	// Ensure session directory exists
	if err := os.MkdirAll(s.sessionDir, 0755); err != nil {
		return fmt.Errorf("failed to create session directory: %w", err)
	}

	// Build session options
	opts := []claude.SessionOption{
		claude.WithModel(s.config.Model),
		claude.WithWorkDir(s.config.WorkDir),
		claude.WithRecording(s.sessionDir),
		claude.WithPermissionMode(claude.PermissionModeBypass),
		claude.WithDisablePlugins(),
	}

	// Add any extra options (e.g., MCP config)
	opts = append(opts, s.extraOptions...)

	// Create the Claude session with recording enabled
	s.session = claude.NewSession(opts...)

	if err := s.session.Start(ctx); err != nil {
		return fmt.Errorf("failed to start session: %w", err)
	}

	return nil
}

// Stop gracefully shuts down the session.
func (s *LongRunningSession) Stop() error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.started || s.session == nil {
		return nil
	}

	s.started = false
	return s.session.Stop()
}

// SendMessage sends a message and waits for the turn to complete.
// This is the primary way to interact with long-running agents.
func (s *LongRunningSession) SendMessage(ctx context.Context, message string) (*claude.TurnResult, error) {
	s.mu.Lock()
	if !s.started {
		s.mu.Unlock()
		return nil, fmt.Errorf("session not started")
	}

	// Ensure the Claude session is started (lazy initialization)
	if err := s.ensureSession(ctx); err != nil {
		s.mu.Unlock()
		return nil, err
	}

	session := s.session
	s.mu.Unlock()

	// Send the message
	result, err := session.Ask(ctx, message)
	if err != nil {
		return nil, err
	}

	// Update metrics
	s.mu.Lock()
	s.totalCost += result.Usage.CostUSD
	s.turnCount++
	s.mu.Unlock()

	return result, nil
}

// Events returns the event channel for streaming responses.
func (s *LongRunningSession) Events() <-chan claude.Event {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.session == nil {
		return nil
	}
	return s.session.Events()
}

// SessionDir returns the session recording directory.
func (s *LongRunningSession) SessionDir() string {
	return s.sessionDir
}

// TotalCost returns the accumulated cost.
func (s *LongRunningSession) TotalCost() float64 {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.totalCost
}

// TurnCount returns the number of turns completed.
func (s *LongRunningSession) TurnCount() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.turnCount
}

// Recording returns the session recording if available.
func (s *LongRunningSession) Recording() *claude.SessionRecording {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.session == nil {
		return nil
	}
	return s.session.Recording()
}

// EphemeralSession creates fresh Claude sessions for each task (Designer, Builder, Reviewer).
// Each Execute() call creates a new session, runs one interaction, and stops the session.
type EphemeralSession struct {
	config          AgentConfig
	swarmSessionID  string
	baseSessionDir  string
	mu              sync.Mutex
	totalCost       float64
	taskCount       int
}

// NewEphemeralSession creates a new ephemeral session factory.
func NewEphemeralSession(config AgentConfig, swarmSessionID string) *EphemeralSession {
	baseSessionDir := filepath.Join(config.SessionDir, swarmSessionID, config.Role.String())
	return &EphemeralSession{
		config:         config,
		swarmSessionID: swarmSessionID,
		baseSessionDir: baseSessionDir,
	}
}

// ExecuteResult contains the result of an ephemeral session execution.
type ExecuteResult struct {
	*claude.TurnResult
	FilesCreated  []string
	FilesModified []string
}

// Execute creates a fresh session, runs the prompt, and returns the result.
// Each call is independent - no conversation history is preserved.
func (e *EphemeralSession) Execute(ctx context.Context, prompt string) (*claude.TurnResult, string, error) {
	result, _, taskID, err := e.ExecuteWithFiles(ctx, prompt)
	return result, taskID, err
}

// ExecuteWithFiles creates a fresh session, runs the prompt, and returns the result with file tracking.
// This method tracks Write and Edit tool calls to report which files were created/modified.
func (e *EphemeralSession) ExecuteWithFiles(ctx context.Context, prompt string) (*claude.TurnResult, *ExecuteResult, string, error) {
	// Generate unique task directory
	taskID := nextTaskID()
	taskDir := filepath.Join(e.baseSessionDir, taskID)

	// Ensure task directory exists
	if err := os.MkdirAll(taskDir, 0755); err != nil {
		return nil, nil, "", fmt.Errorf("failed to create task directory: %w", err)
	}

	// Create a fresh session for this task
	session := claude.NewSession(
		claude.WithModel(e.config.Model),
		claude.WithWorkDir(e.config.WorkDir),
		claude.WithRecording(taskDir),
		claude.WithPermissionMode(claude.PermissionModeBypass),
		claude.WithDisablePlugins(),
	)

	// Start the session
	if err := session.Start(ctx); err != nil {
		return nil, nil, taskID, fmt.Errorf("failed to start session: %w", err)
	}

	// Track files from tool events in background
	var filesMu sync.Mutex
	filesCreated := make([]string, 0)
	filesModified := make([]string, 0)
	fileSeen := make(map[string]bool)
	eventsDone := make(chan struct{})

	go func() {
		defer close(eventsDone)
		events := session.Events()
		if events == nil {
			return
		}
		for event := range events {
			if toolEvent, ok := event.(claude.ToolCompleteEvent); ok {
				filePath, _ := toolEvent.Input["file_path"].(string)
				if filePath != "" {
					filesMu.Lock()
					if !fileSeen[filePath] {
						fileSeen[filePath] = true
						switch toolEvent.Name {
						case "Write":
							filesCreated = append(filesCreated, filePath)
						case "Edit":
							filesModified = append(filesModified, filePath)
						}
					}
					filesMu.Unlock()
				}
			}
		}
	}()

	// Execute the single turn
	result, err := session.Ask(ctx, prompt)

	// Stop the session first - this closes the events channel
	session.Stop()

	// Now wait for event processing goroutine to finish
	<-eventsDone

	if err != nil {
		return nil, nil, taskID, err
	}

	// Update metrics
	e.mu.Lock()
	e.totalCost += result.Usage.CostUSD
	e.taskCount++
	e.mu.Unlock()

	// Safe to access file lists now - goroutine has finished
	filesMu.Lock()
	execResult := &ExecuteResult{
		TurnResult:    result,
		FilesCreated:  append([]string(nil), filesCreated...),
		FilesModified: append([]string(nil), filesModified...),
	}
	filesMu.Unlock()

	return result, execResult, taskID, nil
}

// BaseSessionDir returns the base directory for task recordings.
func (e *EphemeralSession) BaseSessionDir() string {
	return e.baseSessionDir
}

// TotalCost returns the accumulated cost across all tasks.
func (e *EphemeralSession) TotalCost() float64 {
	e.mu.Lock()
	defer e.mu.Unlock()
	return e.totalCost
}

// TaskCount returns the number of tasks executed.
func (e *EphemeralSession) TaskCount() int {
	e.mu.Lock()
	defer e.mu.Unlock()
	return e.taskCount
}
