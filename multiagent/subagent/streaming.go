package subagent

import (
	"context"
	"fmt"
	"sync"
	"time"

	"github.com/mzhaom/claude-cli-protocol/multiagent/agent"
	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude"
)

// StreamingSubAgent wraps an ephemeral Claude session with streaming progress.
// It transforms Claude SDK events into protocol events and tracks file operations,
// costs, and accumulated text automatically.
type StreamingSubAgent struct {
	config     agent.AgentConfig
	sessionID  string
	requestID  string
	agentType  AgentType

	// Event streaming
	events     chan interface{}
	eventsDone chan struct{}

	// Cancellation support
	mu         sync.Mutex
	cancelled  bool
	cancelFunc context.CancelFunc

	// Tracked state
	filesCreated  []string
	filesModified []string
	fullText      string
	totalCost     float64
	inputTokens   int
	outputTokens  int
}

// NewStreamingSubAgent creates a new streaming sub-agent.
func NewStreamingSubAgent(config agent.AgentConfig, sessionID, requestID string, agentType AgentType) *StreamingSubAgent {
	return &StreamingSubAgent{
		config:        config,
		sessionID:     sessionID,
		requestID:     requestID,
		agentType:     agentType,
		events:        make(chan interface{}, 100),
		eventsDone:    make(chan struct{}),
		filesCreated:  make([]string, 0),
		filesModified: make([]string, 0),
	}
}

// Events returns the event channel for streaming updates.
// The channel receives Progress, FileEvent, CostUpdate, Result, and Error events.
func (s *StreamingSubAgent) Events() <-chan interface{} {
	return s.events
}

// Cancel requests cancellation of the current task.
func (s *StreamingSubAgent) Cancel(reason string) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if s.cancelled {
		return
	}
	s.cancelled = true

	if s.cancelFunc != nil {
		s.cancelFunc()
	}

	// Emit cancellation error
	s.emitEventLocked(NewError(s.requestID, s.agentType, "cancelled", reason, false))
}

// IsCancelled returns whether the sub-agent has been cancelled.
func (s *StreamingSubAgent) IsCancelled() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.cancelled
}

// Execute runs the task with streaming progress.
// It creates a Claude session, processes events, and returns the final result.
func (s *StreamingSubAgent) Execute(ctx context.Context, prompt string) (*Result, error) {
	startTime := time.Now()

	// Create cancellable context
	ctx, cancel := context.WithCancel(ctx)
	s.mu.Lock()
	s.cancelFunc = cancel
	s.mu.Unlock()
	defer cancel()

	// Apply timeout if configured
	if s.config.TurnTimeout > 0 {
		var timeoutCancel context.CancelFunc
		ctx, timeoutCancel = context.WithTimeout(ctx, s.config.TurnTimeout)
		defer timeoutCancel()
	}

	// Create session options
	opts := []claude.SessionOption{
		claude.WithModel(s.config.Model),
		claude.WithWorkDir(s.config.WorkDir),
		claude.WithPermissionMode(claude.PermissionModeBypass),
		claude.WithDisablePlugins(),
	}

	if s.config.SystemPrompt != "" {
		opts = append(opts, claude.WithSystemPrompt(s.config.SystemPrompt))
	}

	// Create Claude session
	session := claude.NewSession(opts...)

	if err := session.Start(ctx); err != nil {
		s.emitError("session_start_failed", err.Error(), true)
		s.closeEvents()
		return nil, fmt.Errorf("failed to start session: %w", err)
	}
	defer session.Stop()

	// Start event processing goroutine
	go s.processClaudeEvents(ctx, session)

	// Send the message
	result, err := session.Ask(ctx, prompt)
	if err != nil {
		s.emitError("execution_failed", err.Error(), true)
		s.closeEvents()
		return nil, fmt.Errorf("execution failed: %w", err)
	}

	// Wait for event processing to complete
	<-s.eventsDone

	// Build final result
	durationMs := time.Since(startTime).Milliseconds()
	finalResult := s.buildResult(result, durationMs)

	// Emit final result
	s.emitEvent(finalResult)
	s.closeEvents()

	return finalResult, nil
}

// processClaudeEvents reads Claude events and transforms them to protocol events.
func (s *StreamingSubAgent) processClaudeEvents(ctx context.Context, session *claude.Session) {
	defer close(s.eventsDone)

	for {
		select {
		case <-ctx.Done():
			return
		case event, ok := <-session.Events():
			if !ok {
				return
			}
			s.handleClaudeEvent(event)
		}
	}
}

// handleClaudeEvent transforms a Claude SDK event to protocol events.
func (s *StreamingSubAgent) handleClaudeEvent(event claude.Event) {
	switch e := event.(type) {
	case claude.TextEvent:
		s.mu.Lock()
		s.fullText = e.FullText
		s.mu.Unlock()

		progress := NewProgress(s.requestID, s.agentType, PhaseStreaming)
		progress.TextDelta = e.Text
		progress.FullText = e.FullText
		s.emitEvent(progress)

	case claude.ThinkingEvent:
		progress := NewProgress(s.requestID, s.agentType, PhaseThinking)
		progress.Message = "Thinking..."
		s.emitEvent(progress)

	case claude.ToolStartEvent:
		progress := NewProgress(s.requestID, s.agentType, PhaseToolCall)
		progress.ToolName = e.Name
		progress.ToolID = e.ID
		progress.ToolStarted = true
		s.emitEvent(progress)

	case claude.ToolCompleteEvent:
		progress := NewProgress(s.requestID, s.agentType, PhaseToolCall)
		progress.ToolName = e.Name
		progress.ToolID = e.ID
		progress.ToolInput = e.Input
		progress.ToolStarted = false
		s.emitEvent(progress)

		// Track file operations
		s.trackFileOperation(e.Name, e.Input)

	case claude.TurnCompleteEvent:
		s.mu.Lock()
		s.totalCost += e.Usage.CostUSD
		s.inputTokens += e.Usage.InputTokens
		s.outputTokens += e.Usage.OutputTokens
		totalCost := s.totalCost
		inputTokens := s.inputTokens
		outputTokens := s.outputTokens
		s.mu.Unlock()

		costUpdate := NewCostUpdate(
			s.requestID,
			s.agentType,
			e.Usage.CostUSD,
			totalCost,
			inputTokens,
			outputTokens,
		)
		s.emitEvent(costUpdate)

	case claude.ErrorEvent:
		s.emitError("claude_error", e.Error.Error(), true)
	}
}

// trackFileOperation extracts file paths from tool inputs and emits FileEvents.
func (s *StreamingSubAgent) trackFileOperation(toolName string, input map[string]interface{}) {
	var path string
	var action FileAction

	switch toolName {
	case "Write":
		if p, ok := input["file_path"].(string); ok {
			path = p
			action = FileActionCreate
		}
	case "Edit":
		if p, ok := input["file_path"].(string); ok {
			path = p
			action = FileActionModify
		}
	case "Read":
		if p, ok := input["file_path"].(string); ok {
			path = p
			action = FileActionRead
		}
	default:
		// Other tools don't track file operations
		return
	}

	if path == "" {
		return
	}

	// Emit file event
	fileEvent := NewFileEvent(s.requestID, s.agentType, path, action, toolName)
	s.emitEvent(fileEvent)

	// Track in local state
	s.mu.Lock()
	defer s.mu.Unlock()

	switch action {
	case FileActionCreate:
		s.filesCreated = append(s.filesCreated, path)
	case FileActionModify:
		s.filesModified = append(s.filesModified, path)
	}
}

// buildResult creates a Result from the turn result and tracked state.
func (s *StreamingSubAgent) buildResult(turnResult *claude.TurnResult, durationMs int64) *Result {
	s.mu.Lock()
	defer s.mu.Unlock()

	result := NewResult(s.requestID, s.agentType, turnResult.Success, durationMs)
	result.Text = s.fullText
	result.FilesCreated = s.filesCreated
	result.FilesModified = s.filesModified
	result.TotalCostUSD = s.totalCost

	if !turnResult.Success && turnResult.Error != nil {
		result.Error = turnResult.Error.Error()
	}

	return result
}

// emitEvent sends an event to the events channel.
func (s *StreamingSubAgent) emitEvent(event interface{}) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.emitEventLocked(event)
}

// emitEventLocked sends an event while holding the lock.
func (s *StreamingSubAgent) emitEventLocked(event interface{}) {
	select {
	case s.events <- event:
	default:
		// Channel full, drop event
		// In a production system, we might want to log this
	}
}

// emitError sends an error event.
func (s *StreamingSubAgent) emitError(code, message string, retryable bool) {
	s.emitEvent(NewError(s.requestID, s.agentType, code, message, retryable))
}

// closeEvents closes the events channel.
func (s *StreamingSubAgent) closeEvents() {
	close(s.events)
}

// RequestID returns the request ID.
func (s *StreamingSubAgent) RequestID() string {
	return s.requestID
}

// AgentType returns the agent type.
func (s *StreamingSubAgent) AgentType() AgentType {
	return s.agentType
}

// TotalCost returns the accumulated cost.
func (s *StreamingSubAgent) TotalCost() float64 {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.totalCost
}

// FullText returns the accumulated response text.
func (s *StreamingSubAgent) FullText() string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.fullText
}

// FilesCreated returns the list of files created.
func (s *StreamingSubAgent) FilesCreated() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	result := make([]string, len(s.filesCreated))
	copy(result, s.filesCreated)
	return result
}

// FilesModified returns the list of files modified.
func (s *StreamingSubAgent) FilesModified() []string {
	s.mu.Lock()
	defer s.mu.Unlock()
	result := make([]string, len(s.filesModified))
	copy(result, s.filesModified)
	return result
}
