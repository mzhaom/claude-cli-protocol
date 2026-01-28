package codex

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"io"
	"sync"
	"time"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/codexprotocol"
)

// SessionInfo contains session metadata.
type SessionInfo struct {
	SessionID      string
	Model          string
	WorkDir        string
	Tools          []string
	ApprovalPolicy codexprotocol.AskForApproval
	SandboxPolicy  codexprotocol.SandboxPolicy
}

// Session manages interaction with the Codex CLI.
type Session struct {
	mu sync.RWMutex

	config          SessionConfig
	process         *processManager
	accumulator     *streamAccumulator
	turnManager     *turnManager
	approvalManager *approvalManager
	recorder        *sessionRecorder
	state           *sessionState

	// Event channel (read-only to consumers)
	events chan Event

	// Session info (set after init)
	info *SessionInfo

	// Submission ID counter
	subID int

	// Lifecycle
	started  bool
	stopping bool
	done     chan struct{}
}

// NewSession creates a new Codex session with options.
func NewSession(opts ...SessionOption) *Session {
	config := defaultConfig()
	for _, opt := range opts {
		opt(&config)
	}

	s := &Session{
		config: config,
		events: make(chan Event, config.EventBufferSize),
		done:   make(chan struct{}),
	}

	s.turnManager = newTurnManager()
	s.state = newSessionState()
	s.accumulator = newStreamAccumulator(s)
	s.approvalManager = newApprovalManager(config.ApprovalHandler)

	if config.RecordMessages {
		s.recorder = newSessionRecorder(config.RecordingDir)
	}

	return s
}

// Start spawns the CLI process and begins the session.
func (s *Session) Start(ctx context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if s.started {
		return ErrAlreadyStarted
	}

	s.process = newProcessManager(s.config)
	if err := s.process.Start(ctx); err != nil {
		return err
	}

	// Transition state
	if err := s.state.Transition(TransitionStarted); err != nil {
		s.process.Stop()
		return err
	}

	// Start message handling goroutine
	go s.readLoop(ctx)

	// Start stderr handling if configured
	if s.config.StderrHandler != nil {
		go s.stderrLoop()
	}

	s.started = true
	return nil
}

// Events returns a read-only channel for receiving events.
func (s *Session) Events() <-chan Event {
	return s.events
}

// nextSubmissionID generates the next submission ID.
func (s *Session) nextSubmissionID() string {
	s.subID++
	return fmt.Sprintf("sub_%d", s.subID)
}

// SendMessage sends a user message and starts a new turn.
// Returns the turn number.
func (s *Session) SendMessage(ctx context.Context, content string) (int, error) {
	return s.SendUserInput(ctx, []codexprotocol.UserInput{
		&codexprotocol.TextInput{Text: content},
	})
}

// SendUserInput sends user input items and starts a new turn.
// Returns the turn number.
func (s *Session) SendUserInput(ctx context.Context, items []codexprotocol.UserInput) (int, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.started {
		return 0, ErrNotStarted
	}

	if s.stopping {
		return 0, ErrStopping
	}

	turn := s.turnManager.StartTurn(items)

	// Record turn start
	if s.recorder != nil {
		s.recorder.StartTurn(turn.Number, turn.TurnID, items)
	}

	sub := codexprotocol.Submission{
		ID: s.nextSubmissionID(),
		Op: &codexprotocol.UserInputOp{
			Items: items,
		},
	}

	if err := s.process.WriteMessage(sub); err != nil {
		return 0, err
	}

	if s.recorder != nil {
		s.recorder.RecordSent(sub)
	}

	// Transition to processing state if we're ready
	// Ignore error if already processing (multiple messages in flight)
	_ = s.state.Transition(TransitionUserMessageSent)

	return turn.Number, nil
}

// convertUserInputItems converts UserInput slice for protocol operations.
func convertUserInputItems(items []codexprotocol.UserInput) []codexprotocol.UserInput {
	return items
}

// SendTurn sends a full turn with turn context configuration.
// Returns the turn number.
func (s *Session) SendTurn(ctx context.Context, items []codexprotocol.UserInput, turnCtx *codexprotocol.TurnContext) (int, error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.started {
		return 0, ErrNotStarted
	}

	if s.stopping {
		return 0, ErrStopping
	}

	turn := s.turnManager.StartTurn(items)

	// Record turn start
	if s.recorder != nil {
		s.recorder.StartTurn(turn.Number, turn.TurnID, items)
	}

	op := &codexprotocol.UserTurnOp{
		Items: items,
	}
	if turnCtx != nil {
		op.CWD = turnCtx.CWD
		op.ApprovalPolicy = turnCtx.ApprovalPolicy
		op.SandboxPolicy = turnCtx.SandboxPolicy
		op.Model = turnCtx.Model
		op.Effort = turnCtx.Effort
		op.Summary = turnCtx.Summary
		op.FinalOutputJSONSchema = turnCtx.FinalOutputJSONSchema
	}

	sub := codexprotocol.Submission{
		ID: s.nextSubmissionID(),
		Op: op,
	}

	if err := s.process.WriteMessage(sub); err != nil {
		return 0, err
	}

	if s.recorder != nil {
		s.recorder.RecordSent(sub)
	}

	// Transition to processing state
	_ = s.state.Transition(TransitionUserMessageSent)

	return turn.Number, nil
}

// WaitForTurn blocks until the current turn completes.
// If no turn is in progress, it returns immediately with nil.
func (s *Session) WaitForTurn(ctx context.Context) (*TurnResult, error) {
	turnNumber := s.turnManager.CurrentTurnNumber()
	if turnNumber == 0 {
		return nil, nil
	}
	return s.turnManager.WaitForTurn(ctx, turnNumber)
}

// Ask sends a message and waits for turn completion (blocking).
func (s *Session) Ask(ctx context.Context, content string) (*TurnResult, error) {
	_, err := s.SendMessage(ctx, content)
	if err != nil {
		return nil, err
	}
	return s.WaitForTurn(ctx)
}

// AskWithTimeout is a convenience wrapper with timeout context.
func (s *Session) AskWithTimeout(content string, timeout time.Duration) (*TurnResult, error) {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	return s.Ask(ctx, content)
}

// Interrupt sends an interrupt operation.
func (s *Session) Interrupt(ctx context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.started {
		return ErrNotStarted
	}

	sub := codexprotocol.Submission{
		ID: s.nextSubmissionID(),
		Op: &codexprotocol.InterruptOp{},
	}

	if s.recorder != nil {
		s.recorder.RecordSent(sub)
	}

	return s.process.WriteMessage(sub)
}

// Compact sends a compact operation to summarize conversation context.
func (s *Session) Compact(ctx context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.started {
		return ErrNotStarted
	}

	sub := codexprotocol.Submission{
		ID: s.nextSubmissionID(),
		Op: &codexprotocol.CompactOp{},
	}

	if s.recorder != nil {
		s.recorder.RecordSent(sub)
	}

	return s.process.WriteMessage(sub)
}

// Undo sends an undo operation.
func (s *Session) Undo(ctx context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.started {
		return ErrNotStarted
	}

	sub := codexprotocol.Submission{
		ID: s.nextSubmissionID(),
		Op: &codexprotocol.UndoOp{},
	}

	if s.recorder != nil {
		s.recorder.RecordSent(sub)
	}

	return s.process.WriteMessage(sub)
}

// Shutdown sends a shutdown operation.
func (s *Session) Shutdown(ctx context.Context) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	if !s.started {
		return ErrNotStarted
	}

	sub := codexprotocol.Submission{
		ID: s.nextSubmissionID(),
		Op: &codexprotocol.ShutdownOp{},
	}

	if s.recorder != nil {
		s.recorder.RecordSent(sub)
	}

	return s.process.WriteMessage(sub)
}

// Stop gracefully shuts down the session.
func (s *Session) Stop() error {
	s.mu.Lock()
	if !s.started || s.stopping {
		s.mu.Unlock()
		return nil
	}
	s.stopping = true
	s.mu.Unlock()

	// Close done channel to signal goroutines
	close(s.done)

	// Stop the process
	if s.process != nil {
		s.process.Stop()
	}

	// Transition to closed state
	_ = s.state.Transition(TransitionClosed)

	// Close recorder
	if s.recorder != nil {
		s.recorder.Close()
	}

	// Close event channel after process stops
	close(s.events)

	return nil
}

// Info returns session information (available after Ready event).
func (s *Session) Info() *SessionInfo {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return s.info
}

// CurrentTurnNumber returns the current turn number.
func (s *Session) CurrentTurnNumber() int {
	return s.turnManager.CurrentTurnNumber()
}

// State returns the current session state.
func (s *Session) State() SessionState {
	return s.state.Current()
}

// Recording returns the session recording (if enabled).
func (s *Session) Recording() *SessionRecording {
	if s.recorder == nil {
		return nil
	}
	return s.recorder.GetRecording()
}

// RecordingPath returns the path to recordings (if enabled).
func (s *Session) RecordingPath() string {
	if s.recorder == nil {
		return ""
	}
	return s.recorder.Path()
}

// CLIArgs returns the CLI arguments that will be (or were) used to spawn the CLI.
func (s *Session) CLIArgs() ([]string, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	pm := newProcessManager(s.config)
	return pm.BuildCLIArgs()
}

// readLoop reads and processes messages from the CLI.
func (s *Session) readLoop(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-s.done:
			return
		default:
			line, err := s.process.ReadLine()
			if err != nil {
				if err == io.EOF {
					return
				}
				if !s.stopping {
					s.emitError(err, "read_line", nil)
				}
				return
			}

			s.handleLine(line)
		}
	}
}

// stderrLoop reads and handles stderr from the CLI.
func (s *Session) stderrLoop() {
	stderr := s.process.Stderr()
	if stderr == nil {
		return
	}

	buf := make([]byte, 4096)
	for {
		select {
		case <-s.done:
			return
		default:
			n, err := stderr.Read(buf)
			if err != nil {
				return
			}
			if n > 0 && s.config.StderrHandler != nil {
				s.config.StderrHandler(buf[:n])
			}
		}
	}
}

// handleLine processes a single JSON line.
func (s *Session) handleLine(line []byte) {
	event, err := codexprotocol.ParseEvent(line)
	if err != nil {
		s.emitError(&ProtocolError{
			Message: "failed to parse event",
			Line:    string(line),
			Cause:   err,
		}, "parse_event", nil)
		return
	}

	// Record received event
	if s.recorder != nil {
		s.recorder.RecordReceived(event)
	}

	s.handleEvent(event)
}

// handleEvent processes a single event.
func (s *Session) handleEvent(event *codexprotocol.Event) {
	switch msg := event.Msg.(type) {
	case *codexprotocol.SessionConfiguredEvent:
		s.handleSessionConfigured(msg)
	case *codexprotocol.TurnStartedEvent:
		s.handleTurnStarted(msg)
	case *codexprotocol.TurnCompleteEvent:
		s.handleTurnComplete(msg)
	case *codexprotocol.TurnAbortedEvent:
		s.handleTurnAborted(msg)
	case *codexprotocol.AgentMessageEvent:
		s.handleAgentMessage(msg)
	case *codexprotocol.AgentMessageDeltaEvent:
		s.accumulator.HandleEvent(msg)
	case *codexprotocol.AgentReasoningEvent:
		s.handleAgentReasoning(msg)
	case *codexprotocol.AgentReasoningDeltaEvent:
		s.accumulator.HandleEvent(msg)
	case *codexprotocol.AgentReasoningRawContentEvent:
		s.handleAgentReasoningRaw(msg)
	case *codexprotocol.AgentReasoningRawContentDeltaEvent:
		s.accumulator.HandleEvent(msg)
	case *codexprotocol.ExecApprovalRequestEvent:
		s.handleExecApprovalRequest(msg)
	case *codexprotocol.ApplyPatchApprovalRequestEvent:
		s.handlePatchApprovalRequest(msg)
	case *codexprotocol.ExecCommandBeginEvent:
		s.handleExecCommandBegin(msg)
	case *codexprotocol.ExecCommandOutputDeltaEvent:
		s.handleExecCommandOutputDelta(msg)
	case *codexprotocol.ExecCommandEndEvent:
		s.handleExecCommandEnd(msg)
	case *codexprotocol.PatchApplyBeginEvent:
		s.handlePatchApplyBegin(msg)
	case *codexprotocol.PatchApplyEndEvent:
		s.handlePatchApplyEnd(msg)
	case *codexprotocol.McpToolCallBeginEvent:
		s.handleMCPToolCallBegin(msg)
	case *codexprotocol.McpToolCallEndEvent:
		s.handleMCPToolCallEnd(msg)
	case *codexprotocol.ErrorEvent:
		s.handleError(msg)
	case *codexprotocol.WarningEvent:
		s.handleWarning(msg)
	case *codexprotocol.TokenCountEvent:
		s.handleTokenCount(msg)
	case *codexprotocol.ShutdownCompleteEvent:
		s.handleShutdownComplete(msg)
	}
}

func (s *Session) handleSessionConfigured(msg *codexprotocol.SessionConfiguredEvent) {
	// Extract sandbox policy type from interface
	sandboxPolicy := ""
	if sp, ok := msg.SandboxPolicy.(string); ok {
		sandboxPolicy = sp
	} else if spMap, ok := msg.SandboxPolicy.(map[string]interface{}); ok {
		if spType, ok := spMap["type"].(string); ok {
			sandboxPolicy = spType
		}
	}

	s.mu.Lock()
	s.info = &SessionInfo{
		SessionID:      msg.SessionID,
		Model:          msg.Model,
		WorkDir:        msg.CWD,
		Tools:          msg.Tools,
		ApprovalPolicy: codexprotocol.AskForApproval(msg.ApprovalPolicy),
		SandboxPolicy:  codexprotocol.SandboxPolicy(sandboxPolicy),
	}
	s.mu.Unlock()

	// Initialize recorder with session info
	if s.recorder != nil {
		s.recorder.Initialize(RecordingMetadata{
			SessionID:      msg.SessionID,
			Model:          msg.Model,
			WorkDir:        msg.CWD,
			Tools:          msg.Tools,
			ApprovalPolicy: msg.ApprovalPolicy,
			SandboxPolicy:  sandboxPolicy,
		})
	}

	// Transition to ready state
	_ = s.state.Transition(TransitionSessionConfigured)

	// Emit ready event
	s.emit(ReadyEvent{Info: *s.info})
}

func (s *Session) handleTurnStarted(msg *codexprotocol.TurnStartedEvent) {
	// Turn started can arrive before or after our SendMessage
	// If a turn is already in progress, update its ID
	turn := s.turnManager.CurrentTurn()
	if turn != nil && turn.TurnID == "" {
		s.turnManager.SetTurnID(msg.TurnID)
	}
}

func (s *Session) handleTurnComplete(msg *codexprotocol.TurnCompleteEvent) {
	turnNumber := s.turnManager.CurrentTurnNumber()
	turn := s.turnManager.CurrentTurn()

	durationMs := int64(0)
	if turn != nil {
		durationMs = time.Since(turn.StartTime).Milliseconds()
	}

	result := TurnResult{
		TurnNumber:       turnNumber,
		TurnID:           msg.TurnID,
		Success:          true,
		DurationMs:       durationMs,
		LastAgentMessage: msg.LastAgentMessage,
	}

	// Record turn completion
	if s.recorder != nil {
		s.recorder.CompleteTurn(turnNumber, result)
	}

	// Transition back to ready state
	_ = s.state.Transition(TransitionTurnComplete)

	// Emit turn complete event
	s.emit(TurnCompleteEvent{
		TurnNumber:       result.TurnNumber,
		Success:          result.Success,
		DurationMs:       result.DurationMs,
		Usage:            result.Usage,
		Error:            result.Error,
		LastAgentMessage: result.LastAgentMessage,
	})

	// Complete turn (notifies waiters)
	s.turnManager.CompleteTurn(result)
}

func (s *Session) handleTurnAborted(msg *codexprotocol.TurnAbortedEvent) {
	turnNumber := s.turnManager.CurrentTurnNumber()
	turn := s.turnManager.CurrentTurn()

	durationMs := int64(0)
	if turn != nil {
		durationMs = time.Since(turn.StartTime).Milliseconds()
	}

	message := msg.Message
	result := TurnResult{
		TurnNumber: turnNumber,
		Success:    false,
		DurationMs: durationMs,
		Error:      fmt.Errorf("turn aborted: %s", message),
	}

	// Record turn completion
	if s.recorder != nil {
		s.recorder.CompleteTurn(turnNumber, result)
	}

	// Transition back to ready state
	_ = s.state.Transition(TransitionTurnComplete)

	// Emit turn complete event
	s.emit(TurnCompleteEvent{
		TurnNumber: result.TurnNumber,
		Success:    result.Success,
		DurationMs: result.DurationMs,
		Error:      result.Error,
	})

	// Complete turn (notifies waiters)
	s.turnManager.CompleteTurn(result)
}

func (s *Session) handleAgentMessage(msg *codexprotocol.AgentMessageEvent) {
	turn := s.turnManager.CurrentTurn()
	if turn != nil {
		// Check if we have new text not emitted via streaming
		if len(msg.Message) > len(turn.FullText) {
			newText := msg.Message[len(turn.FullText):]
			fullText := s.turnManager.AppendText(newText)
			s.emit(TextEvent{
				TurnNumber: s.turnManager.CurrentTurnNumber(),
				Text:       newText,
				FullText:   fullText,
			})
		}
	}
}

func (s *Session) handleAgentReasoning(msg *codexprotocol.AgentReasoningEvent) {
	turn := s.turnManager.CurrentTurn()
	if turn != nil {
		// Check if we have new reasoning not emitted via streaming
		if len(msg.Text) > len(turn.FullReasoning) {
			newReasoning := msg.Text[len(turn.FullReasoning):]
			fullReasoning := s.turnManager.AppendReasoning(newReasoning)
			s.emit(ReasoningEvent{
				TurnNumber:    s.turnManager.CurrentTurnNumber(),
				Reasoning:     newReasoning,
				FullReasoning: fullReasoning,
			})
		}
	}
}

func (s *Session) handleAgentReasoningRaw(msg *codexprotocol.AgentReasoningRawContentEvent) {
	// Raw reasoning is also accumulated
	turn := s.turnManager.CurrentTurn()
	if turn != nil {
		if len(msg.Text) > len(turn.FullReasoning) {
			newReasoning := msg.Text[len(turn.FullReasoning):]
			fullReasoning := s.turnManager.AppendReasoning(newReasoning)
			s.emit(ReasoningEvent{
				TurnNumber:    s.turnManager.CurrentTurnNumber(),
				Reasoning:     newReasoning,
				FullReasoning: fullReasoning,
			})
		}
	}
}

func (s *Session) handleExecApprovalRequest(msg *codexprotocol.ExecApprovalRequestEvent) {
	reason := ""
	if msg.Reason != nil {
		reason = *msg.Reason
	}

	// Emit the approval request event
	s.emit(ExecApprovalRequestEvent{
		TurnNumber:                   s.turnManager.CurrentTurnNumber(),
		CallID:                       msg.CallID,
		TurnID:                       msg.TurnID,
		Command:                      msg.Command,
		CWD:                          msg.CWD,
		Reason:                       reason,
		ProposedExecpolicyAmendment:  msg.ProposedExecpolicyAmendment,
		ParsedCommand:                msg.ParsedCmd,
	})

	// Handle approval via approval manager
	ctx := context.Background()
	req := &ExecApprovalRequest{
		CallID:                      msg.CallID,
		TurnID:                      msg.TurnID,
		Command:                     msg.Command,
		CWD:                         msg.CWD,
		Reason:                      reason,
		ProposedExecpolicyAmendment: msg.ProposedExecpolicyAmendment,
		ParsedCommand:               msg.ParsedCmd,
	}

	op, err := s.approvalManager.HandleExecApproval(ctx, req)
	if err != nil {
		s.emitError(err, "exec_approval_handling", nil)
	}

	// Send approval response
	sub := codexprotocol.Submission{
		ID: s.nextSubmissionID(),
		Op: op,
	}

	if err := s.process.WriteMessage(sub); err != nil {
		s.emitError(err, "send_exec_approval", nil)
	}

	if s.recorder != nil {
		s.recorder.RecordSent(sub)
	}
}

func (s *Session) handlePatchApprovalRequest(msg *codexprotocol.ApplyPatchApprovalRequestEvent) {
	reason := ""
	if msg.Reason != nil {
		reason = *msg.Reason
	}

	// Emit the approval request event
	s.emit(PatchApprovalRequestEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		CallID:     msg.CallID,
		TurnID:     msg.TurnID,
		Changes:    msg.Changes,
		Reason:     reason,
		GrantRoot:  msg.GrantRoot,
	})

	// Handle approval via approval manager
	ctx := context.Background()
	req := &PatchApprovalRequest{
		CallID:    msg.CallID,
		TurnID:    msg.TurnID,
		Changes:   msg.Changes,
		Reason:    reason,
		GrantRoot: msg.GrantRoot,
	}

	op, err := s.approvalManager.HandlePatchApproval(ctx, req)
	if err != nil {
		s.emitError(err, "patch_approval_handling", nil)
	}

	// Send approval response
	sub := codexprotocol.Submission{
		ID: s.nextSubmissionID(),
		Op: op,
	}

	if err := s.process.WriteMessage(sub); err != nil {
		s.emitError(err, "send_patch_approval", nil)
	}

	if s.recorder != nil {
		s.recorder.RecordSent(sub)
	}
}

func (s *Session) handleExecCommandBegin(msg *codexprotocol.ExecCommandBeginEvent) {
	// Track command in turn state
	s.turnManager.GetOrCreateCommand(msg.CallID, msg.Command, msg.CWD)

	s.emit(CommandStartEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		CallID:     msg.CallID,
		TurnID:     msg.TurnID,
		Command:    msg.Command,
		CWD:        msg.CWD,
		ProcessID:  msg.ProcessID,
		Timestamp:  time.Now(),
	})
}

func (s *Session) handleExecCommandOutputDelta(msg *codexprotocol.ExecCommandOutputDeltaEvent) {
	stdout := ""
	stderr := ""
	if msg.Stdout != nil {
		stdout = *msg.Stdout
	}
	if msg.Stderr != nil {
		stderr = *msg.Stderr
	}

	// Update command state
	cmd := s.turnManager.GetCommand(msg.CallID)
	if cmd != nil {
		if stdout != "" {
			cmd.Stdout = append(cmd.Stdout, []byte(stdout)...)
		}
		if stderr != "" {
			cmd.Stderr = append(cmd.Stderr, []byte(stderr)...)
		}
	}

	s.emit(CommandOutputEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		CallID:     msg.CallID,
		Stdout:     stdout,
		Stderr:     stderr,
	})
}

func (s *Session) handleExecCommandEnd(msg *codexprotocol.ExecCommandEndEvent) {
	// Update command state
	cmd := s.turnManager.GetCommand(msg.CallID)
	if cmd != nil {
		cmd.ExitCode = &msg.ExitCode
		cmd.TimedOut = msg.TimedOut
	}

	s.emit(CommandEndEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		CallID:     msg.CallID,
		TurnID:     msg.TurnID,
		Command:    msg.Command,
		CWD:        msg.CWD,
		ProcessID:  msg.ProcessID,
		ExitCode:   msg.ExitCode,
		TimedOut:   msg.TimedOut,
		Timestamp:  time.Now(),
	})
}

func (s *Session) handlePatchApplyBegin(msg *codexprotocol.PatchApplyBeginEvent) {
	reason := ""
	if msg.Reason != nil {
		reason = *msg.Reason
	}
	s.emit(PatchStartEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		CallID:     msg.CallID,
		TurnID:     msg.TurnID,
		Changes:    msg.Changes,
		Reason:     reason,
	})
}

func (s *Session) handlePatchApplyEnd(msg *codexprotocol.PatchApplyEndEvent) {
	errStr := ""
	if msg.Error != nil {
		errStr = *msg.Error
	}
	s.emit(PatchEndEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		CallID:     msg.CallID,
		TurnID:     msg.TurnID,
		Changes:    msg.Changes,
		Success:    msg.Success,
		Error:      errStr,
	})
}

func (s *Session) handleMCPToolCallBegin(msg *codexprotocol.McpToolCallBeginEvent) {
	var args map[string]interface{}
	if m, ok := msg.Invocation.Arguments.(map[string]interface{}); ok {
		args = m
	}
	s.emit(MCPToolStartEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		CallID:     msg.CallID,
		ServerName: msg.Invocation.Server,
		ToolName:   msg.Invocation.Tool,
		Arguments:  args,
		Timestamp:  time.Now(),
	})
}

func (s *Session) handleMCPToolCallEnd(msg *codexprotocol.McpToolCallEndEvent) {
	s.emit(MCPToolEndEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		CallID:     msg.CallID,
		ServerName: msg.Invocation.Server,
		ToolName:   msg.Invocation.Tool,
		Duration:   0, // Duration is a string in protocol, need to parse
		Result:     msg.Result,
		Timestamp:  time.Now(),
	})
}

func (s *Session) handleError(msg *codexprotocol.ErrorEvent) {
	turnNumber := s.turnManager.CurrentTurnNumber()

	// Emit error event
	s.emit(ErrorEvent{
		TurnNumber: turnNumber,
		Error:      fmt.Errorf("%s", msg.Message),
		Context:    "agent_error",
		ErrorInfo:  msg.CodexErrorInfo,
	})

	// If this is a turn-ending error, complete the turn
	if msg.CodexErrorInfo != nil {
		turn := s.turnManager.CurrentTurn()
		durationMs := int64(0)
		if turn != nil {
			durationMs = time.Since(turn.StartTime).Milliseconds()
		}

		result := TurnResult{
			TurnNumber: turnNumber,
			Success:    false,
			DurationMs: durationMs,
			Error:      fmt.Errorf("%s", msg.Message),
		}

		// Transition back to ready state
		_ = s.state.Transition(TransitionTurnComplete)

		// Complete turn (notifies waiters)
		s.turnManager.CompleteTurn(result)
	}
}

func (s *Session) handleWarning(msg *codexprotocol.WarningEvent) {
	s.emit(WarningEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		Message:    msg.Message,
	})
}

func (s *Session) handleTokenCount(msg *codexprotocol.TokenCountEvent) {
	s.emit(TokenCountEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		Info:       msg.Info,
		RateLimits: msg.RateLimits,
	})
}

func (s *Session) handleShutdownComplete(msg *codexprotocol.ShutdownCompleteEvent) {
	// Session is shutting down
	_ = s.state.Transition(TransitionClosed)
}

// emit sends an event to the events channel.
func (s *Session) emit(event Event) {
	select {
	case s.events <- event:
	default:
		// Channel full, drop event
		// In production, might want to log this
	}
}

// emitError emits an error event.
func (s *Session) emitError(err error, context string, info *codexprotocol.CodexErrorInfo) {
	s.emit(ErrorEvent{
		TurnNumber: s.turnManager.CurrentTurnNumber(),
		Error:      err,
		Context:    context,
		ErrorInfo:  info,
	})
}

// generateRequestID generates a unique request ID.
func generateRequestID() string {
	b := make([]byte, 8)
	rand.Read(b)
	return fmt.Sprintf("req_%d_%s", time.Now().UnixNano(), hex.EncodeToString(b))
}
