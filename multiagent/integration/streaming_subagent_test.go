package integration

import (
	"context"
	"errors"
	"sync"
	"testing"
	"time"

	"github.com/mzhaom/claude-cli-protocol/multiagent/subagent"
	"github.com/mzhaom/claude-cli-protocol/multiagent/testutil"
)

// =============================================================================
// Happy Path Tests
// =============================================================================

func TestStreamingSubAgent_DesignerSuccessResult(t *testing.T) {
	result := testutil.NewSampleDesignResult()

	if !result.Success {
		t.Error("expected success to be true")
	}
	if result.Agent != subagent.AgentTypeDesigner {
		t.Errorf("expected agent %q, got %q", subagent.AgentTypeDesigner, result.Agent)
	}
	if result.Design == nil {
		t.Error("expected Design to be non-nil")
	}
	if result.Design.Architecture == "" {
		t.Error("expected non-empty Architecture")
	}
}

func TestStreamingSubAgent_BuilderSuccessResult(t *testing.T) {
	result := testutil.NewSampleBuildResult()

	if !result.Success {
		t.Error("expected success to be true")
	}
	if result.Agent != subagent.AgentTypeBuilder {
		t.Errorf("expected agent %q, got %q", subagent.AgentTypeBuilder, result.Agent)
	}
	if result.Build == nil {
		t.Error("expected Build to be non-nil")
	}
	if len(result.FilesCreated) == 0 {
		t.Error("expected non-empty FilesCreated")
	}
}

func TestStreamingSubAgent_ReviewerSuccessResult(t *testing.T) {
	result := testutil.NewSampleReviewResult()

	if !result.Success {
		t.Error("expected success to be true")
	}
	if result.Agent != subagent.AgentTypeReviewer {
		t.Errorf("expected agent %q, got %q", subagent.AgentTypeReviewer, result.Agent)
	}
	if result.Review == nil {
		t.Error("expected Review to be non-nil")
	}
	if result.Review.HasCriticalIssues() {
		t.Error("expected no critical issues in sample pass result")
	}
}

// =============================================================================
// Event Streaming Tests
// =============================================================================

func TestStreamingSubAgent_ProgressEventsOrder(t *testing.T) {
	events := testutil.NewSampleProgressEvents()

	if len(events) < 3 {
		t.Fatalf("expected at least 3 events, got %d", len(events))
	}

	// Verify expected order: thinking -> tool_call -> streaming
	expectedPhases := []subagent.Phase{
		subagent.PhaseThinking,
		subagent.PhaseToolCall,
		subagent.PhaseStreaming,
	}

	for i, expected := range expectedPhases {
		if events[i].Phase != expected {
			t.Errorf("event %d: expected phase %q, got %q", i, expected, events[i].Phase)
		}
	}
}

func TestStreamingSubAgent_FileEventTracking(t *testing.T) {
	events := testutil.NewSampleFileEvents()

	if len(events) < 3 {
		t.Fatalf("expected at least 3 file events, got %d", len(events))
	}

	// First two should be creates, third should be modify
	if events[0].Action != subagent.FileActionCreate {
		t.Errorf("event 0: expected action %q, got %q", subagent.FileActionCreate, events[0].Action)
	}
	if events[1].Action != subagent.FileActionCreate {
		t.Errorf("event 1: expected action %q, got %q", subagent.FileActionCreate, events[1].Action)
	}
	if events[2].Action != subagent.FileActionModify {
		t.Errorf("event 2: expected action %q, got %q", subagent.FileActionModify, events[2].Action)
	}
}

func TestStreamingSubAgent_CostUpdateAccumulation(t *testing.T) {
	costUpdate := testutil.NewSampleCostUpdate()

	if costUpdate.TurnCostUSD <= 0 {
		t.Error("expected positive turn cost")
	}
	if costUpdate.TotalCostUSD <= 0 {
		t.Error("expected positive total cost")
	}
	if costUpdate.InputTokens <= 0 {
		t.Error("expected positive input tokens")
	}
	if costUpdate.OutputTokens <= 0 {
		t.Error("expected positive output tokens")
	}
}

// =============================================================================
// Error Handling Tests
// =============================================================================

func TestMockStreamingSubAgent_ExecutionError(t *testing.T) {
	expectedErr := errors.New("simulated execution error")
	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		Error: expectedErr,
	})

	_, err := mock.Execute(context.Background(), "test prompt")

	if err != expectedErr {
		t.Errorf("expected error %v, got %v", expectedErr, err)
	}
	if !mock.WasExecuted() {
		t.Error("expected Execute to be called")
	}
}

func TestMockStreamingSubAgent_EmptyResult(t *testing.T) {
	// Simulate empty response from Claude
	result := &subagent.Result{
		Type:      subagent.MessageTypeResult,
		RequestID: "req-1",
		Agent:     subagent.AgentTypeDesigner,
		Success:   true,
		Text:      "", // Empty text
	}

	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		Result: result,
	})

	got, err := mock.Execute(context.Background(), "test")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if got.Text != "" {
		t.Errorf("expected empty text, got %q", got.Text)
	}
	if got.Design != nil {
		t.Error("expected nil Design for empty text")
	}
}

func TestMockStreamingSubAgent_MalformedResponse(t *testing.T) {
	// Simulate response with text but failed parsing
	result := &subagent.Result{
		Type:      subagent.MessageTypeResult,
		RequestID: "req-1",
		Agent:     subagent.AgentTypeDesigner,
		Success:   true,
		Text:      "not valid json {{{",
		Design:    nil, // Parsing failed
	}

	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		Result: result,
	})

	got, err := mock.Execute(context.Background(), "test")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Text should still be populated even if parsing fails
	if got.Text == "" {
		t.Error("expected text to be populated")
	}
	if got.Design != nil {
		t.Error("expected Design to be nil when parsing fails")
	}
}

// =============================================================================
// Cancellation Tests
// =============================================================================

func TestMockStreamingSubAgent_Cancel(t *testing.T) {
	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		ExecuteDelay: 1 * time.Second, // Long delay
	})

	// Start execution in background
	ctx, cancel := context.WithCancel(context.Background())
	var wg sync.WaitGroup
	wg.Add(1)
	var execErr error
	go func() {
		defer wg.Done()
		_, execErr = mock.Execute(ctx, "test")
	}()

	// Cancel after a short delay
	time.Sleep(50 * time.Millisecond)
	cancel()
	wg.Wait()

	if execErr != context.Canceled {
		t.Errorf("expected context.Canceled, got %v", execErr)
	}
}

func TestMockStreamingSubAgent_CancelBeforeStart(t *testing.T) {
	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{})

	// Cancel before Execute
	mock.Cancel("preemptive cancel")

	if !mock.IsCancelled() {
		t.Error("expected IsCancelled() to return true")
	}

	// Drain the error event
	select {
	case event := <-mock.Events():
		errEvent, ok := event.(*subagent.Error)
		if !ok {
			t.Fatalf("expected Error event, got %T", event)
		}
		if errEvent.Code != "cancelled" {
			t.Errorf("expected code 'cancelled', got %q", errEvent.Code)
		}
	case <-time.After(100 * time.Millisecond):
		t.Error("expected error event after cancel")
	}
}

func TestMockStreamingSubAgent_CancelIdempotent(t *testing.T) {
	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{})

	// Multiple cancels should not panic
	mock.Cancel("first")
	mock.Cancel("second")
	mock.Cancel("third")

	if !mock.IsCancelled() {
		t.Error("expected IsCancelled() to return true")
	}
}

// =============================================================================
// Timeout Tests
// =============================================================================

func TestMockStreamingSubAgent_ContextTimeout(t *testing.T) {
	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		ExecuteDelay: 5 * time.Second, // Long delay
	})

	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	_, err := mock.Execute(ctx, "test")

	if err != context.DeadlineExceeded {
		t.Errorf("expected context.DeadlineExceeded, got %v", err)
	}
}

// =============================================================================
// Mock Infrastructure Tests
// =============================================================================

func TestMockStreamingSubAgent_EventsEmitted(t *testing.T) {
	progressEvent := subagent.NewProgress("req-1", subagent.AgentTypeBuilder, subagent.PhaseThinking)
	fileEvent := subagent.NewFileEvent("req-1", subagent.AgentTypeBuilder, "/tmp/test.go", subagent.FileActionCreate, "Write")

	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		Result: testutil.NewSampleBuildResult(),
		Events: []interface{}{progressEvent, fileEvent},
	})

	var collectedEvents []interface{}
	done := make(chan struct{})

	go func() {
		for event := range mock.Events() {
			collectedEvents = append(collectedEvents, event)
		}
		close(done)
	}()

	_, err := mock.Execute(context.Background(), "test")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	<-done

	if len(collectedEvents) != 2 {
		t.Errorf("expected 2 events, got %d", len(collectedEvents))
	}
}

func TestMockStreamingSubAgent_PromptRecorded(t *testing.T) {
	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		Result: testutil.NewSampleDesignResult(),
	})

	testPrompt := "Create a hello world application"
	mock.Execute(context.Background(), testPrompt)

	if mock.LastPrompt() != testPrompt {
		t.Errorf("expected prompt %q, got %q", testPrompt, mock.LastPrompt())
	}
}

func TestMockClaudeSession_Lifecycle(t *testing.T) {
	events := testutil.NewSampleClaudeEvents()
	mock := testutil.NewMockClaudeSession(testutil.MockClaudeSessionConfig{
		Events: events,
	})

	// Start
	if err := mock.Start(context.Background()); err != nil {
		t.Fatalf("Start error: %v", err)
	}
	if !mock.IsStarted() {
		t.Error("expected IsStarted() to return true")
	}

	// Emit events
	mock.EmitEvents()

	// Collect events
	var collected []interface{}
	for i := 0; i < len(events); i++ {
		select {
		case event := <-mock.Events():
			collected = append(collected, event)
		case <-time.After(100 * time.Millisecond):
			t.Fatal("timeout waiting for event")
		}
	}

	if len(collected) != len(events) {
		t.Errorf("expected %d events, got %d", len(events), len(collected))
	}

	// Stop
	if err := mock.Stop(); err != nil {
		t.Fatalf("Stop error: %v", err)
	}
	if !mock.IsStopped() {
		t.Error("expected IsStopped() to return true")
	}
}

// =============================================================================
// Edge Cases
// =============================================================================

func TestStreamingSubAgent_EventChannelFull(t *testing.T) {
	// Create a sub-agent and fill its channel
	// This tests that the implementation doesn't deadlock when channel is full
	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		Result: testutil.NewSampleDesignResult(),
	})

	// Don't drain events - let them potentially fill up
	_, err := mock.Execute(context.Background(), "test")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should complete without deadlock
}

func TestStreamingSubAgent_LargeTextResponse(t *testing.T) {
	// Create a result with large text
	largeText := ""
	for i := 0; i < 10000; i++ {
		largeText += "Lorem ipsum dolor sit amet. "
	}

	result := &subagent.Result{
		Type:      subagent.MessageTypeResult,
		RequestID: "req-1",
		Agent:     subagent.AgentTypeDesigner,
		Success:   true,
		Text:      largeText,
	}

	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		Result: result,
	})

	got, err := mock.Execute(context.Background(), "test")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(got.Text) != len(largeText) {
		t.Errorf("expected text length %d, got %d", len(largeText), len(got.Text))
	}
}

func TestStreamingSubAgent_RapidToolCalls(t *testing.T) {
	// Create many file events to simulate rapid tool calls
	var events []interface{}
	for i := 0; i < 100; i++ {
		events = append(events, subagent.NewFileEvent(
			"req-1",
			subagent.AgentTypeBuilder,
			"/tmp/file_"+string(rune('a'+i%26))+".go",
			subagent.FileActionCreate,
			"Write",
		))
	}

	mock := testutil.NewMockStreamingSubAgent(testutil.MockStreamingConfig{
		Result: testutil.NewSampleBuildResult(),
		Events: events,
	})

	var collected []interface{}
	done := make(chan struct{})

	go func() {
		for event := range mock.Events() {
			collected = append(collected, event)
		}
		close(done)
	}()

	_, err := mock.Execute(context.Background(), "test")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	<-done

	if len(collected) != 100 {
		t.Errorf("expected 100 events, got %d", len(collected))
	}
}

// =============================================================================
// Protocol Type Tests
// =============================================================================

func TestProtocol_NewProgress(t *testing.T) {
	progress := subagent.NewProgress("req-1", subagent.AgentTypeDesigner, subagent.PhaseThinking)

	if progress.Type != subagent.MessageTypeProgress {
		t.Errorf("expected type %q, got %q", subagent.MessageTypeProgress, progress.Type)
	}
	if progress.RequestID != "req-1" {
		t.Errorf("expected requestID 'req-1', got %q", progress.RequestID)
	}
	if progress.Agent != subagent.AgentTypeDesigner {
		t.Errorf("expected agent %q, got %q", subagent.AgentTypeDesigner, progress.Agent)
	}
	if progress.Phase != subagent.PhaseThinking {
		t.Errorf("expected phase %q, got %q", subagent.PhaseThinking, progress.Phase)
	}
	if progress.Timestamp.IsZero() {
		t.Error("expected non-zero timestamp")
	}
}

func TestProtocol_NewFileEvent(t *testing.T) {
	event := subagent.NewFileEvent("req-1", subagent.AgentTypeBuilder, "/tmp/test.go", subagent.FileActionCreate, "Write")

	if event.Type != subagent.MessageTypeFileEvent {
		t.Errorf("expected type %q, got %q", subagent.MessageTypeFileEvent, event.Type)
	}
	if event.Path != "/tmp/test.go" {
		t.Errorf("expected path '/tmp/test.go', got %q", event.Path)
	}
	if event.Action != subagent.FileActionCreate {
		t.Errorf("expected action %q, got %q", subagent.FileActionCreate, event.Action)
	}
}

func TestProtocol_NewCostUpdate(t *testing.T) {
	update := subagent.NewCostUpdate("req-1", subagent.AgentTypeDesigner, 0.01, 0.05, 100, 200)

	if update.Type != subagent.MessageTypeCostUpdate {
		t.Errorf("expected type %q, got %q", subagent.MessageTypeCostUpdate, update.Type)
	}
	if update.TurnCostUSD != 0.01 {
		t.Errorf("expected turn cost 0.01, got %f", update.TurnCostUSD)
	}
	if update.TotalCostUSD != 0.05 {
		t.Errorf("expected total cost 0.05, got %f", update.TotalCostUSD)
	}
	if update.InputTokens != 100 {
		t.Errorf("expected input tokens 100, got %d", update.InputTokens)
	}
	if update.OutputTokens != 200 {
		t.Errorf("expected output tokens 200, got %d", update.OutputTokens)
	}
}

func TestProtocol_NewResult(t *testing.T) {
	result := subagent.NewResult("req-1", subagent.AgentTypeBuilder, true, 1500)

	if result.Type != subagent.MessageTypeResult {
		t.Errorf("expected type %q, got %q", subagent.MessageTypeResult, result.Type)
	}
	if !result.Success {
		t.Error("expected success to be true")
	}
	if result.DurationMs != 1500 {
		t.Errorf("expected duration 1500, got %d", result.DurationMs)
	}
	if result.FilesCreated == nil {
		t.Error("expected FilesCreated to be initialized")
	}
	if result.FilesModified == nil {
		t.Error("expected FilesModified to be initialized")
	}
}

func TestProtocol_NewError(t *testing.T) {
	err := subagent.NewError("req-1", subagent.AgentTypeDesigner, "timeout", "operation timed out", true)

	if err.Type != subagent.MessageTypeError {
		t.Errorf("expected type %q, got %q", subagent.MessageTypeError, err.Type)
	}
	if err.Code != "timeout" {
		t.Errorf("expected code 'timeout', got %q", err.Code)
	}
	if err.Message != "operation timed out" {
		t.Errorf("expected message 'operation timed out', got %q", err.Message)
	}
	if !err.Retryable {
		t.Error("expected retryable to be true")
	}
}
