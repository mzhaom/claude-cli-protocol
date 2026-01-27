package integration

import (
	"context"
	"testing"

	"github.com/mzhaom/claude-cli-protocol/multiagent/testutil"
)

func TestMockLongRunningSession_Integration(t *testing.T) {
	// Test mock session works correctly for integration testing
	config := testutil.MockSessionConfig{
		SessionDir: "/tmp/test-integration",
		Responses: []testutil.MockResponse{
			{Text: "Response 1", Cost: 0.01, Success: true},
			{Text: "Response 2", Cost: 0.02, Success: true},
			{Text: "Response 3", Cost: 0.03, Success: true},
		},
	}

	session := testutil.NewMockLongRunningSession(config)

	// Start
	if err := session.Start(context.Background()); err != nil {
		t.Fatalf("Start() error: %v", err)
	}

	// Send multiple messages
	for i := 0; i < 3; i++ {
		result, err := session.SendMessage(context.Background(), "test message")
		if err != nil {
			t.Fatalf("SendMessage(%d) error: %v", i, err)
		}
		if !result.Success {
			t.Errorf("expected success on message %d", i)
		}
	}

	// Verify metrics
	if session.TurnCount() != 3 {
		t.Errorf("expected turn count 3, got %d", session.TurnCount())
	}

	expectedCost := 0.01 + 0.02 + 0.03
	if session.TotalCost() != expectedCost {
		t.Errorf("expected total cost %v, got %v", expectedCost, session.TotalCost())
	}

	// Verify messages recorded
	messages := session.Messages()
	if len(messages) != 3 {
		t.Errorf("expected 3 messages recorded, got %d", len(messages))
	}

	// Stop
	if err := session.Stop(); err != nil {
		t.Fatalf("Stop() error: %v", err)
	}
}

func TestMockEphemeralSession_Integration(t *testing.T) {
	config := testutil.MockSessionConfig{
		SessionDir: "/tmp/test-ephemeral-integration",
		Responses: []testutil.MockResponse{
			{Text: testutil.SampleDesignResponseJSON, Cost: 0.01, Success: true},
			{Text: testutil.SampleBuildResponseJSON, Cost: 0.02, Success: true},
			{Text: testutil.SampleReviewResponsePassJSON, Cost: 0.005, Success: true},
		},
	}

	session := testutil.NewMockEphemeralSession(config)

	// Execute multiple tasks
	taskIDs := make([]string, 3)
	prompts := []string{"design", "build", "review"}

	for i, prompt := range prompts {
		result, taskID, err := session.Execute(context.Background(), prompt)
		if err != nil {
			t.Fatalf("Execute(%d) error: %v", i, err)
		}
		if !result.Success {
			t.Errorf("expected success on task %d", i)
		}
		taskIDs[i] = taskID
	}

	// Verify task IDs are sequential
	expected := []string{"task-001", "task-002", "task-003"}
	for i, taskID := range taskIDs {
		if taskID != expected[i] {
			t.Errorf("expected task ID %s, got %s", expected[i], taskID)
		}
	}

	// Verify metrics
	if session.TaskCount() != 3 {
		t.Errorf("expected task count 3, got %d", session.TaskCount())
	}

	expectedCost := 0.01 + 0.02 + 0.005
	actualCost := session.TotalCost()
	// Use approximate comparison for floating point
	if actualCost < expectedCost-0.001 || actualCost > expectedCost+0.001 {
		t.Errorf("expected total cost %v, got %v", expectedCost, actualCost)
	}

	// Verify prompts recorded
	recordedPrompts := session.Prompts()
	if len(recordedPrompts) != 3 {
		t.Errorf("expected 3 prompts recorded, got %d", len(recordedPrompts))
	}
}

func TestMockResponses_Fixtures(t *testing.T) {
	// Test standard mock responses
	responses := testutil.StandardMockResponses()
	if len(responses) != 3 {
		t.Errorf("expected 3 standard responses, got %d", len(responses))
	}

	// Test failure iteration responses
	failResponses := testutil.FailureIterationMockResponses()
	if len(failResponses) != 5 {
		t.Errorf("expected 5 failure iteration responses, got %d", len(failResponses))
	}
}
