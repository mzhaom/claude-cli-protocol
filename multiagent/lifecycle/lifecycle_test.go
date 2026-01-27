package lifecycle

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	"github.com/mzhaom/claude-cli-protocol/multiagent/agent"
	"github.com/mzhaom/claude-cli-protocol/multiagent/orchestrator"
)

func TestLifecycle_SummaryGeneration(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "lifecycle-summary-test",
		WorkDir:           tempDir,
		SessionDir:        tempDir,
		OrchestratorModel: "sonnet",
		PlannerModel:      "sonnet",
		DesignerModel:     "haiku",
		BuilderModel:      "haiku",
		ReviewerModel:     "haiku",
		TotalBudgetUSD:    1.0,
		MaxIterations:     10,
	}

	orch, err := orchestrator.New(config)
	if err != nil {
		t.Fatalf("Failed to create orchestrator: %v", err)
	}

	// Write summary
	if err := orch.WriteSummary(); err != nil {
		t.Fatalf("WriteSummary() error: %v", err)
	}

	// Verify file exists
	summaryPath := filepath.Join(tempDir, "lifecycle-summary-test", "summary.json")
	if _, err := os.Stat(summaryPath); os.IsNotExist(err) {
		t.Fatalf("summary.json not created at %s", summaryPath)
	}
}

func TestLifecycle_SummaryContents(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "lifecycle-contents-test",
		WorkDir:           tempDir,
		SessionDir:        tempDir,
		OrchestratorModel: "sonnet",
		PlannerModel:      "sonnet",
		DesignerModel:     "haiku",
		BuilderModel:      "haiku",
		ReviewerModel:     "haiku",
		TotalBudgetUSD:    1.0,
		MaxIterations:     10,
	}

	orch, err := orchestrator.New(config)
	if err != nil {
		t.Fatalf("Failed to create orchestrator: %v", err)
	}

	// Write summary
	if err := orch.WriteSummary(); err != nil {
		t.Fatalf("WriteSummary() error: %v", err)
	}

	// Read and verify contents
	summaryPath := filepath.Join(tempDir, "lifecycle-contents-test", "summary.json")
	data, err := os.ReadFile(summaryPath)
	if err != nil {
		t.Fatalf("Failed to read summary: %v", err)
	}

	var summary orchestrator.Summary
	if err := json.Unmarshal(data, &summary); err != nil {
		t.Fatalf("Failed to parse summary: %v", err)
	}

	// Verify contents
	if summary.SessionID != "lifecycle-contents-test" {
		t.Errorf("expected session ID 'lifecycle-contents-test', got %q", summary.SessionID)
	}

	if summary.TotalCost < 0 {
		t.Error("expected non-negative total cost")
	}

	if summary.OrchestratorTurns < 0 {
		t.Error("expected non-negative orchestrator turns")
	}

	if summary.PlannerTurns < 0 {
		t.Error("expected non-negative planner turns")
	}
}

func TestLifecycle_AllSessionsStopped(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "lifecycle-stop-test",
		WorkDir:           tempDir,
		SessionDir:        tempDir,
		OrchestratorModel: "sonnet",
		PlannerModel:      "sonnet",
		DesignerModel:     "haiku",
		BuilderModel:      "haiku",
		ReviewerModel:     "haiku",
	}

	orch, err := orchestrator.New(config)
	if err != nil {
		t.Fatalf("Failed to create orchestrator: %v", err)
	}

	// Stop should not error even if not started
	if err := orch.Stop(); err != nil {
		t.Errorf("Stop() on unstarted orchestrator should not error: %v", err)
	}

	// Summary should still be writable
	if err := orch.WriteSummary(); err != nil {
		t.Errorf("WriteSummary() should succeed even after stop: %v", err)
	}
}

func TestLifecycle_SummaryWrittenOnNormalExit(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "lifecycle-exit-test",
		WorkDir:           tempDir,
		SessionDir:        tempDir,
		OrchestratorModel: "sonnet",
		PlannerModel:      "sonnet",
		DesignerModel:     "haiku",
		BuilderModel:      "haiku",
		ReviewerModel:     "haiku",
	}

	// Simulate normal exit flow
	orch, err := orchestrator.New(config)
	if err != nil {
		t.Fatalf("Failed to create orchestrator: %v", err)
	}

	// In main.go, this happens in defer
	_ = orch.Stop()
	_ = orch.WriteSummary()

	// Verify summary was written
	summaryPath := filepath.Join(tempDir, "lifecycle-exit-test", "summary.json")
	if _, err := os.Stat(summaryPath); os.IsNotExist(err) {
		t.Errorf("summary.json should be written on exit")
	}
}

func TestLifecycle_GracefulShutdownSequence(t *testing.T) {
	// This tests the shutdown sequence without actual signal handling
	// The actual signal handling is in cmd/swarm/main.go

	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "lifecycle-shutdown-test",
		WorkDir:           tempDir,
		SessionDir:        tempDir,
		OrchestratorModel: "sonnet",
		PlannerModel:      "sonnet",
		DesignerModel:     "haiku",
		BuilderModel:      "haiku",
		ReviewerModel:     "haiku",
	}

	orch, err := orchestrator.New(config)
	if err != nil {
		t.Fatalf("Failed to create orchestrator: %v", err)
	}

	// Simulate graceful shutdown sequence:
	// 1. Stop orchestrator
	if err := orch.Stop(); err != nil {
		t.Errorf("Stop() error: %v", err)
	}

	// 2. Write summary
	if err := orch.WriteSummary(); err != nil {
		t.Errorf("WriteSummary() error: %v", err)
	}

	// 3. Verify summary exists
	summaryPath := filepath.Join(tempDir, "lifecycle-shutdown-test", "summary.json")
	data, err := os.ReadFile(summaryPath)
	if err != nil {
		t.Fatalf("Failed to read summary: %v", err)
	}

	var summary orchestrator.Summary
	if err := json.Unmarshal(data, &summary); err != nil {
		t.Fatalf("Failed to parse summary: %v", err)
	}

	if summary.SessionID != "lifecycle-shutdown-test" {
		t.Errorf("expected session ID 'lifecycle-shutdown-test', got %q", summary.SessionID)
	}
}
