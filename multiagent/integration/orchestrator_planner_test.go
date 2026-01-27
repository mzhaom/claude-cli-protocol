package integration

import (
	"errors"
	"testing"

	"github.com/mzhaom/claude-cli-protocol/multiagent/agent"
	"github.com/mzhaom/claude-cli-protocol/multiagent/orchestrator"
)

func TestOrchestrator_Initialization(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "test-init",
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
		t.Fatalf("New() error: %v", err)
	}

	// Verify session ID is set
	if orch.SessionID() != "test-init" {
		t.Errorf("expected session ID 'test-init', got %q", orch.SessionID())
	}

	// Verify role
	if orch.Role() != agent.RoleOrchestrator {
		t.Errorf("expected role %v, got %v", agent.RoleOrchestrator, orch.Role())
	}

	// Verify initial costs are zero
	if orch.TotalCost() != 0 {
		t.Errorf("expected initial cost 0, got %v", orch.TotalCost())
	}

	// Verify turn count starts at zero
	if orch.TurnCount() != 0 {
		t.Errorf("expected initial turn count 0, got %v", orch.TurnCount())
	}
}

func TestOrchestrator_BudgetEnforcementIntegration(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "test-budget-integration",
		WorkDir:           tempDir,
		SessionDir:        tempDir,
		OrchestratorModel: "sonnet",
		PlannerModel:      "sonnet",
		DesignerModel:     "haiku",
		BuilderModel:      "haiku",
		ReviewerModel:     "haiku",
		TotalBudgetUSD:    0.10, // Low budget
		MaxIterations:     10,
	}

	orch, err := orchestrator.New(config)
	if err != nil {
		t.Fatalf("New() error: %v", err)
	}

	// Get summary before any cost
	summary1 := orch.GetSummary()
	if summary1.TotalCost != 0 {
		t.Errorf("expected initial summary cost 0, got %v", summary1.TotalCost)
	}

	// Verify budget is reflected in config
	if config.TotalBudgetUSD != 0.10 {
		t.Errorf("expected budget 0.10, got %v", config.TotalBudgetUSD)
	}
}

func TestOrchestrator_SummaryGeneration(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "test-summary-gen",
		WorkDir:           tempDir,
		SessionDir:        tempDir,
		OrchestratorModel: "sonnet",
		PlannerModel:      "sonnet",
		DesignerModel:     "haiku",
		BuilderModel:      "haiku",
		ReviewerModel:     "haiku",
		TotalBudgetUSD:    1.0,
		MaxIterations:     50,
	}

	orch, err := orchestrator.New(config)
	if err != nil {
		t.Fatalf("New() error: %v", err)
	}

	summary := orch.GetSummary()

	// Verify summary structure
	if summary.SessionID != "test-summary-gen" {
		t.Errorf("expected session ID 'test-summary-gen', got %q", summary.SessionID)
	}

	if _, ok := summary.AgentCosts["orchestrator"]; !ok {
		t.Error("expected 'orchestrator' in AgentCosts")
	}

	if _, ok := summary.AgentCosts["planner"]; !ok {
		t.Error("expected 'planner' in AgentCosts")
	}

	// Write summary and verify
	if err := orch.WriteSummary(); err != nil {
		t.Fatalf("WriteSummary() error: %v", err)
	}
}

func TestOrchestrator_BudgetExceededError(t *testing.T) {
	// Verify that ErrBudgetExceeded can be detected with errors.Is
	err := orchestrator.ErrBudgetExceeded
	if !errors.Is(err, orchestrator.ErrBudgetExceeded) {
		t.Error("errors.Is should match ErrBudgetExceeded")
	}
}
