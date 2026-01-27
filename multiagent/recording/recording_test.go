package recording

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	"github.com/mzhaom/claude-cli-protocol/multiagent/agent"
	"github.com/mzhaom/claude-cli-protocol/multiagent/orchestrator"
)

// TestRecording_DirectoryStructure verifies the expected directory structure is created.
func TestRecording_DirectoryStructure(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "recording-test",
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

	_, err := orchestrator.New(config)
	if err != nil {
		t.Fatalf("Failed to create orchestrator: %v", err)
	}

	// Verify session directory was created
	sessionDir := filepath.Join(tempDir, "recording-test")
	if _, err := os.Stat(sessionDir); os.IsNotExist(err) {
		t.Errorf("session directory not created: %s", sessionDir)
	}
}

// TestRecording_SummaryFileFormat verifies summary.json has correct format.
func TestRecording_SummaryFileFormat(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "summary-format-test",
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
		t.Fatalf("Failed to write summary: %v", err)
	}

	// Read and validate summary file
	summaryPath := filepath.Join(tempDir, "summary-format-test", "summary.json")

	data, err := os.ReadFile(summaryPath)
	if err != nil {
		t.Fatalf("Failed to read summary file: %v", err)
	}

	// Validate JSON structure
	var summary orchestrator.Summary
	if err := json.Unmarshal(data, &summary); err != nil {
		t.Fatalf("Failed to parse summary JSON: %v", err)
	}

	// Validate required fields
	if summary.SessionID != "summary-format-test" {
		t.Errorf("expected session_id 'summary-format-test', got %q", summary.SessionID)
	}

	if summary.AgentCosts == nil {
		t.Error("expected agent_costs to be present")
	}

	if _, ok := summary.AgentCosts["orchestrator"]; !ok {
		t.Error("expected 'orchestrator' in agent_costs")
	}

	if _, ok := summary.AgentCosts["planner"]; !ok {
		t.Error("expected 'planner' in agent_costs")
	}
}

// TestRecording_LongRunningAgentPaths verifies paths for orchestrator/planner.
func TestRecording_LongRunningAgentPaths(t *testing.T) {
	tempDir := t.TempDir()

	config := agent.SwarmConfig{
		SessionID:         "longrunning-path-test",
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

	// Verify orchestrator session dir follows pattern
	orchSessionDir := orch.SessionDir()
	expectedOrchestratorPath := filepath.Join(tempDir, "longrunning-path-test", "orchestrator")

	if orchSessionDir != expectedOrchestratorPath {
		t.Errorf("expected orchestrator session dir %q, got %q", expectedOrchestratorPath, orchSessionDir)
	}
}

// TestRecording_EphemeralAgentPaths verifies task-XXX directory naming.
func TestRecording_EphemeralAgentPaths(t *testing.T) {
	tempDir := t.TempDir()

	// Create ephemeral session config
	config := agent.AgentConfig{
		Role:       agent.RoleDesigner,
		Model:      "haiku",
		WorkDir:    tempDir,
		SessionDir: tempDir,
	}

	session := agent.NewEphemeralSession(config, "ephemeral-path-test")

	// Verify base session dir follows pattern
	expectedBasePath := filepath.Join(tempDir, "ephemeral-path-test", "designer")
	if session.BaseSessionDir() != expectedBasePath {
		t.Errorf("expected base session dir %q, got %q", expectedBasePath, session.BaseSessionDir())
	}
}

// TestRecording_JSONValidation validates JSON can be parsed correctly.
func TestRecording_JSONValidation(t *testing.T) {
	// Test that summary JSON is valid
	summaryJSON := `{
		"session_id": "test-123",
		"total_cost": 0.05,
		"orchestrator_turns": 2,
		"planner_turns": 5,
		"agent_costs": {
			"orchestrator": 0.02,
			"planner": 0.03
		}
	}`

	var summary orchestrator.Summary
	if err := json.Unmarshal([]byte(summaryJSON), &summary); err != nil {
		t.Fatalf("Failed to parse test JSON: %v", err)
	}

	if summary.SessionID != "test-123" {
		t.Errorf("expected session_id 'test-123', got %q", summary.SessionID)
	}

	if summary.TotalCost != 0.05 {
		t.Errorf("expected total_cost 0.05, got %v", summary.TotalCost)
	}

	if summary.OrchestratorTurns != 2 {
		t.Errorf("expected orchestrator_turns 2, got %d", summary.OrchestratorTurns)
	}

	if summary.PlannerTurns != 5 {
		t.Errorf("expected planner_turns 5, got %d", summary.PlannerTurns)
	}
}
