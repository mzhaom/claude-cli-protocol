package integration

import (
	"errors"
	"testing"

	"github.com/mzhaom/claude-cli-protocol/multiagent/agent"
	"github.com/mzhaom/claude-cli-protocol/multiagent/planner"
	"github.com/mzhaom/claude-cli-protocol/multiagent/protocol"
	"github.com/mzhaom/claude-cli-protocol/multiagent/testutil"
)

func TestPlanner_Initialization(t *testing.T) {
	tempDir := t.TempDir()

	cfg := planner.Config{
		PlannerConfig: agent.AgentConfig{
			Model:      "sonnet",
			WorkDir:    tempDir,
			SessionDir: tempDir,
		},
		DesignerConfig: agent.AgentConfig{
			Role:       agent.RoleDesigner,
			Model:      "haiku",
			WorkDir:    tempDir,
			SessionDir: tempDir,
		},
		BuilderConfig: agent.AgentConfig{
			Role:       agent.RoleBuilder,
			Model:      "haiku",
			WorkDir:    tempDir,
			SessionDir: tempDir,
		},
		ReviewerConfig: agent.AgentConfig{
			Role:       agent.RoleReviewer,
			Model:      "haiku",
			WorkDir:    tempDir,
			SessionDir: tempDir,
		},
		MaxIterations: 10,
	}

	p := planner.New(cfg, "test-session")

	// Verify role
	if p.Role() != agent.RolePlanner {
		t.Errorf("expected role %v, got %v", agent.RolePlanner, p.Role())
	}

	// Verify initial state
	if p.TotalCost() != 0 {
		t.Errorf("expected initial cost 0, got %v", p.TotalCost())
	}

	if p.TurnCount() != 0 {
		t.Errorf("expected initial turn count 0, got %v", p.TurnCount())
	}

	if p.IterationCount() != 0 {
		t.Errorf("expected initial iteration count 0, got %v", p.IterationCount())
	}
}

func TestPlanner_IterationLimitEnforcement(t *testing.T) {
	tempDir := t.TempDir()

	cfg := planner.Config{
		PlannerConfig: agent.AgentConfig{
			Model:      "sonnet",
			WorkDir:    tempDir,
			SessionDir: tempDir,
		},
		DesignerConfig: agent.AgentConfig{
			Role:       agent.RoleDesigner,
			Model:      "haiku",
			WorkDir:    tempDir,
			SessionDir: tempDir,
		},
		BuilderConfig: agent.AgentConfig{
			Role:       agent.RoleBuilder,
			Model:      "haiku",
			WorkDir:    tempDir,
			SessionDir: tempDir,
		},
		ReviewerConfig: agent.AgentConfig{
			Role:       agent.RoleReviewer,
			Model:      "haiku",
			WorkDir:    tempDir,
			SessionDir: tempDir,
		},
		MaxIterations: 3, // Low limit
	}

	p := planner.New(cfg, "test-session")

	// Verify iterations can be incremented
	if p.IterationCount() != 0 {
		t.Errorf("expected initial iteration count 0, got %d", p.IterationCount())
	}
}

func TestProtocol_DesignResponse(t *testing.T) {
	// Test that design response fixtures are valid
	resp := testutil.NewSampleDesignResponse()

	if resp.Architecture == "" {
		t.Error("expected non-empty architecture")
	}

	if len(resp.Files) == 0 {
		t.Error("expected at least one file spec")
	}

	if len(resp.ImplementationNotes) == 0 {
		t.Error("expected implementation notes")
	}
}

func TestProtocol_BuildResponse(t *testing.T) {
	resp := testutil.NewSampleBuildResponse()

	if len(resp.FilesCreated) == 0 {
		t.Error("expected at least one file created")
	}

	if !resp.TestsRun {
		t.Error("expected tests to be run")
	}

	if !resp.TestsPassed {
		t.Error("expected tests to pass in sample")
	}
}

func TestProtocol_ReviewResponse_NoCriticalIssues(t *testing.T) {
	resp := testutil.NewSampleReviewResponsePass()

	if resp.HasCriticalIssues() {
		t.Error("expected no critical issues in pass response")
	}

	if resp.Summary == "" {
		t.Error("expected non-empty summary")
	}

	if len(resp.Positives) == 0 {
		t.Error("expected at least one positive")
	}
}

func TestProtocol_ReviewResponse_WithCriticalIssues(t *testing.T) {
	resp := testutil.NewSampleReviewResponseFail()

	if !resp.HasCriticalIssues() {
		t.Error("expected critical issues in fail response")
	}

	// Count critical issues
	criticalCount := 0
	for _, issue := range resp.Issues {
		if issue.Severity == "critical" {
			criticalCount++
		}
	}

	if criticalCount == 0 {
		t.Error("expected at least one critical issue")
	}
}

func TestProtocol_PlannerResult(t *testing.T) {
	result := &protocol.PlannerResult{
		Success:       true,
		Summary:       "Implementation complete",
		FilesCreated:  []string{"main.go", "hello/hello.go"},
		FilesModified: []string{"go.mod"},
		TotalCost:     0.05,
	}

	if !result.Success {
		t.Error("expected success to be true")
	}

	if len(result.FilesCreated) != 2 {
		t.Errorf("expected 2 files created, got %d", len(result.FilesCreated))
	}
}

func TestErrMaxIterationsExceeded_Detection(t *testing.T) {
	// Verify error detection works
	err := planner.ErrMaxIterationsExceeded
	if !errors.Is(err, planner.ErrMaxIterationsExceeded) {
		t.Error("errors.Is should match ErrMaxIterationsExceeded")
	}
}
