package agent

import (
	"sync/atomic"
	"testing"
)

func TestNextTaskID(t *testing.T) {
	// Reset counter for test
	atomic.StoreUint64(&taskCounter, 0)

	id1 := nextTaskID()
	id2 := nextTaskID()
	id3 := nextTaskID()

	if id1 != "task-001" {
		t.Errorf("expected task-001, got %s", id1)
	}

	if id2 != "task-002" {
		t.Errorf("expected task-002, got %s", id2)
	}

	if id3 != "task-003" {
		t.Errorf("expected task-003, got %s", id3)
	}
}

func TestNewLongRunningSession(t *testing.T) {
	config := AgentConfig{
		Role:       RoleOrchestrator,
		Model:      "sonnet",
		WorkDir:    "/tmp/test",
		SessionDir: "/tmp/sessions",
	}

	session := NewLongRunningSession(config, "test-session-123")

	expectedDir := "/tmp/sessions/test-session-123/orchestrator"
	if session.SessionDir() != expectedDir {
		t.Errorf("SessionDir() = %s, want %s", session.SessionDir(), expectedDir)
	}

	if session.TotalCost() != 0 {
		t.Errorf("TotalCost() = %v, want 0", session.TotalCost())
	}

	if session.TurnCount() != 0 {
		t.Errorf("TurnCount() = %v, want 0", session.TurnCount())
	}
}

func TestNewEphemeralSession(t *testing.T) {
	config := AgentConfig{
		Role:       RoleDesigner,
		Model:      "haiku",
		WorkDir:    "/tmp/test",
		SessionDir: "/tmp/sessions",
	}

	session := NewEphemeralSession(config, "test-session-456")

	expectedDir := "/tmp/sessions/test-session-456/designer"
	if session.BaseSessionDir() != expectedDir {
		t.Errorf("BaseSessionDir() = %s, want %s", session.BaseSessionDir(), expectedDir)
	}

	if session.TotalCost() != 0 {
		t.Errorf("TotalCost() = %v, want 0", session.TotalCost())
	}

	if session.TaskCount() != 0 {
		t.Errorf("TaskCount() = %v, want 0", session.TaskCount())
	}
}

func TestLongRunningSessionNotStarted(t *testing.T) {
	config := AgentConfig{
		Role:       RolePlanner,
		Model:      "sonnet",
		WorkDir:    ".",
		SessionDir: "/tmp/sessions",
	}

	session := NewLongRunningSession(config, "test")

	// Recording should be nil before start
	if session.Recording() != nil {
		t.Error("Recording() should be nil before start")
	}

	// Events should be nil before start
	if session.Events() != nil {
		t.Error("Events() should be nil before start")
	}
}
