package codex

import (
	"context"
	"testing"
	"time"
)

func TestTurnManager_StartTurn(t *testing.T) {
	tm := newTurnManager()

	// First turn
	turn1 := tm.StartTurn("Hello")
	if turn1.Number != 1 {
		t.Errorf("Expected turn number 1, got %d", turn1.Number)
	}
	if tm.CurrentTurnNumber() != 1 {
		t.Errorf("Expected current turn number 1, got %d", tm.CurrentTurnNumber())
	}

	// Second turn
	turn2 := tm.StartTurn("World")
	if turn2.Number != 2 {
		t.Errorf("Expected turn number 2, got %d", turn2.Number)
	}
	if tm.CurrentTurnNumber() != 2 {
		t.Errorf("Expected current turn number 2, got %d", tm.CurrentTurnNumber())
	}
}

func TestTurnManager_AppendText(t *testing.T) {
	tm := newTurnManager()
	tm.StartTurn("test")

	full := tm.AppendText("Hello ")
	if full != "Hello " {
		t.Errorf("Expected 'Hello ', got %q", full)
	}

	full = tm.AppendText("World")
	if full != "Hello World" {
		t.Errorf("Expected 'Hello World', got %q", full)
	}

	turn := tm.CurrentTurn()
	if turn.FullText != "Hello World" {
		t.Errorf("Expected FullText 'Hello World', got %q", turn.FullText)
	}
}

func TestTurnManager_AppendReasoning(t *testing.T) {
	tm := newTurnManager()
	tm.StartTurn("test")

	full := tm.AppendReasoning("Thinking... ")
	if full != "Thinking... " {
		t.Errorf("Expected 'Thinking... ', got %q", full)
	}

	full = tm.AppendReasoning("Done")
	if full != "Thinking... Done" {
		t.Errorf("Expected 'Thinking... Done', got %q", full)
	}
}

func TestTurnManager_Commands(t *testing.T) {
	tm := newTurnManager()
	tm.StartTurn("test")

	cmd1 := tm.GetOrCreateCommand("call-1", []string{"ls", "-la"}, "/tmp")
	if cmd1 == nil {
		t.Fatal("Expected command, got nil")
	}
	if cmd1.CallID != "call-1" {
		t.Errorf("Expected CallID 'call-1', got %q", cmd1.CallID)
	}

	// Get same command
	cmd2 := tm.GetCommand("call-1")
	if cmd2 != cmd1 {
		t.Error("Expected same command instance")
	}

	// Get non-existent command
	cmd3 := tm.GetCommand("call-2")
	if cmd3 != nil {
		t.Error("Expected nil for non-existent command")
	}
}

func TestTurnManager_WaitForTurn(t *testing.T) {
	tm := newTurnManager()
	tm.StartTurn("test")

	// Start waiting in background
	resultChan := make(chan *TurnResult, 1)
	errChan := make(chan error, 1)
	go func() {
		ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
		defer cancel()
		result, err := tm.WaitForTurn(ctx, 1)
		if err != nil {
			errChan <- err
		} else {
			resultChan <- result
		}
	}()

	// Give goroutine time to start waiting
	time.Sleep(50 * time.Millisecond)

	// Complete the turn
	tm.CompleteTurn(TurnResult{
		TurnNumber: 1,
		Success:    true,
		DurationMs: 100,
	})

	// Wait for result
	select {
	case result := <-resultChan:
		if result.TurnNumber != 1 {
			t.Errorf("Expected turn number 1, got %d", result.TurnNumber)
		}
		if !result.Success {
			t.Error("Expected success")
		}
	case err := <-errChan:
		t.Fatalf("Unexpected error: %v", err)
	case <-time.After(500 * time.Millisecond):
		t.Fatal("Timed out waiting for result")
	}
}

func TestTurnManager_WaitForTurn_Timeout(t *testing.T) {
	tm := newTurnManager()
	tm.StartTurn("test")

	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()

	_, err := tm.WaitForTurn(ctx, 1)
	if err == nil {
		t.Fatal("Expected timeout error")
	}
	if err != context.DeadlineExceeded {
		t.Errorf("Expected DeadlineExceeded, got %v", err)
	}
}
