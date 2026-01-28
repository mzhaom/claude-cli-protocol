package codex

import (
	"context"
	"sync"
	"time"
)

// TurnUsage contains token usage for a turn.
type TurnUsage struct {
	InputTokens  int
	OutputTokens int
}

// TurnResult contains the result of a completed turn.
type TurnResult struct {
	TurnNumber       int
	TurnID           string
	Success          bool
	DurationMs       int64
	Usage            TurnUsage
	Error            error
	LastAgentMessage string
}

// turnState tracks the state of a single turn.
type turnState struct {
	Number       int
	TurnID       string
	UserMessage  interface{} // string or UserInput items
	StartTime    time.Time
	FullText     string
	FullReasoning string
	Commands     map[string]*commandState
}

// commandState tracks the state of a command within a turn.
type commandState struct {
	CallID    string
	TurnID    string
	Command   []string
	CWD       string
	ProcessID *int
	Stdout    []byte
	Stderr    []byte
	ExitCode  *int
	TimedOut  bool
	StartTime time.Time
}

// turnManager manages turn state and completion tracking.
type turnManager struct {
	mu                sync.RWMutex
	currentTurnNumber int
	currentTurn       *turnState
	turns             []*turnState
	completionWaiters map[int][]chan *TurnResult
}

// newTurnManager creates a new turn manager.
func newTurnManager() *turnManager {
	return &turnManager{
		turns:             make([]*turnState, 0),
		completionWaiters: make(map[int][]chan *TurnResult),
	}
}

// StartTurn starts a new turn with the given user message.
func (tm *turnManager) StartTurn(userMessage interface{}) *turnState {
	tm.mu.Lock()
	defer tm.mu.Unlock()

	tm.currentTurnNumber++
	turn := &turnState{
		Number:      tm.currentTurnNumber,
		UserMessage: userMessage,
		StartTime:   time.Now(),
		Commands:    make(map[string]*commandState),
	}
	tm.currentTurn = turn
	tm.turns = append(tm.turns, turn)
	return turn
}

// SetTurnID sets the turn ID from server response.
func (tm *turnManager) SetTurnID(turnID string) {
	tm.mu.Lock()
	defer tm.mu.Unlock()
	if tm.currentTurn != nil {
		tm.currentTurn.TurnID = turnID
	}
}

// CurrentTurnNumber returns the current turn number.
func (tm *turnManager) CurrentTurnNumber() int {
	tm.mu.RLock()
	defer tm.mu.RUnlock()
	return tm.currentTurnNumber
}

// CurrentTurn returns the current turn state.
func (tm *turnManager) CurrentTurn() *turnState {
	tm.mu.RLock()
	defer tm.mu.RUnlock()
	return tm.currentTurn
}

// AppendText appends text to the current turn.
func (tm *turnManager) AppendText(text string) string {
	tm.mu.Lock()
	defer tm.mu.Unlock()
	if tm.currentTurn != nil {
		tm.currentTurn.FullText += text
		return tm.currentTurn.FullText
	}
	return ""
}

// AppendReasoning appends reasoning to the current turn.
func (tm *turnManager) AppendReasoning(reasoning string) string {
	tm.mu.Lock()
	defer tm.mu.Unlock()
	if tm.currentTurn != nil {
		tm.currentTurn.FullReasoning += reasoning
		return tm.currentTurn.FullReasoning
	}
	return ""
}

// GetOrCreateCommand returns a command state by call ID, creating it if it doesn't exist.
func (tm *turnManager) GetOrCreateCommand(callID string, command []string, cwd string) *commandState {
	tm.mu.Lock()
	defer tm.mu.Unlock()
	if tm.currentTurn == nil {
		return nil
	}
	cmd, exists := tm.currentTurn.Commands[callID]
	if !exists {
		cmd = &commandState{
			CallID:    callID,
			Command:   command,
			CWD:       cwd,
			StartTime: time.Now(),
		}
		tm.currentTurn.Commands[callID] = cmd
	}
	return cmd
}

// GetCommand returns a command state by call ID.
func (tm *turnManager) GetCommand(callID string) *commandState {
	tm.mu.RLock()
	defer tm.mu.RUnlock()
	if tm.currentTurn == nil {
		return nil
	}
	return tm.currentTurn.Commands[callID]
}

// WaitForTurn waits for a specific turn to complete.
func (tm *turnManager) WaitForTurn(ctx context.Context, turnNumber int) (*TurnResult, error) {
	// Create a channel to receive the result
	resultChan := make(chan *TurnResult, 1)

	tm.mu.Lock()
	tm.completionWaiters[turnNumber] = append(tm.completionWaiters[turnNumber], resultChan)
	tm.mu.Unlock()

	select {
	case result := <-resultChan:
		if result.Error != nil {
			return nil, result.Error
		}
		return result, nil
	case <-ctx.Done():
		// Remove the waiter on cancellation
		tm.mu.Lock()
		waiters := tm.completionWaiters[turnNumber]
		for i, ch := range waiters {
			if ch == resultChan {
				tm.completionWaiters[turnNumber] = append(waiters[:i], waiters[i+1:]...)
				break
			}
		}
		tm.mu.Unlock()
		return nil, ctx.Err()
	}
}

// CompleteTurn marks a turn as complete and notifies waiters.
func (tm *turnManager) CompleteTurn(result TurnResult) {
	tm.mu.Lock()
	waiters := tm.completionWaiters[result.TurnNumber]
	delete(tm.completionWaiters, result.TurnNumber)
	tm.mu.Unlock()

	// Notify all waiters
	for _, ch := range waiters {
		ch <- &result
		close(ch)
	}
}

// GetTurnHistory returns all turns.
func (tm *turnManager) GetTurnHistory() []*turnState {
	tm.mu.RLock()
	defer tm.mu.RUnlock()
	// Return a copy to prevent races
	result := make([]*turnState, len(tm.turns))
	copy(result, tm.turns)
	return result
}
