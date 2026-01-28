package codex

import "testing"

func TestSessionState_Transitions(t *testing.T) {
	s := newSessionState()

	// Initial state
	if s.Current() != StateUninitialized {
		t.Errorf("Expected StateUninitialized, got %v", s.Current())
	}

	// Start
	if err := s.Transition(TransitionStarted); err != nil {
		t.Fatalf("TransitionStarted failed: %v", err)
	}
	if s.Current() != StateStarting {
		t.Errorf("Expected StateStarting, got %v", s.Current())
	}

	// Session configured
	if err := s.Transition(TransitionSessionConfigured); err != nil {
		t.Fatalf("TransitionSessionConfigured failed: %v", err)
	}
	if s.Current() != StateReady {
		t.Errorf("Expected StateReady, got %v", s.Current())
	}
	if !s.IsReady() {
		t.Error("Expected IsReady to be true")
	}

	// User message sent
	if err := s.Transition(TransitionUserMessageSent); err != nil {
		t.Fatalf("TransitionUserMessageSent failed: %v", err)
	}
	if s.Current() != StateProcessing {
		t.Errorf("Expected StateProcessing, got %v", s.Current())
	}
	if !s.IsProcessing() {
		t.Error("Expected IsProcessing to be true")
	}

	// Turn complete
	if err := s.Transition(TransitionTurnComplete); err != nil {
		t.Fatalf("TransitionTurnComplete failed: %v", err)
	}
	if s.Current() != StateReady {
		t.Errorf("Expected StateReady, got %v", s.Current())
	}

	// Close
	if err := s.Transition(TransitionClosed); err != nil {
		t.Fatalf("TransitionClosed failed: %v", err)
	}
	if s.Current() != StateClosed {
		t.Errorf("Expected StateClosed, got %v", s.Current())
	}
	if !s.IsClosed() {
		t.Error("Expected IsClosed to be true")
	}
}

func TestSessionState_InvalidTransition(t *testing.T) {
	s := newSessionState()

	// Can't transition from Uninitialized directly to Ready
	err := s.Transition(TransitionSessionConfigured)
	if err == nil {
		t.Fatal("Expected error for invalid transition")
	}
}

func TestSessionState_CloseFromAnyState(t *testing.T) {
	tests := []struct {
		name  string
		setup func(*sessionState)
	}{
		{
			name:  "from uninitialized",
			setup: func(s *sessionState) {},
		},
		{
			name: "from starting",
			setup: func(s *sessionState) {
				s.Transition(TransitionStarted)
			},
		},
		{
			name: "from ready",
			setup: func(s *sessionState) {
				s.Transition(TransitionStarted)
				s.Transition(TransitionSessionConfigured)
			},
		},
		{
			name: "from processing",
			setup: func(s *sessionState) {
				s.Transition(TransitionStarted)
				s.Transition(TransitionSessionConfigured)
				s.Transition(TransitionUserMessageSent)
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s := newSessionState()
			tt.setup(s)

			if err := s.Transition(TransitionClosed); err != nil {
				t.Errorf("TransitionClosed failed: %v", err)
			}
			if s.Current() != StateClosed {
				t.Errorf("Expected StateClosed, got %v", s.Current())
			}
		})
	}
}
