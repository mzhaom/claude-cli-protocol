package codex

import (
	"sync"
	"testing"
)

func TestClientState_String(t *testing.T) {
	tests := []struct {
		state    ClientState
		expected string
	}{
		{ClientStateUninitialized, "uninitialized"},
		{ClientStateStarting, "starting"},
		{ClientStateReady, "ready"},
		{ClientStateClosed, "closed"},
		{ClientState(99), "unknown"},
	}

	for _, tt := range tests {
		if got := tt.state.String(); got != tt.expected {
			t.Errorf("ClientState(%d).String() = %q, want %q", tt.state, got, tt.expected)
		}
	}
}

func TestThreadState_String(t *testing.T) {
	tests := []struct {
		state    ThreadState
		expected string
	}{
		{ThreadStateCreating, "creating"},
		{ThreadStateStarting, "starting"},
		{ThreadStateReady, "ready"},
		{ThreadStateProcessing, "processing"},
		{ThreadStateClosed, "closed"},
		{ThreadState(99), "unknown"},
	}

	for _, tt := range tests {
		if got := tt.state.String(); got != tt.expected {
			t.Errorf("ThreadState(%d).String() = %q, want %q", tt.state, got, tt.expected)
		}
	}
}

func TestClientStateManager_Transitions(t *testing.T) {
	m := newClientStateManager()

	// Initial state
	if m.Current() != ClientStateUninitialized {
		t.Errorf("expected initial state uninitialized, got %v", m.Current())
	}

	// SetStarting from uninitialized
	if err := m.SetStarting(); err != nil {
		t.Errorf("SetStarting failed: %v", err)
	}
	if m.Current() != ClientStateStarting {
		t.Errorf("expected starting, got %v", m.Current())
	}

	// SetStarting again should fail
	if err := m.SetStarting(); err == nil {
		t.Error("SetStarting should fail when not uninitialized")
	}

	// SetReady from starting
	if err := m.SetReady(); err != nil {
		t.Errorf("SetReady failed: %v", err)
	}
	if m.Current() != ClientStateReady {
		t.Errorf("expected ready, got %v", m.Current())
	}
	if !m.IsReady() {
		t.Error("IsReady should return true")
	}

	// SetReady again should fail
	if err := m.SetReady(); err == nil {
		t.Error("SetReady should fail when not starting")
	}

	// SetClosed
	m.SetClosed()
	if m.Current() != ClientStateClosed {
		t.Errorf("expected closed, got %v", m.Current())
	}
	if !m.IsClosed() {
		t.Error("IsClosed should return true")
	}
}

func TestClientStateManager_SetClosedFromAnyState(t *testing.T) {
	states := []struct {
		name  string
		setup func(*clientStateManager)
	}{
		{"uninitialized", func(m *clientStateManager) {}},
		{"starting", func(m *clientStateManager) { m.SetStarting() }},
		{"ready", func(m *clientStateManager) { m.SetStarting(); m.SetReady() }},
	}

	for _, tt := range states {
		t.Run(tt.name, func(t *testing.T) {
			m := newClientStateManager()
			tt.setup(m)
			m.SetClosed()
			if !m.IsClosed() {
				t.Error("SetClosed should work from any state")
			}
		})
	}
}

func TestThreadStateManager_Transitions(t *testing.T) {
	m := newThreadStateManager()

	// Initial state
	if m.Current() != ThreadStateCreating {
		t.Errorf("expected initial state creating, got %v", m.Current())
	}

	// SetStarting from creating
	if err := m.SetStarting(); err != nil {
		t.Errorf("SetStarting failed: %v", err)
	}
	if m.Current() != ThreadStateStarting {
		t.Errorf("expected starting, got %v", m.Current())
	}

	// SetReady from starting
	if err := m.SetReady(); err != nil {
		t.Errorf("SetReady failed: %v", err)
	}
	if m.Current() != ThreadStateReady {
		t.Errorf("expected ready, got %v", m.Current())
	}
	if !m.IsReady() {
		t.Error("IsReady should return true")
	}

	// SetProcessing from ready
	if err := m.SetProcessing(); err != nil {
		t.Errorf("SetProcessing failed: %v", err)
	}
	if m.Current() != ThreadStateProcessing {
		t.Errorf("expected processing, got %v", m.Current())
	}
	if !m.IsProcessing() {
		t.Error("IsProcessing should return true")
	}

	// SetReady from processing (turn completed)
	if err := m.SetReady(); err != nil {
		t.Errorf("SetReady from processing failed: %v", err)
	}
	if m.Current() != ThreadStateReady {
		t.Errorf("expected ready, got %v", m.Current())
	}

	// SetClosed
	m.SetClosed()
	if m.Current() != ThreadStateClosed {
		t.Errorf("expected closed, got %v", m.Current())
	}
	if !m.IsClosed() {
		t.Error("IsClosed should return true")
	}
}

func TestThreadStateManager_InvalidTransitions(t *testing.T) {
	m := newThreadStateManager()

	// SetProcessing from creating should fail
	if err := m.SetProcessing(); err == nil {
		t.Error("SetProcessing should fail from creating")
	}

	// Advance to ready
	m.SetStarting()
	m.SetReady()

	// SetStarting from ready should fail
	if err := m.SetStarting(); err == nil {
		t.Error("SetStarting should fail from ready")
	}
}

func TestClientStateManager_Concurrent(t *testing.T) {
	m := newClientStateManager()
	m.SetStarting()
	m.SetReady()

	var wg sync.WaitGroup
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			_ = m.Current()
			_ = m.IsReady()
			_ = m.IsClosed()
		}()
	}
	wg.Wait()
}

func TestThreadStateManager_Concurrent(t *testing.T) {
	m := newThreadStateManager()
	m.SetReady()

	var wg sync.WaitGroup
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			_ = m.Current()
			_ = m.IsReady()
			_ = m.IsProcessing()
			_ = m.IsClosed()
		}()
	}
	wg.Wait()
}
