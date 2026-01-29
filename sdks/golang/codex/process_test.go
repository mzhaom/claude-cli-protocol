package codex

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude"
)

func TestProcessManager_SessionLogging(t *testing.T) {
	// Create temp directory for session log
	tmpDir, err := os.MkdirTemp("", "codex-test-*")
	if err != nil {
		t.Fatalf("failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	logPath := filepath.Join(tmpDir, "session.jsonl")

	// Create process manager with session logging
	pm := newProcessManager(ClientConfig{
		SessionLogPath: logPath,
	})

	// Simulate session log file creation (without starting actual process)
	f, err := os.Create(logPath)
	if err != nil {
		t.Fatalf("failed to create session log: %v", err)
	}
	pm.sessionLog = f

	// Test logging sent message
	testMsg := map[string]string{"type": "test", "data": "hello"}
	msgData, _ := json.Marshal(testMsg)
	pm.logMessageLocked("sent", msgData)

	// Test logging received message
	recvMsg := map[string]string{"type": "response", "data": "world"}
	recvData, _ := json.Marshal(recvMsg)
	pm.logMessageLocked("received", recvData)

	// Close the file
	f.Close()
	pm.sessionLog = nil

	// Verify the log file contents
	data, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatalf("failed to read log file: %v", err)
	}

	// Parse each line
	lines := splitLines(data)
	if len(lines) != 2 {
		t.Errorf("expected 2 log entries, got %d", len(lines))
	}

	// Verify first message (sent)
	var msg1 claude.RecordedMessage
	if err := json.Unmarshal(lines[0], &msg1); err != nil {
		t.Fatalf("failed to parse first message: %v", err)
	}
	if msg1.Direction != "sent" {
		t.Errorf("expected direction 'sent', got '%s'", msg1.Direction)
	}
	if msg1.Timestamp.IsZero() {
		t.Error("timestamp should not be zero")
	}

	// Verify second message (received)
	var msg2 claude.RecordedMessage
	if err := json.Unmarshal(lines[1], &msg2); err != nil {
		t.Fatalf("failed to parse second message: %v", err)
	}
	if msg2.Direction != "received" {
		t.Errorf("expected direction 'received', got '%s'", msg2.Direction)
	}
}

func TestProcessManager_NoSessionLog(t *testing.T) {
	// Create process manager without session logging
	pm := newProcessManager(ClientConfig{})

	// This should not panic
	pm.logMessageLocked("sent", []byte(`{"test": "data"}`))
}

// splitLines splits data by newlines, ignoring empty lines
func splitLines(data []byte) [][]byte {
	var lines [][]byte
	start := 0
	for i, b := range data {
		if b == '\n' {
			if i > start {
				lines = append(lines, data[start:i])
			}
			start = i + 1
		}
	}
	if start < len(data) {
		lines = append(lines, data[start:])
	}
	return lines
}
