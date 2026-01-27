package claude

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"time"
)

// RecordingMetadata contains metadata about a recording.
type RecordingMetadata struct {
	SessionID         string   `json:"session_id"`
	Model             string   `json:"model"`
	WorkDir           string   `json:"cwd"`
	Tools             []string `json:"tools"`
	ClaudeCodeVersion string   `json:"claude_code_version,omitempty"`
	PermissionMode    string   `json:"permission_mode"`
	StartTime         string   `json:"start_time"`
	TotalTurns        int      `json:"total_turns"`
}

// TurnSummary contains a summary of a recorded turn.
type TurnSummary struct {
	Number      int       `json:"number"`
	UserMessage string    `json:"user_message,omitempty"`
	StartTime   time.Time `json:"start_time"`
	EndTime     time.Time `json:"end_time,omitempty"`
	DurationMs  int64     `json:"duration_ms,omitempty"`
	CostUSD     float64   `json:"cost_usd,omitempty"`
	Success     bool      `json:"success"`
}

// SessionRecording contains a complete recording.
type SessionRecording struct {
	Metadata RecordingMetadata `json:"metadata"`
	Turns    []TurnSummary     `json:"turns"`
}

// TotalCost returns the total cost of all turns.
func (r *SessionRecording) TotalCost() float64 {
	var total float64
	for _, turn := range r.Turns {
		total += turn.CostUSD
	}
	return total
}

// sessionRecorder records session messages to disk.
type sessionRecorder struct {
	mu sync.Mutex

	baseDir   string
	sessionID string
	dirPath   string

	metadata RecordingMetadata
	turns    []TurnSummary

	toCliFile   *os.File
	fromCliFile *os.File

	initialized bool
}

// newSessionRecorder creates a new session recorder.
func newSessionRecorder(baseDir string) *sessionRecorder {
	if baseDir == "" {
		baseDir = ".claude-sessions"
	}
	return &sessionRecorder{
		baseDir: baseDir,
		turns:   make([]TurnSummary, 0),
	}
}

// Initialize initializes the recorder with session metadata.
func (sr *sessionRecorder) Initialize(meta RecordingMetadata) error {
	sr.mu.Lock()
	defer sr.mu.Unlock()

	if sr.initialized {
		return nil
	}

	sr.sessionID = meta.SessionID
	sr.metadata = meta
	sr.metadata.StartTime = time.Now().Format(time.RFC3339)

	// Create session directory
	sr.dirPath = filepath.Join(sr.baseDir, fmt.Sprintf("session-%s-%d", sr.sessionID, time.Now().Unix()))
	if err := os.MkdirAll(sr.dirPath, 0755); err != nil {
		return err
	}

	// Open message files
	var err error
	sr.toCliFile, err = os.Create(filepath.Join(sr.dirPath, "to_cli.jsonl"))
	if err != nil {
		return err
	}

	sr.fromCliFile, err = os.Create(filepath.Join(sr.dirPath, "from_cli.jsonl"))
	if err != nil {
		sr.toCliFile.Close()
		return err
	}

	sr.initialized = true
	return nil
}

// RecordSent records a message sent to the CLI (writes raw message JSON).
func (sr *sessionRecorder) RecordSent(msg interface{}) {
	sr.mu.Lock()
	defer sr.mu.Unlock()

	if !sr.initialized || sr.toCliFile == nil {
		return
	}

	data, err := json.Marshal(msg)
	if err != nil {
		return
	}

	sr.toCliFile.Write(data)
	sr.toCliFile.Write([]byte("\n"))
}

// RecordReceived records a message received from the CLI (writes raw message JSON).
func (sr *sessionRecorder) RecordReceived(msg interface{}) {
	sr.mu.Lock()
	defer sr.mu.Unlock()

	if !sr.initialized || sr.fromCliFile == nil {
		return
	}

	data, err := json.Marshal(msg)
	if err != nil {
		return
	}

	sr.fromCliFile.Write(data)
	sr.fromCliFile.Write([]byte("\n"))
}

// StartTurn starts recording a new turn.
func (sr *sessionRecorder) StartTurn(turnNumber int, userMessage interface{}) {
	sr.mu.Lock()
	defer sr.mu.Unlock()

	msgStr := ""
	if s, ok := userMessage.(string); ok {
		msgStr = s
	}

	summary := TurnSummary{
		Number:      turnNumber,
		UserMessage: msgStr,
		StartTime:   time.Now(),
	}

	sr.turns = append(sr.turns, summary)
}

// CompleteTurn marks a turn as complete.
func (sr *sessionRecorder) CompleteTurn(turnNumber int, result TurnResult) {
	sr.mu.Lock()
	defer sr.mu.Unlock()

	for i := range sr.turns {
		if sr.turns[i].Number == turnNumber {
			sr.turns[i].EndTime = time.Now()
			sr.turns[i].DurationMs = result.DurationMs
			sr.turns[i].CostUSD = result.Usage.CostUSD
			sr.turns[i].Success = result.Success
			break
		}
	}

	sr.metadata.TotalTurns = len(sr.turns)
	sr.saveMeta()
}

// saveMeta saves the metadata file.
func (sr *sessionRecorder) saveMeta() {
	if !sr.initialized {
		return
	}

	recording := SessionRecording{
		Metadata: sr.metadata,
		Turns:    sr.turns,
	}

	data, err := json.MarshalIndent(recording, "", "  ")
	if err != nil {
		return
	}

	metaPath := filepath.Join(sr.dirPath, "meta.json")
	os.WriteFile(metaPath, data, 0644)
}

// GetRecording returns the current recording.
func (sr *sessionRecorder) GetRecording() *SessionRecording {
	sr.mu.Lock()
	defer sr.mu.Unlock()

	if !sr.initialized {
		return nil
	}

	return &SessionRecording{
		Metadata: sr.metadata,
		Turns:    sr.turns,
	}
}

// Path returns the recording directory path.
func (sr *sessionRecorder) Path() string {
	sr.mu.Lock()
	defer sr.mu.Unlock()
	return sr.dirPath
}

// Close closes the recorder and saves final metadata.
func (sr *sessionRecorder) Close() error {
	sr.mu.Lock()
	defer sr.mu.Unlock()

	if !sr.initialized {
		return nil
	}

	sr.saveMeta()

	if sr.toCliFile != nil {
		sr.toCliFile.Close()
	}
	if sr.fromCliFile != nil {
		sr.fromCliFile.Close()
	}

	return nil
}

// LoadRecording loads a recording from disk.
func LoadRecording(dirPath string) (*SessionRecording, error) {
	metaPath := filepath.Join(dirPath, "meta.json")
	data, err := os.ReadFile(metaPath)
	if err != nil {
		return nil, err
	}

	var recording SessionRecording
	if err := json.Unmarshal(data, &recording); err != nil {
		return nil, err
	}

	return &recording, nil
}
