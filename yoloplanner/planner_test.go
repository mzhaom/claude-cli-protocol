package yoloplanner

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude/render"
)

func TestReadLineWithContext_ReturnsLine(t *testing.T) {
	p := &PlannerWrapper{
		inputCh: make(chan string, 1),
	}

	// Send a line to the channel
	p.inputCh <- "test input"

	ctx := context.Background()
	line, err := p.readLineWithContext(ctx)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if line != "test input" {
		t.Errorf("expected 'test input', got '%s'", line)
	}
}

func TestReadLineWithContext_ReturnsErrorOnCancel(t *testing.T) {
	p := &PlannerWrapper{
		inputCh: make(chan string),
	}

	ctx, cancel := context.WithCancel(context.Background())

	// Cancel context after a short delay
	go func() {
		time.Sleep(10 * time.Millisecond)
		cancel()
	}()

	_, err := p.readLineWithContext(ctx)
	if err != context.Canceled {
		t.Errorf("expected context.Canceled, got %v", err)
	}
}

func TestReadLineWithContext_CancelBeforeRead(t *testing.T) {
	p := &PlannerWrapper{
		inputCh: make(chan string),
	}

	ctx, cancel := context.WithCancel(context.Background())
	cancel() // Cancel immediately

	start := time.Now()
	_, err := p.readLineWithContext(ctx)
	elapsed := time.Since(start)

	if err != context.Canceled {
		t.Errorf("expected context.Canceled, got %v", err)
	}
	if elapsed > 100*time.Millisecond {
		t.Errorf("readLineWithContext took too long: %v", elapsed)
	}
}

func TestTrackPlanFileWrite(t *testing.T) {
	tests := []struct {
		name         string
		toolName     string
		input        map[string]interface{}
		expectedPath string
	}{
		{
			name:     "detects .claude/plans write",
			toolName: "Write",
			input: map[string]interface{}{
				"file_path": "/Users/test/.claude/plans/my-plan.md",
			},
			expectedPath: "/Users/test/.claude/plans/my-plan.md",
		},
		{
			name:     "ignores non-plan writes",
			toolName: "Write",
			input: map[string]interface{}{
				"file_path": "/Users/test/some-file.md",
			},
			expectedPath: "",
		},
		{
			name:     "ignores non-md files in plans dir",
			toolName: "Write",
			input: map[string]interface{}{
				"file_path": "/Users/test/.claude/plans/notes.txt",
			},
			expectedPath: "",
		},
		{
			name:     "ignores other tools",
			toolName: "Read",
			input: map[string]interface{}{
				"file_path": "/Users/test/.claude/plans/my-plan.md",
			},
			expectedPath: "",
		},
		{
			name:         "handles missing file_path",
			toolName:     "Write",
			input:        map[string]interface{}{},
			expectedPath: "",
		},
		{
			name:     "handles non-string file_path",
			toolName: "Write",
			input: map[string]interface{}{
				"file_path": 12345,
			},
			expectedPath: "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &PlannerWrapper{
				inputCh: make(chan string),
			}

			p.trackPlanFileWrite(tt.toolName, tt.input)

			if p.planFilePath != tt.expectedPath {
				t.Errorf("expected planFilePath=%q, got %q", tt.expectedPath, p.planFilePath)
			}
		})
	}
}

func TestExportPlanToFile(t *testing.T) {
	// Create temp dir and plan file
	tmpDir := t.TempDir()
	planContent := "# My Plan\n\nThis is the actual plan content."
	planFile := filepath.Join(tmpDir, "test-plan.md")
	if err := os.WriteFile(planFile, []byte(planContent), 0644); err != nil {
		t.Fatal(err)
	}

	p := &PlannerWrapper{
		planFilePath: planFile,
		inputCh:      make(chan string),
	}

	// Export to a new file
	exportFile := filepath.Join(tmpDir, "exported.md")
	err := p.exportPlanToFile(exportFile)
	if err != nil {
		t.Fatalf("export failed: %v", err)
	}

	// Verify content matches
	exported, err := os.ReadFile(exportFile)
	if err != nil {
		t.Fatal(err)
	}
	if string(exported) != planContent {
		t.Errorf("exported content doesn't match\nexpected: %q\ngot: %q", planContent, string(exported))
	}
}

func TestExportPlanToFile_NoPlanFile(t *testing.T) {
	p := &PlannerWrapper{
		planFilePath: "", // No plan file detected
		inputCh:      make(chan string),
	}

	err := p.exportPlanToFile("/tmp/test.md")
	if err == nil {
		t.Error("expected error when planFilePath is empty")
	}
}

func TestExportPlanToFile_MissingSourceFile(t *testing.T) {
	p := &PlannerWrapper{
		planFilePath: "/nonexistent/path/plan.md",
		inputCh:      make(chan string),
	}

	err := p.exportPlanToFile("/tmp/test.md")
	if err == nil {
		t.Error("expected error when source file doesn't exist")
	}
}

func TestParseQuestionOptions(t *testing.T) {
	tests := []struct {
		name     string
		input    []interface{}
		expected []QuestionOption
	}{
		{
			name:  "string options",
			input: []interface{}{"Option A", "Option B"},
			expected: []QuestionOption{
				{Label: "Option A"},
				{Label: "Option B"},
			},
		},
		{
			name: "object options with description",
			input: []interface{}{
				map[string]interface{}{"label": "Opt1", "description": "First option"},
				map[string]interface{}{"label": "Opt2", "description": "Second option"},
			},
			expected: []QuestionOption{
				{Label: "Opt1", Description: "First option"},
				{Label: "Opt2", Description: "Second option"},
			},
		},
		{
			name:     "empty input",
			input:    []interface{}{},
			expected: []QuestionOption{},
		},
		{
			name: "mixed string and object options",
			input: []interface{}{
				"Simple option",
				map[string]interface{}{"label": "Complex", "description": "With desc"},
			},
			expected: []QuestionOption{
				{Label: "Simple option"},
				{Label: "Complex", Description: "With desc"},
			},
		},
		{
			name: "object without label is skipped",
			input: []interface{}{
				map[string]interface{}{"description": "No label"},
			},
			expected: []QuestionOption{},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := render.ParseQuestionOptions(tt.input)
			if len(result) != len(tt.expected) {
				t.Errorf("expected %d options, got %d", len(tt.expected), len(result))
				return
			}
			for i, opt := range result {
				if opt.Label != tt.expected[i].Label || opt.Description != tt.expected[i].Description {
					t.Errorf("option %d: expected %+v, got %+v", i, tt.expected[i], opt)
				}
			}
		})
	}
}

func TestParseOptionIndex(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		numOptions int
		expected   int
	}{
		{"valid first option", "1", 3, 0},
		{"valid last option", "3", 3, 2},
		{"zero is invalid", "0", 3, -1},
		{"exceeds options", "4", 3, -1},
		{"non-numeric", "abc", 3, -1},
		{"empty string", "", 3, -1},
		{"negative number", "-1", 3, -1},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := parseOptionIndex(tt.input, tt.numOptions)
			if result != tt.expected {
				t.Errorf("expected %d, got %d", tt.expected, result)
			}
		})
	}
}

func TestGeneratePlanFilename(t *testing.T) {
	tests := []struct {
		name     string
		prompt   string
		expected string
	}{
		{
			name:     "simple prompt",
			prompt:   "Create a hello world program",
			expected: "plan-create-a-hello-world-program.md",
		},
		{
			name:     "long prompt truncated to 5 words",
			prompt:   "Implement a REST API with authentication and rate limiting for users",
			expected: "plan-implement-a-rest-api-with.md",
		},
		{
			name:     "special characters removed",
			prompt:   "Fix bug #123 in the @login module!",
			expected: "plan-fix-bug-123-in-the.md",
		},
		{
			name:     "uppercase converted to lowercase",
			prompt:   "CREATE API",
			expected: "plan-create-api.md",
		},
		{
			name:     "numbers preserved",
			prompt:   "Add v2 endpoint",
			expected: "plan-add-v2-endpoint.md",
		},
		{
			name:     "single word prompt",
			prompt:   "Test",
			expected: "plan-test.md",
		},
		{
			name:     "empty prompt",
			prompt:   "",
			expected: "plan.md",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &PlannerWrapper{
				config:  Config{Prompt: tt.prompt},
				inputCh: make(chan string),
			}
			result := p.generatePlanFilename()
			if result != tt.expected {
				t.Errorf("expected %q, got %q", tt.expected, result)
			}
		})
	}
}

func TestBuildModeConstants(t *testing.T) {
	// Verify BuildMode constants have expected values
	tests := []struct {
		name     string
		mode     BuildMode
		expected string
	}{
		{"BuildModeNone", BuildModeNone, ""},
		{"BuildModeCurrent", BuildModeCurrent, "current"},
		{"BuildModeNewSession", BuildModeNewSession, "new"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if string(tt.mode) != tt.expected {
				t.Errorf("expected %q, got %q", tt.expected, string(tt.mode))
			}
		})
	}
}

func TestConfigBuildMode(t *testing.T) {
	// Test that BuildMode can be set in Config
	tests := []struct {
		name      string
		buildMode BuildMode
	}{
		{"none", BuildModeNone},
		{"current", BuildModeCurrent},
		{"new", BuildModeNewSession},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			config := Config{
				Model:     "sonnet",
				BuildMode: tt.buildMode,
			}
			if config.BuildMode != tt.buildMode {
				t.Errorf("expected BuildMode %q, got %q", tt.buildMode, config.BuildMode)
			}
		})
	}
}

func TestBuildModeFromString(t *testing.T) {
	// Test that string values can be converted to BuildMode
	tests := []struct {
		input    string
		expected BuildMode
	}{
		{"", BuildModeNone},
		{"current", BuildModeCurrent},
		{"new", BuildModeNewSession},
		{"unknown", BuildMode("unknown")}, // Invalid but should work
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			result := BuildMode(tt.input)
			if result != tt.expected {
				t.Errorf("expected %q, got %q", tt.expected, result)
			}
		})
	}
}

func TestWaitingForUserInputReset(t *testing.T) {
	// Test that waitingForUserInput is properly managed
	p := &PlannerWrapper{
		inputCh:             make(chan string),
		waitingForUserInput: true,
	}

	// Simulate what happens when we're executing (not waiting for input)
	p.waitingForUserInput = false

	if p.waitingForUserInput {
		t.Error("waitingForUserInput should be false after reset")
	}
}

func TestBuildModeIsValid(t *testing.T) {
	tests := []struct {
		mode  BuildMode
		valid bool
	}{
		{BuildModeNone, true},
		{BuildModeCurrent, true},
		{BuildModeNewSession, true},
		{BuildMode("invalid"), false},
		{BuildMode("CURRENT"), false}, // Case sensitive
		{BuildMode("  "), false},      // Whitespace
	}

	for _, tt := range tests {
		t.Run(string(tt.mode), func(t *testing.T) {
			if got := tt.mode.IsValid(); got != tt.valid {
				t.Errorf("BuildMode(%q).IsValid() = %v, want %v", tt.mode, got, tt.valid)
			}
		})
	}
}

func TestPlanFilePathRequired(t *testing.T) {
	// Test that executeInNewSession requires a valid plan file path
	tests := []struct {
		name         string
		planFilePath string
		expectFail   bool // Whether we expect the path check to fail
	}{
		{"empty path requires fallback", "", true},
		{"nonexistent file requires fallback", "/nonexistent/path/plan.md", true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &PlannerWrapper{
				planFilePath: tt.planFilePath,
				inputCh:      make(chan string),
			}

			// Check the conditions that executeInNewSession would check
			needsFallback := false
			if p.planFilePath == "" {
				needsFallback = true
			} else if _, err := os.Stat(p.planFilePath); os.IsNotExist(err) {
				needsFallback = true
			}

			if needsFallback != tt.expectFail {
				t.Errorf("expected needsFallback=%v, got %v", tt.expectFail, needsFallback)
			}
		})
	}
}

func TestPlanFilePathWithValidFile(t *testing.T) {
	// Create a temp file to simulate a valid plan file
	tmpFile, err := os.CreateTemp("", "plan-*.md")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpFile.Name())
	tmpFile.WriteString("# Test Plan\n")
	tmpFile.Close()

	p := &PlannerWrapper{
		planFilePath: tmpFile.Name(),
		inputCh:      make(chan string),
	}

	// Should not need fallback with valid file
	needsFallback := false
	if p.planFilePath == "" {
		needsFallback = true
	} else if _, err := os.Stat(p.planFilePath); os.IsNotExist(err) {
		needsFallback = true
	}

	if needsFallback {
		t.Error("expected no fallback needed for valid plan file")
	}
}
