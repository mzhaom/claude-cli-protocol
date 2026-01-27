package yoloplanner

import (
	"context"
	"os"
	"path/filepath"
	"testing"
	"time"
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
			result := parseQuestionOptions(tt.input)
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
