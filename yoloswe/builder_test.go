package yoloswe

import (
	"bytes"
	"fmt"
	"strings"
	"testing"
)

func TestNewBuilderSession(t *testing.T) {
	tests := []struct {
		name           string
		config         BuilderConfig
		expectedModel  string
		expectedRecDir string
	}{
		{
			name:           "default values",
			config:         BuilderConfig{},
			expectedModel:  "sonnet",
			expectedRecDir: ".swe-sessions",
		},
		{
			name: "custom values",
			config: BuilderConfig{
				Model:        "opus",
				RecordingDir: "/tmp/custom",
			},
			expectedModel:  "opus",
			expectedRecDir: "/tmp/custom",
		},
		{
			name: "empty model uses default",
			config: BuilderConfig{
				Model:        "",
				RecordingDir: "/custom",
			},
			expectedModel:  "sonnet",
			expectedRecDir: "/custom",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var buf bytes.Buffer
			builder := NewBuilderSession(tt.config, &buf)

			if builder.config.Model != tt.expectedModel {
				t.Errorf("expected model %q, got %q", tt.expectedModel, builder.config.Model)
			}
			if builder.config.RecordingDir != tt.expectedRecDir {
				t.Errorf("expected recording dir %q, got %q", tt.expectedRecDir, builder.config.RecordingDir)
			}
			if builder.output != &buf {
				t.Error("output writer not set correctly")
			}
			if builder.renderer == nil {
				t.Error("renderer not initialized")
			}
		})
	}
}

func TestAutoAnswerQuestion(t *testing.T) {
	tests := []struct {
		name     string
		input    map[string]interface{}
		expected string
	}{
		{
			name:     "nil input",
			input:    nil,
			expected: "Proceeding with default option.",
		},
		{
			name:     "empty input",
			input:    map[string]interface{}{},
			expected: "Proceeding with default option.",
		},
		{
			name: "no questions array",
			input: map[string]interface{}{
				"questions": "not an array",
			},
			expected: "Proceeding with default option.",
		},
		{
			name: "single question with string options",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "Choose an option?",
						"options":  []interface{}{"option1", "option2"},
					},
				},
			},
			expected: "User responses:\nQ: Choose an option?\nA: option1",
		},
		{
			name: "single question with label options",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "Select library?",
						"options": []interface{}{
							map[string]interface{}{"label": "React"},
							map[string]interface{}{"label": "Vue"},
						},
					},
				},
			},
			expected: "User responses:\nQ: Select library?\nA: React",
		},
		{
			name: "multiple questions",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "Question 1?",
						"options":  []interface{}{"a", "b"},
					},
					map[string]interface{}{
						"question": "Question 2?",
						"options": []interface{}{
							map[string]interface{}{"label": "yes"},
						},
					},
				},
			},
			expected: "User responses:\nQ: Question 1?\nA: a\nQ: Question 2?\nA: yes",
		},
		{
			name: "question with no options",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "Continue?",
						"options":  []interface{}{},
					},
				},
			},
			expected: "User responses:\nQ: Continue?\nA: yes",
		},
		{
			name: "malformed question object",
			input: map[string]interface{}{
				"questions": []interface{}{
					"not a map",
				},
			},
			expected: "Proceeding with default option.",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var buf bytes.Buffer
			builder := NewBuilderSession(BuilderConfig{}, &buf)
			result := builder.autoAnswerQuestion(tt.input)

			if result != tt.expected {
				t.Errorf("expected %q, got %q", tt.expected, result)
			}
		})
	}
}

func TestJoinStrings(t *testing.T) {
	tests := []struct {
		name     string
		strs     []string
		sep      string
		expected string
	}{
		{
			name:     "empty array",
			strs:     []string{},
			sep:      ",",
			expected: "",
		},
		{
			name:     "single element",
			strs:     []string{"hello"},
			sep:      ",",
			expected: "hello",
		},
		{
			name:     "multiple elements with comma",
			strs:     []string{"a", "b", "c"},
			sep:      ",",
			expected: "a,b,c",
		},
		{
			name:     "multiple elements with newline",
			strs:     []string{"first", "second", "third"},
			sep:      "\n",
			expected: "first\nsecond\nthird",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := joinStrings(tt.strs, tt.sep)
			if result != tt.expected {
				t.Errorf("expected %q, got %q", tt.expected, result)
			}
		})
	}
}

func TestBuilderStop(t *testing.T) {
	t.Run("stop with nil session", func(t *testing.T) {
		var buf bytes.Buffer
		builder := NewBuilderSession(BuilderConfig{}, &buf)
		// session is nil before Start
		if err := builder.Stop(); err != nil {
			t.Errorf("Stop() with nil session should not error, got %v", err)
		}
	})
}

func TestBuilderRecordingPath(t *testing.T) {
	t.Run("recording path with nil session", func(t *testing.T) {
		var buf bytes.Buffer
		builder := NewBuilderSession(BuilderConfig{}, &buf)
		// session is nil before Start
		path := builder.RecordingPath()
		if path != "" {
			t.Errorf("RecordingPath() with nil session should return empty string, got %q", path)
		}
	})
}

func TestBuilderConfigValidation(t *testing.T) {
	tests := []struct {
		name        string
		config      BuilderConfig
		shouldPanic bool
	}{
		{
			name: "valid config",
			config: BuilderConfig{
				Model:        "sonnet",
				WorkDir:      "/tmp",
				RecordingDir: "/tmp/recordings",
			},
			shouldPanic: false,
		},
		{
			name: "minimal config",
			config: BuilderConfig{
				Model: "haiku",
			},
			shouldPanic: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			defer func() {
				if r := recover(); r != nil && !tt.shouldPanic {
					t.Errorf("unexpected panic: %v", r)
				}
			}()

			var buf bytes.Buffer
			builder := NewBuilderSession(tt.config, &buf)
			if builder == nil {
				t.Error("NewBuilderSession returned nil")
			}
		})
	}
}

// TestBuilderRunTurnCancellation tests that RunTurn handles context cancellation correctly
func TestBuilderRunTurnCancellation(t *testing.T) {
	t.Skip("Requires integration test with real session")
}

// TestBuilderStartWithInvalidWorkDir tests that Start handles invalid work directory
func TestBuilderStartWithInvalidWorkDir(t *testing.T) {
	t.Skip("Requires integration test to verify error handling")
}

func TestAutoAnswerQuestionEdgeCases(t *testing.T) {
	tests := []struct {
		name     string
		input    map[string]interface{}
		expected string
	}{
		{
			name: "multiSelect flag with multiple options",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question":    "Select features?",
						"multiSelect": true,
						"options": []interface{}{
							map[string]interface{}{"label": "Feature A"},
							map[string]interface{}{"label": "Feature B"},
							map[string]interface{}{"label": "Feature C"},
						},
					},
				},
			},
			expected: "User responses:\nQ: Select features?\nA: Feature A",
		},
		{
			name: "question with description fallback",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "Pick approach?",
						"options": []interface{}{
							map[string]interface{}{"description": "Use caching"},
						},
					},
				},
			},
			expected: "User responses:\nQ: Pick approach?\nA: Use caching",
		},
		{
			name: "empty question text defaults",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "",
						"options":  []interface{}{"opt1"},
					},
				},
			},
			expected: "User responses:\nQ: Question 1\nA: opt1",
		},
		{
			name: "question with 'which' keyword and no options",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "Which approach should we use?",
						"options":  []interface{}{},
					},
				},
			},
			expected: "User responses:\nQ: Which approach should we use?\nA: first option",
		},
		{
			name: "question with 'select' keyword and no options",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "Select the best option?",
						"options":  []interface{}{},
					},
				},
			},
			expected: "User responses:\nQ: Select the best option?\nA: first option",
		},
		{
			name: "question with 'choose' keyword and no options",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "Choose a method?",
						"options":  []interface{}{},
					},
				},
			},
			expected: "User responses:\nQ: Choose a method?\nA: first option",
		},
		{
			name: "mixed valid and invalid questions",
			input: map[string]interface{}{
				"questions": []interface{}{
					"invalid",
					map[string]interface{}{
						"question": "Valid question?",
						"options":  []interface{}{"yes", "no"},
					},
				},
			},
			expected: "User responses:\nQ: Valid question?\nA: yes",
		},
		{
			name: "option without label or description",
			input: map[string]interface{}{
				"questions": []interface{}{
					map[string]interface{}{
						"question": "Test?",
						"options": []interface{}{
							map[string]interface{}{"other": "value"},
						},
					},
				},
			},
			expected: "User responses:\nQ: Test?\nA: Option 1",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var buf bytes.Buffer
			builder := NewBuilderSession(BuilderConfig{}, &buf)
			result := builder.autoAnswerQuestion(tt.input)

			if result != tt.expected {
				t.Errorf("expected %q, got %q", tt.expected, result)
			}
		})
	}
}

func TestBuilderNilOutput(t *testing.T) {
	// Should handle nil output gracefully
	builder := NewBuilderSession(BuilderConfig{}, nil)
	if builder == nil {
		t.Error("NewBuilderSession should not return nil even with nil output")
	}
}

func TestAutoAnswerQuestionPerformance(t *testing.T) {
	// Test with many questions to ensure no performance issues
	questions := make([]interface{}, 100)
	for i := 0; i < 100; i++ {
		questions[i] = map[string]interface{}{
			"question": fmt.Sprintf("Question %d?", i),
			"options":  []interface{}{"a", "b", "c"},
		}
	}

	input := map[string]interface{}{
		"questions": questions,
	}

	var buf bytes.Buffer
	builder := NewBuilderSession(BuilderConfig{}, &buf)

	result := builder.autoAnswerQuestion(input)

	// Should handle all questions
	if !strings.Contains(result, "Question 0?") {
		t.Error("should process first question")
	}
	if !strings.Contains(result, "Question 99?") {
		t.Error("should process last question")
	}
}
