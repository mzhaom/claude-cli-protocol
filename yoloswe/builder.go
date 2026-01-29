// Package yoloswe provides a builder-reviewer loop for software engineering tasks.
package yoloswe

import (
	"context"
	"fmt"
	"io"
	"strings"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude"
	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude/render"
)

// BuilderConfig holds configuration for the builder session.
type BuilderConfig struct {
	Model           string
	WorkDir         string
	RecordingDir    string
	SystemPrompt    string
	Verbose         bool
	RequireApproval bool   // Require user approval for tool executions (default: auto-approve)
	ResumeSessionID string // Resume from a previous session ID
}

// BuilderSession wraps a claude.Session for builder operations.
type BuilderSession struct {
	session  *claude.Session
	config   BuilderConfig
	output   io.Writer
	renderer *render.Renderer
}

// NewBuilderSession creates a new builder session with the given config.
func NewBuilderSession(config BuilderConfig, output io.Writer) *BuilderSession {
	if config.Model == "" {
		config.Model = "sonnet"
	}
	if config.RecordingDir == "" {
		config.RecordingDir = ".swe-sessions"
	}
	return &BuilderSession{
		config:   config,
		output:   output,
		renderer: render.NewRenderer(output, config.Verbose),
	}
}

// Start initializes and starts the claude session.
func (b *BuilderSession) Start(ctx context.Context) error {
	opts := []claude.SessionOption{
		claude.WithModel(b.config.Model),
		claude.WithPermissionPromptToolStdio(),
		claude.WithRecording(b.config.RecordingDir),
	}

	// Default: bypass permissions (auto-approve all tools)
	// Use --require-approval to enable manual approval
	if b.config.RequireApproval {
		opts = append(opts, claude.WithPermissionMode(claude.PermissionModeDefault))
	} else {
		opts = append(opts,
			claude.WithPermissionMode(claude.PermissionModeBypass),
			claude.WithPermissionHandler(claude.AllowAllPermissionHandler()),
		)
	}

	if b.config.WorkDir != "" {
		opts = append(opts, claude.WithWorkDir(b.config.WorkDir))
	}

	if b.config.SystemPrompt != "" {
		opts = append(opts, claude.WithSystemPrompt(b.config.SystemPrompt))
	}

	if b.config.ResumeSessionID != "" {
		opts = append(opts, claude.WithResume(b.config.ResumeSessionID))
	}

	b.session = claude.NewSession(opts...)
	return b.session.Start(ctx)
}

// Stop gracefully shuts down the session. Safe to call before Start.
func (b *BuilderSession) Stop() error {
	if b.session == nil {
		return nil
	}
	return b.session.Stop()
}

// RunTurn sends a message and processes the turn until completion.
// Returns the turn usage for budget tracking.
func (b *BuilderSession) RunTurn(ctx context.Context, message string) (*claude.TurnUsage, error) {
	if strings.TrimSpace(message) == "" {
		return nil, fmt.Errorf("message cannot be empty")
	}

	_, err := b.session.SendMessage(ctx, message)
	if err != nil {
		return nil, fmt.Errorf("failed to send message: %w", err)
	}

	for {
		select {
		case <-ctx.Done():
			return nil, ctx.Err()
		case event, ok := <-b.session.Events():
			if !ok {
				return nil, fmt.Errorf("session ended unexpectedly")
			}

			switch e := event.(type) {
			case claude.ReadyEvent:
				b.renderer.Status(fmt.Sprintf("Builder session started: %s (model: %s)", e.Info.SessionID, e.Info.Model))

			case claude.TextEvent:
				b.renderer.Text(e.Text)

			case claude.ThinkingEvent:
				b.renderer.Thinking(e.Thinking)

			case claude.ToolStartEvent:
				b.renderer.ToolStart(e.Name, e.ID)

			case claude.ToolCompleteEvent:
				b.renderer.ToolComplete(e.Name, e.Input)

				// Handle AskUserQuestion by auto-selecting first option
				if e.Name == "AskUserQuestion" {
					response := b.autoAnswerQuestion(e.Input)
					if _, err := b.session.SendToolResult(ctx, e.ID, response); err != nil {
						return nil, fmt.Errorf("failed to send tool result: %w", err)
					}
				}

			case claude.CLIToolResultEvent:
				b.renderer.ToolResult(e.Content, e.IsError)

			case claude.TurnCompleteEvent:
				b.renderer.TurnSummary(e.TurnNumber, e.Success, e.DurationMs, e.Usage.CostUSD)
				if !e.Success {
					return &e.Usage, fmt.Errorf("turn completed with success=false")
				}
				return &e.Usage, nil

			case claude.ErrorEvent:
				b.renderer.Error(e.Error, e.Context)
				// Return partial usage if available, along with error
				return nil, fmt.Errorf("builder error: %v (context: %s)", e.Error, e.Context)
			}
		}
	}
}

// autoAnswerQuestion automatically responds to AskUserQuestion prompts from the builder.
// This enables autonomous operation by selecting reasonable defaults without user intervention.
//
// The function implements intelligent fallback logic:
//  1. For questions with explicit options: selects the first option
//  2. For multi-select questions: selects only the first option (conservative approach)
//  3. For questions without options: infers answer based on question keywords
//     - "continue", "proceed", "confirm" -> "yes"
//     - "which", "select", "choose" -> "first option"
//     - default -> "yes"
//  4. For malformed or missing questions: returns safe default message
//
// Parameters:
//   - input: The AskUserQuestion tool input containing questions array
//
// Returns:
//   - A formatted string response suitable for SendToolResult
//
// Edge cases handled:
//   - nil input map
//   - missing or malformed questions array
//   - questions without text or options
//   - options as strings vs. objects with label/description
//   - multiSelect flag handling
func (b *BuilderSession) autoAnswerQuestion(input map[string]interface{}) string {
	// Validate input structure
	if input == nil {
		b.renderer.Status("Auto-answering: no questions provided, proceeding with default")
		return "Proceeding with default option."
	}

	questions, ok := input["questions"].([]interface{})
	if !ok || len(questions) == 0 {
		b.renderer.Status("Auto-answering: no questions array found, proceeding with default")
		return "Proceeding with default option."
	}

	var responses []string
	for i, q := range questions {
		qMap, ok := q.(map[string]interface{})
		if !ok {
			b.renderer.Status(fmt.Sprintf("Auto-answering: skipping malformed question %d", i))
			continue
		}

		question, _ := qMap["question"].(string)
		if question == "" {
			question = fmt.Sprintf("Question %d", i+1)
		}

		// Check for multiSelect flag
		multiSelect, _ := qMap["multiSelect"].(bool)

		optionsRaw, _ := qMap["options"].([]interface{})

		var response string
		if len(optionsRaw) > 0 {
			// Parse first option (or multiple if multiSelect)
			opt := optionsRaw[0]
			switch v := opt.(type) {
			case string:
				response = v
			case map[string]interface{}:
				if label, ok := v["label"].(string); ok {
					response = label
				} else if desc, ok := v["description"].(string); ok {
					// Fallback to description if no label
					response = desc
				} else {
					response = "Option 1"
				}
			default:
				response = "Option 1"
			}

			// For multiSelect, only select first option
			if multiSelect && len(optionsRaw) > 1 {
				b.renderer.Status(fmt.Sprintf("Auto-answering (multi-select): %s -> %s (selecting first option only)", question, response))
			} else {
				b.renderer.Status(fmt.Sprintf("Auto-answering: %s -> %s", question, response))
			}
		} else {
			// No options provided, use intelligent default
			questionLower := strings.ToLower(question)
			if strings.Contains(questionLower, "continue") ||
				strings.Contains(questionLower, "proceed") ||
				strings.Contains(questionLower, "confirm") {
				response = "yes"
			} else if strings.Contains(questionLower, "which") ||
				strings.Contains(questionLower, "select") ||
				strings.Contains(questionLower, "choose") {
				response = "first option"
			} else {
				response = "yes"
			}
			b.renderer.Status(fmt.Sprintf("Auto-answering (no options): %s -> %s", question, response))
		}

		responses = append(responses, fmt.Sprintf("Q: %s\nA: %s", question, response))
	}

	if len(responses) == 0 {
		return "Proceeding with default option."
	}

	return "User responses:\n" + joinStrings(responses, "\n")
}

func joinStrings(strs []string, sep string) string {
	if len(strs) == 0 {
		return ""
	}
	result := strs[0]
	for _, s := range strs[1:] {
		result += sep + s
	}
	return result
}

// RecordingPath returns the path to the session recording directory.
// Returns empty string if session not started.
func (b *BuilderSession) RecordingPath() string {
	if b.session == nil {
		return ""
	}
	return b.session.RecordingPath()
}
