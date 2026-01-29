// Package yoloswe provides a builder-reviewer loop for software engineering tasks.
package yoloswe

import (
	"context"
	"fmt"
	"io"

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
	RequireApproval bool // Require user approval for tool executions (default: auto-approve)
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

	b.session = claude.NewSession(opts...)
	return b.session.Start(ctx)
}

// Stop gracefully shuts down the session.
func (b *BuilderSession) Stop() error {
	if b.session != nil {
		return b.session.Stop()
	}
	return nil
}

// RunTurn sends a message and processes the turn until completion.
// Returns the turn usage for budget tracking.
func (b *BuilderSession) RunTurn(ctx context.Context, message string) (*claude.TurnUsage, error) {
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
				return &e.Usage, nil

			case claude.ErrorEvent:
				b.renderer.Error(e.Error, e.Context)
				return nil, e.Error
			}
		}
	}
}

// autoAnswerQuestion extracts the first option from an AskUserQuestion input
// and formats a response.
func (b *BuilderSession) autoAnswerQuestion(input map[string]interface{}) string {
	questions, ok := input["questions"].([]interface{})
	if !ok || len(questions) == 0 {
		return "Proceeding with default option."
	}

	var responses []string
	for _, q := range questions {
		qMap, ok := q.(map[string]interface{})
		if !ok {
			continue
		}

		question, _ := qMap["question"].(string)
		optionsRaw, _ := qMap["options"].([]interface{})

		var response string
		if len(optionsRaw) > 0 {
			// Parse first option
			opt := optionsRaw[0]
			switch v := opt.(type) {
			case string:
				response = v
			case map[string]interface{}:
				if label, ok := v["label"].(string); ok {
					response = label
				}
			}
			b.renderer.Status(fmt.Sprintf("Auto-answering question: %s -> %s", question, response))
		} else {
			response = "yes"
			b.renderer.Status(fmt.Sprintf("Auto-answering question: %s -> %s", question, response))
		}
		responses = append(responses, fmt.Sprintf("Q: %s\nA: %s", question, response))
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
func (b *BuilderSession) RecordingPath() string {
	if b.session != nil {
		return b.session.RecordingPath()
	}
	return ""
}
