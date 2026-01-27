// Package yoloplanner provides a simple wrapper over claude-cli for planner mode.
package yoloplanner

import (
	"bufio"
	"context"
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude"
)

// Config holds planner configuration.
type Config struct {
	// Model to use: "haiku", "sonnet", "opus"
	Model string

	// WorkDir is the working directory for file operations.
	WorkDir string

	// RecordingDir is the directory for session recordings.
	RecordingDir string

	// SystemPrompt overrides the default system prompt.
	SystemPrompt string
}

// PlannerWrapper wraps a claude.Session with planner-specific logic.
type PlannerWrapper struct {
	session  *claude.Session
	renderer *Renderer
	reader   *bufio.Reader
	config   Config

	// Accumulated plan text
	planText strings.Builder

	// State tracking
	waitingForUserInput bool // True when waiting for AskUserQuestion or ExitPlanMode response
}

// NewPlannerWrapper creates a new planner wrapper with the given configuration.
func NewPlannerWrapper(config Config) *PlannerWrapper {
	// Apply defaults
	if config.Model == "" {
		config.Model = "sonnet"
	}
	if config.RecordingDir == "" {
		config.RecordingDir = ".planner-sessions"
	}

	return &PlannerWrapper{
		config:   config,
		renderer: NewRenderer(os.Stdout),
		reader:   bufio.NewReader(os.Stdin),
	}
}

// Start initializes and starts the underlying claude session.
func (p *PlannerWrapper) Start(ctx context.Context) error {
	opts := []claude.SessionOption{
		claude.WithModel(p.config.Model),
		claude.WithPermissionMode(claude.PermissionModePlan),
		claude.WithDangerouslySkipPermissions(),
		claude.WithRecording(p.config.RecordingDir),
	}

	if p.config.WorkDir != "" {
		opts = append(opts, claude.WithWorkDir(p.config.WorkDir))
	}

	if p.config.SystemPrompt != "" {
		opts = append(opts, claude.WithSystemPrompt(p.config.SystemPrompt))
	}

	// Print CLI flags that will be used
	fmt.Fprintf(os.Stderr, "Starting claude with flags: --print --input-format stream-json --output-format stream-json --verbose --model %s --permission-mode plan --dangerously-skip-permissions --include-partial-messages\n", p.config.Model)

	p.session = claude.NewSession(opts...)
	return p.session.Start(ctx)
}

// Stop gracefully shuts down the session.
func (p *PlannerWrapper) Stop() error {
	if p.session != nil {
		return p.session.Stop()
	}
	return nil
}

// Run sends the initial prompt and processes events until completion.
func (p *PlannerWrapper) Run(ctx context.Context, prompt string) error {
	_, err := p.session.SendMessage(ctx, prompt)
	if err != nil {
		return fmt.Errorf("failed to send initial prompt: %w", err)
	}

	for event := range p.session.Events() {
		done, err := p.handleEvent(ctx, event)
		if err != nil {
			return err
		}
		if done {
			return nil
		}
	}

	return nil
}

// handleEvent processes a single event and returns (done, error).
func (p *PlannerWrapper) handleEvent(ctx context.Context, event claude.Event) (bool, error) {
	switch e := event.(type) {
	case claude.ReadyEvent:
		p.renderer.Status(fmt.Sprintf("Session started: %s (model: %s)", e.Info.SessionID, e.Info.Model))

	case claude.TextEvent:
		p.planText.WriteString(e.Text)
		p.renderer.Text(e.Text)

	case claude.ThinkingEvent:
		p.renderer.Thinking(e.Thinking)

	case claude.ToolStartEvent:
		p.renderer.ToolStart(e.Name, e.ID)

	case claude.ToolProgressEvent:
		// Skip streaming tool progress - too noisy
		// p.renderer.ToolProgress(e.InputChunk)

	case claude.ToolCompleteEvent:
		p.renderer.ToolComplete(e.Name, e.Input)

		// Handle special interactive tools
		if e.Name == "AskUserQuestion" {
			p.waitingForUserInput = true
			return false, p.handleAskUserQuestion(ctx, e.Input)
		} else if e.Name == "ExitPlanMode" {
			p.waitingForUserInput = true
			return p.handleExitPlanMode(ctx, e.Input)
		}

	case claude.CLIToolResultEvent:
		p.renderer.ToolResult(e.Content, e.IsError)

	case claude.TurnCompleteEvent:
		p.renderer.TurnSummary(e)
		// If we're not waiting for user input (AskUserQuestion/ExitPlanMode),
		// the turn is complete and we should exit
		if !p.waitingForUserInput {
			return true, nil
		}
		p.waitingForUserInput = false

	case claude.ErrorEvent:
		p.renderer.Error(e.Error, e.Context)
		return false, e.Error
	}

	return false, nil
}

// QuestionOption represents an option for a question.
type QuestionOption struct {
	Label       string
	Description string
}

// handleAskUserQuestion handles the AskUserQuestion tool call.
func (p *PlannerWrapper) handleAskUserQuestion(ctx context.Context, input map[string]interface{}) error {
	questions, ok := input["questions"].([]interface{})
	if !ok {
		return fmt.Errorf("invalid AskUserQuestion input: missing questions array")
	}

	var responses []string

	for i, q := range questions {
		qMap, ok := q.(map[string]interface{})
		if !ok {
			continue
		}

		question, _ := qMap["question"].(string)
		header, _ := qMap["header"].(string)
		optionsRaw, _ := qMap["options"].([]interface{})

		// Parse options - can be strings or objects with label/description
		options := parseQuestionOptions(optionsRaw)

		p.renderer.QuestionWithOptions(question, header, options)

		fmt.Printf("\nYour answer: ")
		response, err := p.reader.ReadString('\n')
		if err != nil {
			return fmt.Errorf("failed to read user input: %w", err)
		}
		response = strings.TrimSpace(response)

		// Handle numeric selection
		if len(options) > 0 {
			if idx := parseOptionIndex(response, len(options)); idx >= 0 {
				response = options[idx].Label
			}
		}

		responses = append(responses, response)
		fmt.Printf("  [Q%d] Selected: %s\n", i+1, response)
	}

	// Format response message
	responseMsg := formatQuestionResponses(questions, responses)
	_, err := p.session.SendMessage(ctx, responseMsg)
	return err
}

// parseQuestionOptions parses options from the AskUserQuestion input.
// Options can be strings or objects with label/description fields.
func parseQuestionOptions(optionsRaw []interface{}) []QuestionOption {
	options := make([]QuestionOption, 0, len(optionsRaw))
	for _, opt := range optionsRaw {
		switch o := opt.(type) {
		case string:
			options = append(options, QuestionOption{Label: o})
		case map[string]interface{}:
			label, _ := o["label"].(string)
			desc, _ := o["description"].(string)
			if label != "" {
				options = append(options, QuestionOption{Label: label, Description: desc})
			}
		}
	}
	return options
}

// handleExitPlanMode handles the ExitPlanMode tool call.
// Returns (done, error) where done=true means the session should end.
func (p *PlannerWrapper) handleExitPlanMode(ctx context.Context, input map[string]interface{}) (bool, error) {
	p.renderer.PlanComplete(input)

	fmt.Println("\n" + strings.Repeat("─", 60))
	fmt.Println("What would you like to do?")
	fmt.Println("  1. Execute the plan")
	fmt.Println("  2. Export plan to markdown")
	fmt.Println("  3. Continue refining (optional: add feedback)")
	fmt.Println()
	fmt.Print("Enter choice (1-3) or feedback: ")

	choice, err := p.reader.ReadString('\n')
	if err != nil {
		return false, fmt.Errorf("failed to read user choice: %w", err)
	}
	choice = strings.TrimSpace(choice)

	switch choice {
	case "1":
		// Execute the plan
		fmt.Println("\n→ Executing plan...")
		_, err := p.session.SendMessage(ctx, "Please proceed with executing the plan.")
		return false, err

	case "2":
		// Export to markdown
		filename := fmt.Sprintf("plan-%d.md", time.Now().Unix())
		content := p.planText.String()
		if err := os.WriteFile(filename, []byte(content), 0644); err != nil {
			return false, fmt.Errorf("failed to write plan file: %w", err)
		}
		fmt.Printf("\n✓ Plan exported to %s\n", filename)
		return true, nil

	case "3":
		// Continue refining - prompt for optional feedback
		fmt.Print("Enter feedback (or press Enter to continue): ")
		feedback, err := p.reader.ReadString('\n')
		if err != nil {
			return false, fmt.Errorf("failed to read feedback: %w", err)
		}
		feedback = strings.TrimSpace(feedback)

		msg := "Please continue refining the plan."
		if feedback != "" {
			msg = fmt.Sprintf("Please refine the plan with this feedback: %s", feedback)
		}
		_, err = p.session.SendMessage(ctx, msg)
		return false, err

	default:
		// Treat any other input as direct feedback
		msg := fmt.Sprintf("Please refine the plan with this feedback: %s", choice)
		_, err := p.session.SendMessage(ctx, msg)
		return false, err
	}
}

// parseOptionIndex parses a 1-based numeric option selection.
// Returns -1 if not a valid option number.
func parseOptionIndex(input string, numOptions int) int {
	var idx int
	if _, err := fmt.Sscanf(input, "%d", &idx); err != nil {
		return -1
	}
	if idx < 1 || idx > numOptions {
		return -1
	}
	return idx - 1
}

// formatQuestionResponses formats question responses for sending back to Claude.
func formatQuestionResponses(questions []interface{}, responses []string) string {
	var sb strings.Builder
	sb.WriteString("User responses:\n")

	for i, q := range questions {
		if i >= len(responses) {
			break
		}
		qMap, ok := q.(map[string]interface{})
		if !ok {
			continue
		}
		question, _ := qMap["question"].(string)
		sb.WriteString(fmt.Sprintf("- Q: %s\n  A: %s\n", question, responses[i]))
	}

	return sb.String()
}

// RecordingPath returns the path to the session recording directory.
func (p *PlannerWrapper) RecordingPath() string {
	if p.session != nil {
		return p.session.RecordingPath()
	}
	return ""
}

// PlanText returns the accumulated plan text.
func (p *PlannerWrapper) PlanText() string {
	return p.planText.String()
}
