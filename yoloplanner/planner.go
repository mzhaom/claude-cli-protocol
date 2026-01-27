// Package yoloplanner provides a simple wrapper over claude-cli for planner mode.
//
// # Permission Mode and Setup
//
// This package demonstrates a specific permission handling approach:
//
//  1. Start with PermissionModeDefault - The CLI starts without --permission-mode plan
//     or --dangerously-skip-permissions flags, allowing fine-grained permission control.
//
//  2. Use AllowAllPermissionHandler - Instead of --dangerously-skip-permissions, we
//     register a permission handler that auto-approves all tool execution requests.
//     This gives us programmatic control over permissions (could be extended to
//     selectively approve/deny based on tool name, input, etc.).
//
//  3. Switch to plan mode via control message - After the session starts, we send a
//     set_permission_mode control request to switch to plan mode. This approach was
//     validated in experiments/plan_mode_analysis/ANALYSIS.md (Experiment C).
//
// # Limitations
//
//   - The AllowAllPermissionHandler approves everything - for production use, you may
//     want a more selective handler that reviews dangerous operations.
//   - Control message responses are not currently awaited - we fire-and-forget the
//     mode switch. The CLI processes it before the first user message.
package yoloplanner

import (
	"bufio"
	"context"
	"fmt"
	"os"
	"strings"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude"
)

// readLineWithContext reads a line from stdin with context cancellation support.
func (p *PlannerWrapper) readLineWithContext(ctx context.Context) (string, error) {
	select {
	case <-ctx.Done():
		return "", ctx.Err()
	case line := <-p.inputCh:
		return line, nil
	}
}

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

	// Verbose enables detailed tool result output. When false, only errors are shown.
	Verbose bool

	// Simple enables non-interactive mode: auto-answers questions with first option
	// and exports plan to markdown on completion.
	Simple bool

	// Prompt is the initial prompt, used for generating output filenames in simple mode.
	Prompt string
}

// PlannerWrapper wraps a claude.Session with planner-specific logic.
type PlannerWrapper struct {
	session  *claude.Session
	renderer *Renderer
	reader   *bufio.Reader
	config   Config

	// Path to the plan file written by Claude (detected from Write tool calls)
	planFilePath string

	// State tracking
	waitingForUserInput bool // True when waiting for AskUserQuestion or ExitPlanMode response

	// Channel for context-cancelable stdin reads
	inputCh chan string
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
		renderer: NewRenderer(os.Stdout, config.Verbose),
		reader:   bufio.NewReader(os.Stdin),
		inputCh:  make(chan string),
	}
}

// Start initializes and starts the underlying claude session.
//
// The session is configured with:
//   - PermissionModeDefault: Start without plan mode flag, allowing control message switch
//   - AllowAllPermissionHandler: Auto-approve all tool executions (replaces --dangerously-skip-permissions)
//   - Recording enabled: All messages are recorded for debugging/replay
//
// After starting, we switch to plan mode via a control message. This approach
// (vs starting directly with --permission-mode plan) demonstrates dynamic
// permission mode changes through the protocol.
func (p *PlannerWrapper) Start(ctx context.Context) error {
	opts := []claude.SessionOption{
		claude.WithModel(p.config.Model),
		// Start in default mode - we'll switch to plan mode via control message
		claude.WithPermissionMode(claude.PermissionModeDefault),
		// Auto-approve all permissions instead of using --dangerously-skip-permissions
		claude.WithPermissionHandler(claude.AllowAllPermissionHandler()),
		claude.WithRecording(p.config.RecordingDir),
	}

	if p.config.WorkDir != "" {
		opts = append(opts, claude.WithWorkDir(p.config.WorkDir))
	}

	if p.config.SystemPrompt != "" {
		opts = append(opts, claude.WithSystemPrompt(p.config.SystemPrompt))
	}

	p.session = claude.NewSession(opts...)

	// Print CLI flags that will be used
	if args, err := p.session.CLIArgs(); err == nil {
		fmt.Fprintf(os.Stderr, "Starting claude with flags: %s\n", strings.Join(args, " "))
	}

	if err := p.session.Start(ctx); err != nil {
		return err
	}

	// Start persistent stdin reader goroutine
	go func() {
		for {
			line, err := p.reader.ReadString('\n')
			if err != nil {
				return // EOF or error, stop reading
			}
			p.inputCh <- strings.TrimSpace(line)
		}
	}()

	// Switch to plan mode via control message (per experiments/plan_mode_analysis/ANALYSIS.md)
	return p.session.SetPermissionMode(ctx, claude.PermissionModePlan)
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

	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case event, ok := <-p.session.Events():
			if !ok {
				return nil
			}
			done, err := p.handleEvent(ctx, event)
			if err != nil {
				return err
			}
			if done {
				return nil
			}
		}
	}
}

// handleEvent processes a single event and returns (done, error).
func (p *PlannerWrapper) handleEvent(ctx context.Context, event claude.Event) (bool, error) {
	switch e := event.(type) {
	case claude.ReadyEvent:
		p.renderer.Status(fmt.Sprintf("Session started: %s (model: %s)", e.Info.SessionID, e.Info.Model))

	case claude.TextEvent:
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

		// Track plan file writes
		p.trackPlanFileWrite(e.Name, e.Input)

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

		// In simple mode, auto-select first option but show full question
		if p.config.Simple && len(options) > 0 {
			response := options[0].Label
			p.renderer.QuestionAutoAnswer(question, header, options, 0)
			responses = append(responses, response)
			continue
		}

		p.renderer.QuestionWithOptions(question, header, options)

		fmt.Printf("\nYour answer: ")
		response, err := p.readLineWithContext(ctx)
		if err != nil {
			return fmt.Errorf("failed to read user input: %w", err)
		}

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

	// In simple mode, auto-export and exit
	if p.config.Simple {
		filename := p.generatePlanFilename()
		return p.exportPlanAndExit(ctx, filename)
	}

	fmt.Println("\n" + strings.Repeat("─", 60))
	fmt.Println("What would you like to do?")
	fmt.Println("  1. Execute the plan")
	fmt.Println("  2. Export plan to markdown")
	fmt.Println("  3. Continue refining (optional: add feedback)")
	fmt.Println()
	fmt.Print("Enter choice (1-3) or feedback: ")

	choice, err := p.readLineWithContext(ctx)
	if err != nil {
		return false, fmt.Errorf("failed to read user choice: %w", err)
	}

	switch choice {
	case "1":
		// Execute the plan
		fmt.Println("\n→ Executing plan...")
		_, err := p.session.SendMessage(ctx, "Please proceed with executing the plan.")
		return false, err

	case "2":
		// Export to markdown
		filename := p.generatePlanFilename()
		return p.exportPlanAndExit(ctx, filename)

	case "3":
		// Continue refining - prompt for optional feedback
		fmt.Print("Enter feedback (or press Enter to continue): ")
		feedback, err := p.readLineWithContext(ctx)
		if err != nil {
			return false, fmt.Errorf("failed to read feedback: %w", err)
		}

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

// trackPlanFileWrite detects when Claude writes to a plan file in .claude/plans/.
func (p *PlannerWrapper) trackPlanFileWrite(toolName string, input map[string]interface{}) {
	if toolName != "Write" {
		return
	}
	filePath, ok := input["file_path"].(string)
	if !ok {
		return
	}
	if strings.Contains(filePath, ".claude/plans/") && strings.HasSuffix(filePath, ".md") {
		p.planFilePath = filePath
	}
}

// exportPlanAndExit exports the plan to the given filename and returns done=true.
// If no plan file was detected, it asks Claude to write the plan first.
// Returns (done, error) where done=true means the session should end.
func (p *PlannerWrapper) exportPlanAndExit(ctx context.Context, filename string) (bool, error) {
	if p.planFilePath == "" {
		// No plan file detected - ask Claude to write it
		p.renderer.Status("No plan file detected, requesting export...")
		_, err := p.session.SendMessage(ctx, fmt.Sprintf("Please write your complete plan to %s", filename))
		return false, err
	}

	// Read and export the actual plan file written by Claude
	if err := p.exportPlanToFile(filename); err != nil {
		return false, err
	}
	fmt.Printf("\n✓ Plan exported to %s\n", filename)
	return true, nil
}

// exportPlanToFile copies the plan file to the specified destination.
func (p *PlannerWrapper) exportPlanToFile(destPath string) error {
	if p.planFilePath == "" {
		return fmt.Errorf("no plan file path set")
	}
	data, err := os.ReadFile(p.planFilePath)
	if err != nil {
		return fmt.Errorf("failed to read plan file %s: %w", p.planFilePath, err)
	}
	if err := os.WriteFile(destPath, data, 0644); err != nil {
		return fmt.Errorf("failed to write export file %s: %w", destPath, err)
	}
	return nil
}

// PlanFilePath returns the path to the detected plan file.
func (p *PlannerWrapper) PlanFilePath() string {
	return p.planFilePath
}

// generatePlanFilename creates a filename from the prompt.
// Example: "Create a hello world program" -> "plan-create-a-hello-world.md"
func (p *PlannerWrapper) generatePlanFilename() string {
	prompt := p.config.Prompt
	// Lowercase and replace non-alphanumeric with hyphens
	prompt = strings.ToLower(prompt)
	var result strings.Builder
	result.WriteString("plan-")
	wordCount := 0
	inWord := false
	for _, r := range prompt {
		if (r >= 'a' && r <= 'z') || (r >= '0' && r <= '9') {
			result.WriteRune(r)
			inWord = true
		} else if inWord {
			result.WriteRune('-')
			inWord = false
			wordCount++
			if wordCount >= 5 { // Limit to ~5 words
				break
			}
		}
	}
	s := strings.TrimSuffix(result.String(), "-")
	return s + ".md"
}
