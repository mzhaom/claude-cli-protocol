// Command yoloswe runs a builder-reviewer loop for software engineering tasks.
package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"os/signal"
	"strings"
	"syscall"

	"github.com/mzhaom/claude-cli-protocol/yoloswe"
)

func main() {
	// Define flags
	builderModel := flag.String("builder-model", "sonnet", "Builder model: haiku, sonnet, opus")
	reviewerModel := flag.String("reviewer-model", "gpt-5.2-codex", "Reviewer model: gpt-5.2-codex, o4-mini")
	dir := flag.String("dir", "", "Working directory (default: current)")
	budget := flag.Float64("budget", 5.0, "Max USD for builder session")
	timeout := flag.Int("timeout", 600, "Max seconds")
	maxIterations := flag.Int("max-iterations", 10, "Max builder-reviewer iterations")
	goal := flag.String("goal", "", "Goal description for reviewer context")
	record := flag.String("record", ".swe-sessions", "Session recordings directory")
	verbose := flag.Bool("verbose", false, "Show detailed output")
	systemPrompt := flag.String("system", "", "Custom system prompt for builder")
	requireApproval := flag.Bool("require-approval", false, "Require user approval for tool executions (default: auto-approve)")

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: yoloswe [flags] <prompt>\n\n")
		fmt.Fprintf(os.Stderr, "yoloswe runs a builder-reviewer loop for software engineering tasks.\n")
		fmt.Fprintf(os.Stderr, "The builder (Claude) implements the task, and the reviewer (Codex) reviews.\n")
		fmt.Fprintf(os.Stderr, "The loop continues until the reviewer accepts or limits are reached.\n\n")
		fmt.Fprintf(os.Stderr, "Flags:\n")
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nExamples:\n")
		fmt.Fprintf(os.Stderr, "  yoloswe \"Add unit tests for the user service\"\n")
		fmt.Fprintf(os.Stderr, "  yoloswe -budget 10 -timeout 1800 \"Refactor the database layer\"\n")
		fmt.Fprintf(os.Stderr, "  yoloswe -builder-model opus \"Fix the authentication bug\"\n")
	}

	flag.Parse()

	// Get prompt from remaining args
	args := flag.Args()
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Error: prompt is required")
		flag.Usage()
		os.Exit(1)
	}
	prompt := strings.Join(args, " ")

	// Use prompt as goal if goal not specified
	goalText := *goal
	if goalText == "" {
		goalText = prompt
	}

	// Get working directory
	workDir := *dir
	if workDir == "" {
		var err error
		workDir, err = os.Getwd()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error getting working directory: %v\n", err)
			os.Exit(1)
		}
	}

	// Create config
	config := yoloswe.Config{
		BuilderModel:    *builderModel,
		BuilderWorkDir:  workDir,
		RecordingDir:    *record,
		SystemPrompt:    *systemPrompt,
		RequireApproval: *requireApproval,
		ReviewerModel:   *reviewerModel,
		Goal:            goalText,
		MaxBudgetUSD:    *budget,
		MaxTimeSeconds:  *timeout,
		MaxIterations:   *maxIterations,
		Verbose:         *verbose,
	}

	// Setup context with cancellation
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Handle signals
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		<-sigChan
		fmt.Fprintln(os.Stderr, "\nInterrupted, shutting down...")
		cancel()
	}()

	// Print configuration
	fmt.Println(strings.Repeat("=", 60))
	fmt.Println("YOLOSWE - Builder-Reviewer Loop")
	fmt.Println(strings.Repeat("-", 60))
	fmt.Printf("Builder model:  %s\n", config.BuilderModel)
	fmt.Printf("Reviewer model: %s\n", config.ReviewerModel)
	fmt.Printf("Working dir:    %s\n", config.BuilderWorkDir)
	fmt.Printf("Budget:         $%.2f\n", config.MaxBudgetUSD)
	fmt.Printf("Timeout:        %ds\n", config.MaxTimeSeconds)
	fmt.Printf("Max iterations: %d\n", config.MaxIterations)
	fmt.Println(strings.Repeat("-", 60))
	fmt.Printf("Prompt: %s\n", prompt)
	fmt.Println(strings.Repeat("=", 60))

	// Create and run SWE wrapper
	swe := yoloswe.New(config)

	if err := swe.Run(ctx, prompt); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		swe.PrintSummary()
		os.Exit(1)
	}

	swe.PrintSummary()

	// Exit with appropriate code based on result
	stats := swe.Stats()
	if stats.ExitReason == yoloswe.ExitReasonAccepted {
		os.Exit(0)
	}
	os.Exit(1)
}
