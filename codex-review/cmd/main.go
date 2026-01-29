// codex-review is a simple code review CLI using the Codex SDK.
package main

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"flag"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"

	"github.com/mzhaom/claude-cli-protocol/codex-review/reviewer"
	"github.com/mzhaom/claude-cli-protocol/sdks/golang/codex"
)

func main() {
	model := flag.String("model", "gpt-5.2-codex", "Model to use: gpt-5.2-codex, o4-mini, gpt-4o")
	workDir := flag.String("dir", "", "Working directory (defaults to current directory)")
	goal := flag.String("goal", "", "Goal/purpose of the changes on this branch")
	sessionLog := flag.String("session-log", "", "Path to write session log (default: ~/.codex-reviewer/<session-id>.jsonl)")
	noSessionLog := flag.Bool("no-session-log", false, "Disable session logging")
	verbose := flag.Bool("verbose", false, "Show progress information (tool use, tokens, etc.)")
	noColor := flag.Bool("no-color", false, "Disable ANSI color codes")
	approvalPolicy := flag.String("approval", "on-failure", "Tool approval policy: untrusted, on-failure, on-request, never")
	jsonOutput := flag.Bool("json", false, "Output review as structured JSON")

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [flags]\n\n", os.Args[0])
		fmt.Fprintln(os.Stderr, "AI-powered code review for changes on the current branch.")
		fmt.Fprintln(os.Stderr, "\nFlags:")
		flag.PrintDefaults()
		fmt.Fprintln(os.Stderr, "\nApproval policies:")
		fmt.Fprintln(os.Stderr, "  untrusted   - require approval for all tool executions")
		fmt.Fprintln(os.Stderr, "  on-failure  - auto-approve unless command fails (default)")
		fmt.Fprintln(os.Stderr, "  on-request  - approve on explicit request")
		fmt.Fprintln(os.Stderr, "  never       - auto-approve everything (use with caution)")
		fmt.Fprintln(os.Stderr, "\nExamples:")
		fmt.Fprintln(os.Stderr, "  codex-review -goal \"add user authentication\"")
		fmt.Fprintln(os.Stderr, "  codex-review -approval never -goal \"trusted repo review\"")
		fmt.Fprintln(os.Stderr, "  codex-review -verbose -no-color -goal \"review my changes\"")
	}
	flag.Parse()

	if *workDir == "" {
		var err error
		*workDir, err = os.Getwd()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error getting working directory: %v\n", err)
			os.Exit(1)
		}
	}

	// Set up session log path (default: ~/.codex-reviewer/<session-id>.jsonl)
	sessionLogPath := *sessionLog
	if !*noSessionLog && sessionLogPath == "" {
		sessionLogPath = getDefaultSessionLogPath()
	}

	// Build the review prompt
	var prompt string
	if *jsonOutput {
		prompt = reviewer.BuildJSONPrompt(*goal)
	} else {
		prompt = reviewer.BuildPrompt(*goal)
	}

	// Create reviewer
	rev := reviewer.New(reviewer.Config{
		Model:          *model,
		WorkDir:        *workDir,
		Goal:           *goal,
		SessionLogPath: sessionLogPath,
		Verbose:        *verbose,
		NoColor:        *noColor,
		ApprovalPolicy: codex.ApprovalPolicy(*approvalPolicy),
		JSONOutput:     *jsonOutput,
	})

	// Setup context with signal handling
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		<-sigChan
		fmt.Fprintln(os.Stderr, "\nInterrupted, shutting down...")
		cancel()
	}()

	// Start reviewer
	if err := rev.Start(ctx); err != nil {
		fmt.Fprintf(os.Stderr, "Error starting reviewer: %v\n", err)
		os.Exit(1)
	}
	defer rev.Stop()

	// Run review
	if err := rev.Review(ctx, prompt); err != nil {
		if ctx.Err() != nil {
			os.Exit(0)
		}
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// getDefaultSessionLogPath returns the default session log path in ~/.codex-reviewer/
func getDefaultSessionLogPath() string {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return "" // Fall back to no logging if we can't get home dir
	}

	logDir := filepath.Join(homeDir, ".codex-reviewer")
	if err := os.MkdirAll(logDir, 0755); err != nil {
		fmt.Fprintf(os.Stderr, "Warning: could not create session log directory: %v\n", err)
		return ""
	}

	// Generate session ID from timestamp + random suffix to avoid collisions
	timestamp := time.Now().Format("20060102-150405")
	suffix := randomHex(4)
	return filepath.Join(logDir, timestamp+"-"+suffix+".jsonl")
}

// randomHex returns a random hex string of the specified byte length.
func randomHex(n int) string {
	b := make([]byte, n)
	rand.Read(b)
	return hex.EncodeToString(b)
}
