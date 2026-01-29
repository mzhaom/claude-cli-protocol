// Package reviewer provides a simple wrapper around the Codex SDK for code review.
package reviewer

import (
	"context"
	"fmt"
	"io"
	"os"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/codex"
)

// Config holds reviewer configuration.
type Config struct {
	Model          string
	WorkDir        string
	Goal           string
	SessionLogPath string              // Path to write session log (JSON messages)
	Verbose        bool                // Show progress information (tool use, etc.)
	ApprovalPolicy codex.ApprovalPolicy // Tool approval policy (default: on-failure)
}

// BuildPrompt creates the review prompt from the config.
func BuildPrompt(goal string) string {
	goalText := goal
	if goalText == "" {
		goalText = "(not specified)"
	}

	return fmt.Sprintf(`You are experienced software engineer, reviewing changes by another engineer on this branch (this is using git worktree). The main goal of the change on this branch is %s.

Focus on these areas:
- Is the implementation correct? Is there any gap that should be addressed.
- Does it provide sufficient test coverage about the code path it touched.
- maintainability. also look at code around it, is there any code duplication that can be avoided.
- developer experience.
- performance.
- security.

When you flag an issue, provide a short, direct explanation and cite the affected file and line range.
Prioritize severe issues and avoid nit-level comments unless they block understanding of the diff.
After listing findings, produce an overall correctness verdict ("patch is correct" or "patch is incorrect") with a concise justification and a confidence score between 0 and 1.
Ensure that file citations and line numbers are exactly correct using the tools available; if they are incorrect your comments will be rejected.`, goalText)
}

// Reviewer wraps the Codex SDK for code review operations.
type Reviewer struct {
	client  *codex.Client
	config  Config
	output  io.Writer
	verbose io.Writer // Writer for verbose output (stderr)
}

// New creates a new Reviewer with the given config.
func New(config Config) *Reviewer {
	if config.Model == "" {
		config.Model = "gpt-5.2-codex"
	}
	if config.ApprovalPolicy == "" {
		config.ApprovalPolicy = codex.ApprovalPolicyOnFailure
	}
	r := &Reviewer{
		config: config,
		output: os.Stdout,
	}
	if config.Verbose {
		r.verbose = os.Stderr
	}
	return r
}

// SetOutput sets the output writer for streaming responses.
func (r *Reviewer) SetOutput(w io.Writer) {
	r.output = w
}

// Start initializes the Codex client.
func (r *Reviewer) Start(ctx context.Context) error {
	opts := []codex.ClientOption{
		codex.WithClientName("codex-review"),
		codex.WithClientVersion("1.0.0"),
	}
	if r.config.SessionLogPath != "" {
		opts = append(opts, codex.WithSessionLogPath(r.config.SessionLogPath))
	}
	r.client = codex.NewClient(opts...)
	return r.client.Start(ctx)
}

// Stop shuts down the Codex client.
func (r *Reviewer) Stop() error {
	if r.client != nil {
		return r.client.Stop()
	}
	return nil
}

// Review sends a review prompt and streams the response to output.
func (r *Reviewer) Review(ctx context.Context, prompt string) error {
	r.logVerbose("Creating thread with model %s...\n", r.config.Model)

	// Create thread
	thread, err := r.client.CreateThread(ctx,
		codex.WithModel(r.config.Model),
		codex.WithWorkDir(r.config.WorkDir),
		codex.WithApprovalPolicy(r.config.ApprovalPolicy),
	)
	if err != nil {
		return fmt.Errorf("failed to create thread: %w", err)
	}

	r.logVerbose("Waiting for thread to be ready...\n")

	// Wait for thread to be ready
	if err := r.waitForThreadReady(ctx, thread.ID()); err != nil {
		return fmt.Errorf("thread not ready: %w", err)
	}

	r.logVerbose("Thread ready, sending review prompt...\n")

	// Send the review prompt
	_, err = thread.SendMessage(ctx, prompt)
	if err != nil {
		return fmt.Errorf("failed to send message: %w", err)
	}

	// Stream the response
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case event, ok := <-r.client.Events():
			if !ok {
				return nil
			}

			switch e := event.(type) {
			case codex.TextDeltaEvent:
				if e.ThreadID == thread.ID() {
					fmt.Fprint(r.output, e.Delta)
				}
			case codex.ItemStartedEvent:
				if e.ThreadID == thread.ID() {
					r.logVerbose("[%s] started\n", e.ItemType)
				}
			case codex.ItemCompletedEvent:
				if e.ThreadID == thread.ID() {
					r.logVerbose("[%s] completed\n", e.ItemType)
				}
			case codex.TurnCompletedEvent:
				if e.ThreadID == thread.ID() {
					fmt.Fprintln(r.output)
					r.logVerbose("Review completed (tokens: %d input, %d output)\n",
						e.Usage.InputTokens, e.Usage.OutputTokens)
					return nil
				}
			case codex.ErrorEvent:
				return fmt.Errorf("error: %v", e.Error)
			}
		}
	}
}

// logVerbose logs a message if verbose mode is enabled.
func (r *Reviewer) logVerbose(format string, args ...interface{}) {
	if r.verbose != nil {
		fmt.Fprintf(r.verbose, format, args...)
	}
}

// waitForThreadReady waits for the thread to be ready for messages.
func (r *Reviewer) waitForThreadReady(ctx context.Context, threadID string) error {
	thread, ok := r.client.GetThread(threadID)
	if !ok {
		return fmt.Errorf("thread not found")
	}

	// If already ready, return
	if thread.State() == codex.ThreadStateReady {
		return nil
	}

	// Wait for ready event
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case event, ok := <-r.client.Events():
			if !ok {
				return fmt.Errorf("event channel closed")
			}
			if e, ok := event.(codex.ThreadReadyEvent); ok && e.ThreadID == threadID {
				return nil
			}
		}
	}
}
