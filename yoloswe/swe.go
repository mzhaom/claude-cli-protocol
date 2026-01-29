package yoloswe

import (
	"context"
	"fmt"
	"io"
	"os"
	"strings"
	"time"

	"github.com/mzhaom/claude-cli-protocol/codex-review/reviewer"
)

// ExitReason indicates why the builder-reviewer loop exited.
type ExitReason string

const (
	ExitReasonAccepted       ExitReason = "accepted"       // Reviewer approved the changes
	ExitReasonBudgetExceeded ExitReason = "budget"         // Builder budget limit reached
	ExitReasonTimeExceeded   ExitReason = "timeout"        // Time limit reached
	ExitReasonMaxIterations  ExitReason = "max_iterations" // Safety iteration limit reached
	ExitReasonError          ExitReason = "error"          // Unrecoverable error
	ExitReasonInterrupt      ExitReason = "interrupt"      // User interrupted (Ctrl+C)
)

// Config holds yoloswe configuration.
type Config struct {
	// Builder settings
	BuilderModel    string
	BuilderWorkDir  string
	RecordingDir    string
	SystemPrompt    string
	RequireApproval bool // Require user approval for tool executions (default: auto-approve)

	// Reviewer settings
	ReviewerModel string
	Goal          string // Goal description for reviewer context

	// Limits
	MaxBudgetUSD   float64 // Max USD to spend on builder session
	MaxTimeSeconds int     // Max wall-clock seconds
	MaxIterations  int     // Max builder-reviewer iterations (safety limit)

	// Output settings
	Verbose bool
}

// Stats tracks cumulative statistics for the SWE loop.
type Stats struct {
	BuilderCostUSD   float64
	BuilderTokensIn  int
	BuilderTokensOut int
	ReviewerTokensIn  int64
	ReviewerTokensOut int64
	IterationCount   int
	TotalDurationMs  int64
	ExitReason       ExitReason
}

// ReviewVerdict represents the reviewer's decision.
type ReviewVerdict struct {
	Accepted bool
	Feedback string
}

// SWEWrapper orchestrates the builder-reviewer loop.
type SWEWrapper struct {
	builder  *BuilderSession
	reviewer *reviewer.Reviewer
	config   Config
	stats    Stats
	output   io.Writer
}

// New creates a new SWEWrapper with the given configuration.
func New(config Config) *SWEWrapper {
	// Apply defaults
	if config.BuilderModel == "" {
		config.BuilderModel = "sonnet"
	}
	if config.ReviewerModel == "" {
		config.ReviewerModel = "gpt-5.2-codex"
	}
	if config.RecordingDir == "" {
		config.RecordingDir = ".swe-sessions"
	}
	if config.MaxBudgetUSD <= 0 {
		config.MaxBudgetUSD = 5.0
	}
	if config.MaxTimeSeconds <= 0 {
		config.MaxTimeSeconds = 600 // 10 minutes
	}
	if config.MaxIterations <= 0 {
		config.MaxIterations = 10
	}

	output := os.Stdout

	// Create builder session
	builderConfig := BuilderConfig{
		Model:           config.BuilderModel,
		WorkDir:         config.BuilderWorkDir,
		RecordingDir:    config.RecordingDir,
		SystemPrompt:    config.SystemPrompt,
		Verbose:         config.Verbose,
		RequireApproval: config.RequireApproval,
	}
	builder := NewBuilderSession(builderConfig, output)

	// Create reviewer
	reviewerConfig := reviewer.Config{
		Model:   config.ReviewerModel,
		WorkDir: config.BuilderWorkDir,
		Goal:    config.Goal,
		Verbose: config.Verbose,
	}
	rev := reviewer.New(reviewerConfig)

	return &SWEWrapper{
		builder:  builder,
		reviewer: rev,
		config:   config,
		output:   output,
	}
}

// Run executes the builder-reviewer loop with the given initial prompt.
func (s *SWEWrapper) Run(ctx context.Context, prompt string) error {
	startTime := time.Now()

	// Start builder session
	fmt.Fprintln(s.output, "\n=== Starting Builder Session ===")
	if err := s.builder.Start(ctx); err != nil {
		return fmt.Errorf("failed to start builder: %w", err)
	}
	defer s.builder.Stop()

	// Start reviewer
	fmt.Fprintln(s.output, "\n=== Starting Reviewer Session ===")
	if err := s.reviewer.Start(ctx); err != nil {
		return fmt.Errorf("failed to start reviewer: %w", err)
	}
	defer s.reviewer.Stop()

	currentMessage := prompt
	isFirstReview := true

	for iteration := 1; ; iteration++ {
		s.stats.IterationCount = iteration

		// Check time limit before iteration
		elapsed := time.Since(startTime)
		if elapsed.Seconds() >= float64(s.config.MaxTimeSeconds) {
			s.stats.ExitReason = ExitReasonTimeExceeded
			fmt.Fprintf(s.output, "\n=== Time limit reached (%.1fs) ===\n", elapsed.Seconds())
			break
		}

		// === Builder Phase ===
		fmt.Fprintf(s.output, "\n"+strings.Repeat("=", 60)+"\n")
		fmt.Fprintf(s.output, "=== Iteration %d: BUILDER ===\n", iteration)
		fmt.Fprintf(s.output, strings.Repeat("=", 60)+"\n\n")

		builderUsage, err := s.builder.RunTurn(ctx, currentMessage)
		if err != nil {
			if ctx.Err() == context.Canceled {
				s.stats.ExitReason = ExitReasonInterrupt
				return nil
			}
			s.stats.ExitReason = ExitReasonError
			return fmt.Errorf("builder error: %w", err)
		}

		// Update builder stats
		s.stats.BuilderCostUSD += builderUsage.CostUSD
		s.stats.BuilderTokensIn += builderUsage.InputTokens
		s.stats.BuilderTokensOut += builderUsage.OutputTokens

		// Check budget after builder turn
		if s.stats.BuilderCostUSD >= s.config.MaxBudgetUSD {
			s.stats.ExitReason = ExitReasonBudgetExceeded
			fmt.Fprintf(s.output, "\n=== Budget limit reached ($%.4f >= $%.4f) ===\n",
				s.stats.BuilderCostUSD, s.config.MaxBudgetUSD)
			break
		}

		// === Reviewer Phase ===
		fmt.Fprintf(s.output, "\n"+strings.Repeat("=", 60)+"\n")
		fmt.Fprintf(s.output, "=== Iteration %d: REVIEWER ===\n", iteration)
		fmt.Fprintf(s.output, strings.Repeat("=", 60)+"\n\n")

		var reviewResult *reviewer.ReviewResult
		if isFirstReview {
			reviewPrompt := s.buildInitialReviewPrompt()
			reviewResult, err = s.reviewer.ReviewWithResult(ctx, reviewPrompt)
			isFirstReview = false
		} else {
			reviewPrompt := s.buildFollowUpPrompt()
			reviewResult, err = s.reviewer.FollowUp(ctx, reviewPrompt)
		}

		if err != nil {
			if ctx.Err() == context.Canceled {
				s.stats.ExitReason = ExitReasonInterrupt
				return nil
			}
			s.stats.ExitReason = ExitReasonError
			return fmt.Errorf("reviewer error: %w", err)
		}

		// Update reviewer stats
		s.stats.ReviewerTokensIn += reviewResult.InputTokens
		s.stats.ReviewerTokensOut += reviewResult.OutputTokens

		// Parse verdict from response
		verdict := s.parseVerdict(reviewResult.ResponseText)

		if verdict.Accepted {
			s.stats.ExitReason = ExitReasonAccepted
			fmt.Fprintln(s.output, "\n=== Reviewer ACCEPTED the changes ===")
			break
		}

		// Check iteration limit
		if iteration >= s.config.MaxIterations {
			s.stats.ExitReason = ExitReasonMaxIterations
			fmt.Fprintf(s.output, "\n=== Max iterations reached (%d) ===\n", s.config.MaxIterations)
			break
		}

		// Format feedback for next iteration
		fmt.Fprintln(s.output, "\n=== Reviewer requested changes, continuing... ===")
		currentMessage = fmt.Sprintf(`The reviewer provided the following feedback on your changes:

%s

Please address this feedback and improve the implementation.`, verdict.Feedback)
	}

	s.stats.TotalDurationMs = time.Since(startTime).Milliseconds()
	return nil
}

// buildInitialReviewPrompt creates the prompt for the first review.
func (s *SWEWrapper) buildInitialReviewPrompt() string {
	return reviewer.BuildPrompt(s.config.Goal)
}

// buildFollowUpPrompt creates the prompt for follow-up reviews.
func (s *SWEWrapper) buildFollowUpPrompt() string {
	return `The builder has made additional changes based on your previous feedback.

Please review the current state of the changes and provide an updated verdict.

Focus on whether the previous issues have been addressed correctly.
After your analysis, produce an overall correctness verdict ("patch is correct" or "patch is incorrect") with a concise justification.`
}

// parseVerdict extracts the acceptance decision from the reviewer's response.
func (s *SWEWrapper) parseVerdict(text string) *ReviewVerdict {
	upper := strings.ToUpper(text)

	// Match the codex-review prompt which asks for "patch is correct" or "patch is incorrect"
	if strings.Contains(upper, "PATCH IS CORRECT") {
		return &ReviewVerdict{Accepted: true}
	}

	// Also accept explicit VERDICT: ACCEPTED
	if strings.Contains(upper, "VERDICT: ACCEPTED") {
		return &ReviewVerdict{Accepted: true}
	}

	return &ReviewVerdict{
		Accepted: false,
		Feedback: text,
	}
}

// PrintSummary prints the final statistics.
func (s *SWEWrapper) PrintSummary() {
	fmt.Fprintln(s.output, "\n"+strings.Repeat("=", 60))
	fmt.Fprintln(s.output, "YOLOSWE SESSION SUMMARY")
	fmt.Fprintln(s.output, strings.Repeat("-", 60))
	fmt.Fprintf(s.output, "Exit reason:        %s\n", s.stats.ExitReason)
	fmt.Fprintf(s.output, "Iterations:         %d\n", s.stats.IterationCount)
	fmt.Fprintf(s.output, "Duration:           %.1fs\n", float64(s.stats.TotalDurationMs)/1000)
	fmt.Fprintln(s.output, strings.Repeat("-", 60))
	fmt.Fprintln(s.output, "Builder:")
	fmt.Fprintf(s.output, "  Cost:             $%.4f\n", s.stats.BuilderCostUSD)
	fmt.Fprintf(s.output, "  Input tokens:     %d\n", s.stats.BuilderTokensIn)
	fmt.Fprintf(s.output, "  Output tokens:    %d\n", s.stats.BuilderTokensOut)
	fmt.Fprintln(s.output, strings.Repeat("-", 60))
	fmt.Fprintln(s.output, "Reviewer:")
	fmt.Fprintf(s.output, "  Input tokens:     %d\n", s.stats.ReviewerTokensIn)
	fmt.Fprintf(s.output, "  Output tokens:    %d\n", s.stats.ReviewerTokensOut)
	fmt.Fprintln(s.output, strings.Repeat("=", 60))
}

// Stats returns the current statistics.
func (s *SWEWrapper) Stats() Stats {
	return s.stats
}
