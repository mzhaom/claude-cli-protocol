// yoloplanner is a simple wrapper over claude-cli for planner mode.
//
// Usage:
//
//	yoloplanner [flags] <prompt>
//
// Examples:
//
//	yoloplanner "Create a hello world Go program"
//	yoloplanner -model opus "Implement a REST API"
//	echo "Add tests" | yoloplanner
package main

import (
	"bufio"
	"context"
	"flag"
	"fmt"
	"os"
	"os/signal"
	"strings"
	"syscall"

	"github.com/mzhaom/claude-cli-protocol/yoloplanner"
)

func main() {
	// Parse flags
	model := flag.String("model", "sonnet", "Model to use: haiku, sonnet, opus")
	workDir := flag.String("dir", "", "Working directory (defaults to current directory)")
	recordDir := flag.String("record", ".planner-sessions", "Directory for session recordings")
	systemPrompt := flag.String("system", "", "Custom system prompt")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [flags] <prompt>\n\n", os.Args[0])
		fmt.Fprintln(os.Stderr, "A simple wrapper over claude-cli for planner mode.")
		fmt.Fprintln(os.Stderr, "\nFlags:")
		flag.PrintDefaults()
		fmt.Fprintln(os.Stderr, "\nExamples:")
		fmt.Fprintln(os.Stderr, "  yoloplanner \"Create a hello world Go program\"")
		fmt.Fprintln(os.Stderr, "  yoloplanner -model opus \"Implement a REST API\"")
		fmt.Fprintln(os.Stderr, "  echo \"Add tests\" | yoloplanner")
	}
	flag.Parse()

	// Get prompt from args or stdin
	prompt := strings.Join(flag.Args(), " ")
	if prompt == "" {
		prompt = readFromStdin()
	}
	if prompt == "" {
		fmt.Fprintln(os.Stderr, "Error: no prompt provided")
		flag.Usage()
		os.Exit(1)
	}

	// Set default working directory
	if *workDir == "" {
		var err error
		*workDir, err = os.Getwd()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error getting working directory: %v\n", err)
			os.Exit(1)
		}
	}

	// Create config
	config := yoloplanner.Config{
		Model:        *model,
		WorkDir:      *workDir,
		RecordingDir: *recordDir,
		SystemPrompt: *systemPrompt,
	}

	// Create planner wrapper
	planner := yoloplanner.NewPlannerWrapper(config)

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

	// Start the session
	if err := planner.Start(ctx); err != nil {
		fmt.Fprintf(os.Stderr, "Error starting session: %v\n", err)
		os.Exit(1)
	}
	defer planner.Stop()

	// Run the planner
	if err := planner.Run(ctx, prompt); err != nil {
		if ctx.Err() != nil {
			// Context cancelled, exit gracefully
			os.Exit(0)
		}
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Print recording path
	if path := planner.RecordingPath(); path != "" {
		fmt.Fprintf(os.Stderr, "\nSession recorded to: %s\n", path)
	}
}

// readFromStdin reads input from stdin if available.
func readFromStdin() string {
	stat, _ := os.Stdin.Stat()
	if (stat.Mode() & os.ModeCharDevice) != 0 {
		// stdin is a terminal, not piped input
		return ""
	}

	var lines []string
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return strings.Join(lines, "\n")
}
