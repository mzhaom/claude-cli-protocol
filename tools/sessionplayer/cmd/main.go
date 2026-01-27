// sessionplayer plays back recorded Claude session messages with colored output.
//
// Usage:
//
//	sessionplayer [flags] <recording_dir>
//
// Examples:
//
//	sessionplayer .planner-sessions/session-abc123-1234567890/
//	sessionplayer -verbose experiments/plan_mode_analysis/recordings/experiment_a/session-*/
package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/mzhaom/claude-cli-protocol/tools/sessionplayer"
)

func main() {
	verbose := flag.Bool("verbose", false, "Show all tool results (errors are always shown)")
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [flags] <recording_dir>\n\n", os.Args[0])
		fmt.Fprintln(os.Stderr, "Play back recorded Claude session messages with colored output.")
		fmt.Fprintln(os.Stderr, "\nFlags:")
		flag.PrintDefaults()
		fmt.Fprintln(os.Stderr, "\nExamples:")
		fmt.Fprintln(os.Stderr, "  sessionplayer .planner-sessions/session-abc123-1234567890/")
		fmt.Fprintln(os.Stderr, "  sessionplayer -verbose experiments/recordings/session-*/")
	}
	flag.Parse()

	if flag.NArg() < 1 {
		fmt.Fprintln(os.Stderr, "Error: no recording directory provided")
		flag.Usage()
		os.Exit(1)
	}

	// Handle glob patterns - the shell expands them, so we might get multiple args
	dirs := flag.Args()

	player := sessionplayer.NewPlayer(os.Stdout, *verbose)

	for i, dir := range dirs {
		// Check if messages.jsonl exists (required file)
		if _, err := os.Stat(filepath.Join(dir, "messages.jsonl")); os.IsNotExist(err) {
			fmt.Fprintf(os.Stderr, "Warning: %s does not contain messages.jsonl, skipping\n", dir)
			continue
		}

		if len(dirs) > 1 {
			if i > 0 {
				fmt.Println() // Separator between recordings
			}
			fmt.Fprintf(os.Stdout, "=== Playing: %s ===\n\n", dir)
		}

		if err := player.Play(dir); err != nil {
			fmt.Fprintf(os.Stderr, "Error playing %s: %v\n", dir, err)
			os.Exit(1)
		}
	}
}
