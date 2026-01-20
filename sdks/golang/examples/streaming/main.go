// Example: Event-driven streaming usage of the Claude SDK.
//
// This example demonstrates how to use the Events() channel to receive
// real-time streaming updates including text chunks, tool executions,
// and turn completions.
package main

import (
	"context"
	"fmt"
	"log"
	"time"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude"
)

func main() {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	// Create a new session
	session := claude.NewSession(
		claude.WithModel("haiku"),
		claude.WithPermissionMode(claude.PermissionModeBypass),
		claude.WithDisablePlugins(),
	)

	// Start the session
	fmt.Println("Starting Claude session...")
	if err := session.Start(ctx); err != nil {
		log.Fatalf("Failed to start session: %v", err)
	}
	defer session.Stop()

	// Send a message that will generate streaming text
	fmt.Println("Sending message...")
	if _, err := session.SendMessage(ctx, "Write a haiku about Go programming. Be creative!"); err != nil {
		log.Fatalf("SendMessage failed: %v", err)
	}

	fmt.Println("---Response---")

	// Collect and display events until turn completes
	// Note: ReadyEvent arrives with the first turn (CLI sends init after first message)
	for {
		select {
		case <-ctx.Done():
			log.Fatalf("Context cancelled: %v", ctx.Err())
		case event, ok := <-session.Events():
			if !ok {
				log.Fatal("Event channel closed")
			}

			switch e := event.(type) {
			case claude.ReadyEvent:
				fmt.Printf("[Ready] Session: %s, Model: %s\n\n", e.Info.SessionID, e.Info.Model)

			case claude.TextEvent:
				// Print text as it streams in (no newline for seamless output)
				fmt.Print(e.Text)

			case claude.ThinkingEvent:
				// Print thinking (extended thinking mode)
				fmt.Printf("[Thinking] %s\n", e.Thinking)

			case claude.ToolStartEvent:
				fmt.Printf("\n[Tool Started] %s (id: %s)\n", e.Name, e.ID)

			case claude.ToolCompleteEvent:
				fmt.Printf("[Tool Input Complete] %s: %v\n", e.Name, e.Input)

			case claude.CLIToolResultEvent:
				// Tool was executed by CLI
				status := "success"
				if e.IsError {
					status = "error"
				}
				fmt.Printf("[Tool Result] %s (%s): %v\n", e.ToolName, status, truncate(fmt.Sprintf("%v", e.Content), 100))

			case claude.TurnCompleteEvent:
				fmt.Printf("\n\n[Turn Complete] Turn %d\n", e.TurnNumber)
				fmt.Printf("  Success: %v\n", e.Success)
				fmt.Printf("  Duration: %dms\n", e.DurationMs)
				fmt.Printf("  Tokens: %d in / %d out\n", e.Usage.InputTokens, e.Usage.OutputTokens)
				fmt.Printf("  Cost: $%.6f\n", e.Usage.CostUSD)
				if e.Error != nil {
					fmt.Printf("  Error: %v\n", e.Error)
				}
				fmt.Println("\n---End Response---")
				fmt.Println("\nSession complete!")
				return // Exit when turn completes

			case claude.ErrorEvent:
				fmt.Printf("[Error] %s: %v\n", e.Context, e.Error)
			}
		}
	}
}

// truncate truncates a string to max length
func truncate(s string, max int) string {
	if len(s) <= max {
		return s
	}
	return s[:max] + "..."
}
