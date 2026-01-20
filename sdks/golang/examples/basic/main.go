// Example: Basic synchronous usage of the Claude SDK.
//
// This example demonstrates simple interaction with Claude using
// SendMessage and collecting events until TurnCompleteEvent.
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

	// Create a new session with options
	session := claude.NewSession(
		claude.WithModel("haiku"),
		claude.WithPermissionMode(claude.PermissionModeBypass),
		claude.WithDisablePlugins(), // Faster startup
	)

	// Start the session (spawns the CLI process)
	fmt.Println("Starting Claude session...")
	if err := session.Start(ctx); err != nil {
		log.Fatalf("Failed to start session: %v", err)
	}
	defer session.Stop()

	// Send first message
	fmt.Println("\nAsking: What is 2+2?")
	if _, err := session.SendMessage(ctx, "What is 2+2?"); err != nil {
		log.Fatalf("SendMessage failed: %v", err)
	}

	// Collect events until turn completes
	// Note: ReadyEvent arrives with the first turn (CLI sends init after first message)
	result1 := collectUntilTurnComplete(ctx, session)
	fmt.Printf("\nResult:\n")
	fmt.Printf("  Success: %v\n", result1.Success)
	fmt.Printf("  Duration: %dms\n", result1.DurationMs)
	fmt.Printf("  Input tokens: %d\n", result1.Usage.InputTokens)
	fmt.Printf("  Output tokens: %d\n", result1.Usage.OutputTokens)
	fmt.Printf("  Cost: $%.6f\n", result1.Usage.CostUSD)

	// Multi-turn conversation
	fmt.Println("\nAsking follow-up: What about 3+3?")
	if _, err := session.SendMessage(ctx, "What about 3+3?"); err != nil {
		log.Fatalf("SendMessage failed: %v", err)
	}

	result2 := collectUntilTurnComplete(ctx, session)
	fmt.Printf("\nFollow-up result:\n")
	fmt.Printf("  Success: %v\n", result2.Success)
	fmt.Printf("  Cost: $%.6f\n", result2.Usage.CostUSD)

	fmt.Println("\nSession complete!")
}

// collectUntilTurnComplete collects events until TurnCompleteEvent is received.
func collectUntilTurnComplete(ctx context.Context, session *claude.Session) *claude.TurnCompleteEvent {
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
				fmt.Printf("Session ready: %s (model: %s)\n", e.Info.SessionID, e.Info.Model)
			case claude.TurnCompleteEvent:
				return &e
			case claude.ErrorEvent:
				log.Printf("Error: %s: %v", e.Context, e.Error)
			}
		}
	}
}
