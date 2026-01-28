// Example: Streaming events with the Codex SDK.
//
// This example demonstrates how to stream responses in real-time using
// the Codex SDK's event system.
package main

import (
	"context"
	"fmt"
	"log"
	"os"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/codex"
)

func main() {
	ctx := context.Background()

	// Create client
	client := codex.NewClient(
		codex.WithClientName("codex-streaming-example"),
		codex.WithClientVersion("1.0.0"),
	)

	// Start the app-server
	fmt.Println("Starting Codex app-server...")
	if err := client.Start(ctx); err != nil {
		log.Fatalf("Failed to start client: %v", err)
	}
	defer client.Stop()

	// Get working directory
	cwd, _ := os.Getwd()

	// Create a thread
	fmt.Println("Creating thread...")
	thread, err := client.CreateThread(ctx, codex.WithWorkDir(cwd))
	if err != nil {
		log.Fatalf("Failed to create thread: %v", err)
	}

	// Wait for MCP startup complete
	fmt.Println("Waiting for MCP startup...")
	for event := range client.Events() {
		if e, ok := event.(codex.ThreadReadyEvent); ok && e.ThreadID == thread.ID() {
			fmt.Println("Thread ready!")
			break
		}
	}

	// Send message (non-blocking)
	prompt := "Write a short haiku about programming."
	fmt.Printf("\nAsking: %s\n\n", prompt)
	fmt.Println("Response (streaming):")

	if _, err := thread.SendMessage(ctx, prompt); err != nil {
		log.Fatalf("Failed to send message: %v", err)
	}

	// Stream events
	for event := range client.Events() {
		switch e := event.(type) {
		case codex.TextDeltaEvent:
			// Print text as it streams in
			fmt.Print(e.Delta)

		case codex.TurnCompletedEvent:
			fmt.Printf("\n\n[Turn completed - Success: %v]\n", e.Success)
			fmt.Println("\nSession complete!")
			return

		case codex.ErrorEvent:
			log.Printf("Error: %v (context: %s)\n", e.Error, e.Context)
		}
	}
}
