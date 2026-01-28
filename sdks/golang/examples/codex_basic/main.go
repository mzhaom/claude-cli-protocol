// Example: Basic usage of the Codex SDK.
//
// This example demonstrates a simple one-shot query using the high-level
// Codex SDK API.
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

	// Create client with custom name/version
	client := codex.NewClient(
		codex.WithClientName("codex-basic-example"),
		codex.WithClientVersion("1.0.0"),
	)

	// Start the app-server
	fmt.Println("Starting Codex app-server...")
	if err := client.Start(ctx); err != nil {
		log.Fatalf("Failed to start client: %v", err)
	}
	defer client.Stop()
	fmt.Println("Client started successfully")

	// Get working directory for the thread
	cwd, _ := os.Getwd()

	// Simple ask - creates thread, waits for MCP startup, sends message, waits for response
	prompt := "What is 2+2? Reply with just the number."
	fmt.Printf("\nAsking: %s\n\n", prompt)

	response, err := client.Ask(ctx, prompt, codex.WithWorkDir(cwd))
	if err != nil {
		log.Fatalf("Failed to get response: %v", err)
	}

	fmt.Printf("Response: %s\n", response)
	fmt.Println("\nSession complete!")
}
