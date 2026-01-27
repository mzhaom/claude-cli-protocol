package main

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/claude"
)

func main() {
	ctx, cancel := context.WithTimeout(context.Background(), 300*time.Second)
	defer cancel()

	testMessage := "What is 2+2?"

	// Run all three experiments
	fmt.Println("=== Experiment A: Default mode → Send message → Switch to Plan mode ===")
	runExperimentA(ctx, testMessage)

	fmt.Println("\n=== Experiment B: Start directly in Plan mode ===")
	runExperimentB(ctx, testMessage)

	fmt.Println("\n=== Experiment C: Default mode → Switch to Plan BEFORE user message ===")
	runExperimentC(ctx, testMessage)

	fmt.Println("\n=== Comparing Results ===")
	compareRecordings()
}

func runExperimentA(ctx context.Context, message string) {
	// Start in default mode (no --dangerously-skip-permissions)
	session := claude.NewSession(
		claude.WithModel("haiku"),
		claude.WithPermissionMode(claude.PermissionModeDefault),
		claude.WithRecording("./recordings/experiment_a"),
		claude.WithDisablePlugins(),
	)

	fmt.Println("Starting session in DEFAULT mode...")
	if err := session.Start(ctx); err != nil {
		fmt.Printf("Failed to start: %v\n", err)
		return
	}
	defer session.Stop()

	// Send user message first - the CLI sends init AFTER receiving the first message
	fmt.Printf("Sending message: %s\n", message)
	_, err := session.SendMessage(ctx, message)
	if err != nil {
		fmt.Printf("Failed to send message: %v\n", err)
		return
	}

	// Collect events - ReadyEvent comes after the first message
	events := collectTurnEvents(ctx, session)
	if events.Ready != nil {
		fmt.Printf("Session ready! PermissionMode: %s\n", events.Ready.Info.PermissionMode)
	}
	if events.TurnComplete != nil {
		fmt.Printf("Turn complete - Success: %v, Cost: $%.6f\n", events.TurnComplete.Success, events.TurnComplete.Usage.CostUSD)
	}

	// Now switch to plan mode via control message (AFTER turn completes)
	fmt.Println("Switching to plan mode via control message...")
	if err := session.SetPermissionMode(ctx, claude.PermissionModePlan); err != nil {
		fmt.Printf("Failed to set permission mode: %v\n", err)
	}
	time.Sleep(1 * time.Second) // Give time for control response

	fmt.Printf("Recording path: %s\n", session.RecordingPath())
}

func runExperimentB(ctx context.Context, message string) {
	// Start directly in plan mode (no --dangerously-skip-permissions)
	session := claude.NewSession(
		claude.WithModel("haiku"),
		claude.WithPermissionMode(claude.PermissionModePlan),
		claude.WithRecording("./recordings/experiment_b"),
		claude.WithDisablePlugins(),
	)

	fmt.Println("Starting session in PLAN mode...")
	if err := session.Start(ctx); err != nil {
		fmt.Printf("Failed to start: %v\n", err)
		return
	}
	defer session.Stop()

	// Send user message first - the CLI sends init AFTER receiving the first message
	fmt.Printf("Sending message: %s\n", message)
	_, err := session.SendMessage(ctx, message)
	if err != nil {
		fmt.Printf("Failed to send message: %v\n", err)
		return
	}

	// Collect events - ReadyEvent comes after the first message
	events := collectTurnEvents(ctx, session)
	if events.Ready != nil {
		fmt.Printf("Session ready! PermissionMode: %s\n", events.Ready.Info.PermissionMode)
	}
	if events.TurnComplete != nil {
		fmt.Printf("Turn complete - Success: %v, Cost: $%.6f\n", events.TurnComplete.Success, events.TurnComplete.Usage.CostUSD)
	}

	fmt.Printf("Recording path: %s\n", session.RecordingPath())
}

func runExperimentC(ctx context.Context, message string) {
	// Start in default mode, then try to switch to plan BEFORE sending user message
	session := claude.NewSession(
		claude.WithModel("haiku"),
		claude.WithPermissionMode(claude.PermissionModeDefault),
		claude.WithRecording("./recordings/experiment_c"),
		claude.WithDisablePlugins(),
	)

	fmt.Println("Starting session in DEFAULT mode...")
	if err := session.Start(ctx); err != nil {
		fmt.Printf("Failed to start: %v\n", err)
		return
	}
	defer session.Stop()

	// Try to switch to plan mode BEFORE sending any user message
	// This tests whether control messages work before the init message is received
	fmt.Println("Attempting to switch to plan mode BEFORE user message...")
	if err := session.SetPermissionMode(ctx, claude.PermissionModePlan); err != nil {
		fmt.Printf("SetPermissionMode result: %v\n", err)
	} else {
		fmt.Println("SetPermissionMode sent successfully (no error)")
	}

	// Give time to see if any response comes back
	time.Sleep(2 * time.Second)

	// Now send the user message
	fmt.Printf("Sending message: %s\n", message)
	_, err := session.SendMessage(ctx, message)
	if err != nil {
		fmt.Printf("Failed to send message: %v\n", err)
		return
	}

	// Collect events
	events := collectTurnEvents(ctx, session)
	if events.Ready != nil {
		fmt.Printf("Session ready! PermissionMode: %s\n", events.Ready.Info.PermissionMode)
	}
	if events.TurnComplete != nil {
		fmt.Printf("Turn complete - Success: %v, Cost: $%.6f\n", events.TurnComplete.Success, events.TurnComplete.Usage.CostUSD)
	}

	fmt.Printf("Recording path: %s\n", session.RecordingPath())
}

// TurnEvents collects events from a single turn
type TurnEvents struct {
	Ready        *claude.ReadyEvent
	TurnComplete *claude.TurnCompleteEvent
	Errors       []claude.ErrorEvent
}

func collectTurnEvents(ctx context.Context, s *claude.Session) *TurnEvents {
	events := &TurnEvents{}
	timeout := time.After(60 * time.Second)

	for {
		select {
		case <-ctx.Done():
			return events
		case <-timeout:
			fmt.Println("Timeout collecting events")
			return events
		case event, ok := <-s.Events():
			if !ok {
				return events
			}

			switch e := event.(type) {
			case claude.ReadyEvent:
				events.Ready = &e
			case claude.TurnCompleteEvent:
				events.TurnComplete = &e
				return events
			case claude.ErrorEvent:
				events.Errors = append(events.Errors, e)
				fmt.Printf("Error event: %v (context: %s)\n", e.Error, e.Context)
			}
		}
	}
}

func compareRecordings() {
	// Find latest recording directories
	dirA := findLatestRecording("./recordings/experiment_a")
	dirB := findLatestRecording("./recordings/experiment_b")
	dirC := findLatestRecording("./recordings/experiment_c")

	fmt.Printf("\nExperiment A recording: %s\n", dirA)
	fmt.Printf("Experiment B recording: %s\n", dirB)
	fmt.Printf("Experiment C recording: %s\n", dirC)

	// Compare from_cli.jsonl (messages from CLI)
	fmt.Println("\n" + strings.Repeat("=", 70))
	fmt.Println("Messages FROM CLI (what Claude CLI sends to us)")
	fmt.Println(strings.Repeat("=", 70))

	fmt.Println("\n>>> Experiment A (Default → message → Plan):")
	printJsonlSummary(filepath.Join(dirA, "from_cli.jsonl"))

	fmt.Println("\n>>> Experiment B (Plan from start):")
	printJsonlSummary(filepath.Join(dirB, "from_cli.jsonl"))

	fmt.Println("\n>>> Experiment C (Default → Plan BEFORE message):")
	printJsonlSummary(filepath.Join(dirC, "from_cli.jsonl"))

	// Compare to_cli.jsonl (messages to CLI)
	fmt.Println("\n" + strings.Repeat("=", 70))
	fmt.Println("Messages TO CLI (what we send to Claude CLI)")
	fmt.Println(strings.Repeat("=", 70))

	fmt.Println("\n>>> Experiment A (Default → message → Plan):")
	printJsonlSummary(filepath.Join(dirA, "to_cli.jsonl"))

	fmt.Println("\n>>> Experiment B (Plan from start):")
	printJsonlSummary(filepath.Join(dirB, "to_cli.jsonl"))

	fmt.Println("\n>>> Experiment C (Default → Plan BEFORE message):")
	printJsonlSummary(filepath.Join(dirC, "to_cli.jsonl"))

	// Print key differences
	fmt.Println("\n" + strings.Repeat("=", 70))
	fmt.Println("KEY DIFFERENCES")
	fmt.Println(strings.Repeat("=", 70))
	analyzeKeyDifferences(dirA, dirB, dirC)
}

func findLatestRecording(baseDir string) string {
	entries, err := os.ReadDir(baseDir)
	if err != nil {
		return ""
	}
	var latest string
	for _, e := range entries {
		if e.IsDir() {
			latest = filepath.Join(baseDir, e.Name())
		}
	}
	return latest
}

func printJsonlSummary(path string) {
	f, err := os.Open(path)
	if err != nil {
		fmt.Printf("  Error: %v\n", err)
		return
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	msgNum := 0
	for scanner.Scan() {
		msgNum++
		var msg map[string]interface{}
		json.Unmarshal(scanner.Bytes(), &msg)

		msgType, _ := msg["type"].(string)
		subtype, _ := msg["subtype"].(string)

		// Build summary line
		summary := fmt.Sprintf("  [%d] type=%s", msgNum, msgType)
		if subtype != "" {
			summary += fmt.Sprintf(", subtype=%s", subtype)
		}

		// Add key details based on message type
		switch msgType {
		case "system":
			if permMode, ok := msg["permissionMode"].(string); ok {
				summary += fmt.Sprintf(", permissionMode=%s", permMode)
			}
		case "user":
			if message, ok := msg["message"].(map[string]interface{}); ok {
				if content, ok := message["content"].(string); ok && len(content) < 50 {
					summary += fmt.Sprintf(", content=%q", content)
				}
			}
		case "control_request":
			if req, ok := msg["request"].(map[string]interface{}); ok {
				if reqSubtype, ok := req["subtype"].(string); ok {
					summary += fmt.Sprintf(", request.subtype=%s", reqSubtype)
				}
				if mode, ok := req["mode"].(string); ok {
					summary += fmt.Sprintf(", mode=%s", mode)
				}
			}
		case "control_response":
			if resp, ok := msg["response"].(map[string]interface{}); ok {
				if respSubtype, ok := resp["subtype"].(string); ok {
					summary += fmt.Sprintf(", response.subtype=%s", respSubtype)
				}
				if innerResp, ok := resp["response"].(map[string]interface{}); ok {
					if mode, ok := innerResp["mode"].(string); ok {
						summary += fmt.Sprintf(", mode=%s", mode)
					}
				}
			}
		case "result":
			if isErr, ok := msg["is_error"].(bool); ok {
				summary += fmt.Sprintf(", is_error=%v", isErr)
			}
		}

		fmt.Println(summary)
	}
	fmt.Printf("  Total: %d messages\n", msgNum)
}

func analyzeKeyDifferences(dirA, dirB, dirC string) {
	// Read init messages from all experiments
	initA := readInitMessage(filepath.Join(dirA, "from_cli.jsonl"))
	initB := readInitMessage(filepath.Join(dirB, "from_cli.jsonl"))
	initC := readInitMessage(filepath.Join(dirC, "from_cli.jsonl"))

	// Compare permission modes
	fmt.Printf("\n1. Initial Permission Mode (from init message):\n")
	if initA != nil {
		fmt.Printf("   Experiment A: %s\n", initA["permissionMode"])
	} else {
		fmt.Printf("   Experiment A: (no init message recorded)\n")
	}
	if initB != nil {
		fmt.Printf("   Experiment B: %s\n", initB["permissionMode"])
	} else {
		fmt.Printf("   Experiment B: (no init message recorded)\n")
	}
	if initC != nil {
		fmt.Printf("   Experiment C: %s\n", initC["permissionMode"])
	} else {
		fmt.Printf("   Experiment C: (no init message recorded)\n")
	}

	// Count control responses (received from CLI)
	controlRespA := countControlResponses(filepath.Join(dirA, "from_cli.jsonl"))
	controlRespB := countControlResponses(filepath.Join(dirB, "from_cli.jsonl"))
	controlRespC := countControlResponses(filepath.Join(dirC, "from_cli.jsonl"))

	fmt.Printf("\n2. Control Responses Received:\n")
	fmt.Printf("   Experiment A: %d\n", controlRespA)
	fmt.Printf("   Experiment B: %d\n", controlRespB)
	fmt.Printf("   Experiment C: %d\n", controlRespC)

	// Print the actual control responses
	fmt.Printf("\n3. Control Responses in each experiment:\n")
	fmt.Printf("\n   Experiment A:\n")
	printControlResponses(filepath.Join(dirA, "from_cli.jsonl"))
	fmt.Printf("\n   Experiment B:\n")
	printControlResponses(filepath.Join(dirB, "from_cli.jsonl"))
	fmt.Printf("\n   Experiment C:\n")
	printControlResponses(filepath.Join(dirC, "from_cli.jsonl"))
}

func readInitMessage(path string) map[string]interface{} {
	f, err := os.Open(path)
	if err != nil {
		return nil
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var msg map[string]interface{}
		json.Unmarshal(scanner.Bytes(), &msg)
		if msg["type"] == "system" && msg["subtype"] == "init" {
			return msg
		}
	}
	return nil
}

func countControlResponses(path string) int {
	f, err := os.Open(path)
	if err != nil {
		return 0
	}
	defer f.Close()

	count := 0
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var msg map[string]interface{}
		json.Unmarshal(scanner.Bytes(), &msg)
		if msg["type"] == "control_response" {
			count++
		}
	}
	return count
}

func printControlResponses(path string) {
	f, err := os.Open(path)
	if err != nil {
		fmt.Printf("      Error: %v\n", err)
		return
	}
	defer f.Close()

	found := false
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var msg map[string]interface{}
		json.Unmarshal(scanner.Bytes(), &msg)
		if msg["type"] == "control_response" {
			found = true
			prettyJson, _ := json.MarshalIndent(msg, "      ", "  ")
			fmt.Printf("%s\n", prettyJson)
		}
	}
	if !found {
		fmt.Printf("      (none)\n")
	}
}
