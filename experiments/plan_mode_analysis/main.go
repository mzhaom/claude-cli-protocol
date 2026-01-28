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

// ExperimentConfig defines the parameters for each experiment
type ExperimentConfig struct {
	Name                    string
	Description             string
	PermissionMode          claude.PermissionMode
	PermissionPromptTool    string // "stdio" or ""
	PlanModeTrigger         string // "cmdline", "control_before", "control_after", or ""
	PermissionHandler       claude.PermissionHandler
	UseCodeReviewScenario   bool
	Message                 string
}

func main() {
	ctx, cancel := context.WithTimeout(context.Background(), 600*time.Second)
	defer cancel()

	// Define all experiments with their configurations
	experiments := []ExperimentConfig{
		// Original plan mode experiments
		{
			Name:            "A",
			Description:     "Default mode → Send message → Switch to Plan mode",
			PermissionMode:  claude.PermissionModeDefault,
			PlanModeTrigger: "control_after",
			Message:         "What is 2+2?",
		},
		{
			Name:            "B",
			Description:     "Start directly in Plan mode",
			PermissionMode:  claude.PermissionModePlan,
			PlanModeTrigger: "cmdline",
			Message:         "What is 2+2?",
		},
		{
			Name:            "C",
			Description:     "Default mode → Switch to Plan BEFORE user message",
			PermissionMode:  claude.PermissionModeDefault,
			PlanModeTrigger: "control_before",
			Message:         "What is 2+2?",
		},
		// Permission prompt tool experiments
		{
			Name:                  "D",
			Description:           "Permission Prompt Tool + Code Review (Allow)",
			PermissionMode:        claude.PermissionModeDefault,
			PermissionPromptTool:  "stdio",
			PermissionHandler:     claude.AllowAllPermissionHandler(),
			UseCodeReviewScenario: true,
			Message:               "Evaluate the existing code in hello.py, suggest next move and fix the issue.",
		},
		{
			Name:                  "E",
			Description:           "Permission Prompt Tool + Code Review (Deny)",
			PermissionMode:        claude.PermissionModeDefault,
			PermissionPromptTool:  "stdio",
			PermissionHandler:     claude.DefaultPermissionHandler(),
			UseCodeReviewScenario: true,
			Message:               "Evaluate the existing code in hello.py, suggest next move and fix the issue.",
		},
		{
			Name:                  "F",
			Description:           "Control - WITHOUT Permission Prompt Tool",
			PermissionMode:        claude.PermissionModeDefault,
			PermissionPromptTool:  "",
			PermissionHandler:     claude.AllowAllPermissionHandler(),
			UseCodeReviewScenario: true,
			Message:               "Evaluate the existing code in hello.py, suggest next move and fix the issue.",
		},
		{
			Name:                  "G",
			Description:           "Permission Prompt Tool + Plan Mode",
			PermissionMode:        claude.PermissionModePlan,
			PermissionPromptTool:  "stdio",
			PlanModeTrigger:       "cmdline",
			PermissionHandler:     claude.AllowAllPermissionHandler(),
			UseCodeReviewScenario: true,
			Message:               "Evaluate the existing code in hello.py, suggest next move and fix the issue.",
		},
		{
			Name:                  "H",
			Description:           "Permission Prompt Tool + BypassPermissions",
			PermissionMode:        claude.PermissionModeBypass,
			PermissionPromptTool:  "stdio",
			PermissionHandler:     claude.AllowAllPermissionHandler(),
			UseCodeReviewScenario: true,
			Message:               "Evaluate the existing code in hello.py, suggest next move and fix the issue.",
		},
	}

	// Run original plan mode experiments
	fmt.Println("=== PLAN MODE EXPERIMENTS ===")
	for _, exp := range experiments[:3] {
		fmt.Printf("\n=== Experiment %s: %s ===\n", exp.Name, exp.Description)
		runExperiment(ctx, exp)
	}

	// Run permission prompt tool experiments
	fmt.Println("\n" + strings.Repeat("=", 70))
	fmt.Println("PERMISSION PROMPT TOOL EXPERIMENTS")
	fmt.Println(strings.Repeat("=", 70))
	for _, exp := range experiments[3:] {
		fmt.Printf("\n=== Experiment %s: %s ===\n", exp.Name, exp.Description)
		runExperiment(ctx, exp)
	}

	fmt.Println("\n=== Comparing Results ===")
	compareRecordings()

	fmt.Println("\n=== Permission Prompt Tool Analysis ===")
	analyzePermissionPromptToolBehavior()
}

// createBrokenPythonProject creates a temp directory with a broken hello.py file
func createBrokenPythonProject() (string, error) {
	tmpDir, err := os.MkdirTemp("", "permission-test-*")
	if err != nil {
		return "", err
	}

	brokenCode := `#!/usr/bin/env python3
# Hello World example with a bug

def main()
    print("Hello, World!")

if __name__ == "__main__":
    main()
`
	if err := os.WriteFile(filepath.Join(tmpDir, "hello.py"), []byte(brokenCode), 0644); err != nil {
		os.RemoveAll(tmpDir)
		return "", err
	}

	return tmpDir, nil
}

func runExperiment(ctx context.Context, cfg ExperimentConfig) {
	var tmpDir string
	var err error

	// Create temp project if needed
	if cfg.UseCodeReviewScenario {
		tmpDir, err = createBrokenPythonProject()
		if err != nil {
			fmt.Printf("Failed to create temp project: %v\n", err)
			return
		}
		defer os.RemoveAll(tmpDir)
		fmt.Printf("Created temp project at: %s\n", tmpDir)
	}

	// Build session options
	opts := []claude.SessionOption{
		claude.WithModel("haiku"),
		claude.WithRecording(fmt.Sprintf("./recordings/experiment_%s", strings.ToLower(cfg.Name))),
		claude.WithDisablePlugins(),
	}

	// Set permission mode (for cmdline plan mode or other modes)
	if cfg.PlanModeTrigger == "cmdline" || cfg.PlanModeTrigger == "" {
		opts = append(opts, claude.WithPermissionMode(cfg.PermissionMode))
	} else {
		// For control message plan mode triggers, start in default
		opts = append(opts, claude.WithPermissionMode(claude.PermissionModeDefault))
	}

	// Set permission prompt tool
	if cfg.PermissionPromptTool == "stdio" {
		opts = append(opts, claude.WithPermissionPromptToolStdio())
	}

	// Set permission handler
	if cfg.PermissionHandler != nil {
		opts = append(opts, claude.WithPermissionHandler(cfg.PermissionHandler))
	}

	// Set working directory
	if tmpDir != "" {
		opts = append(opts, claude.WithWorkDir(tmpDir))
	}

	session := claude.NewSession(opts...)

	fmt.Printf("Starting session (PermissionMode=%s, PermissionPromptTool=%q)...\n",
		cfg.PermissionMode, cfg.PermissionPromptTool)

	if err := session.Start(ctx); err != nil {
		fmt.Printf("Failed to start: %v\n", err)
		return
	}
	defer session.Stop()

	// Handle control_before plan mode trigger
	if cfg.PlanModeTrigger == "control_before" {
		fmt.Println("Switching to plan mode BEFORE user message...")
		if err := session.SetPermissionMode(ctx, claude.PermissionModePlan); err != nil {
			fmt.Printf("SetPermissionMode result: %v\n", err)
		} else {
			fmt.Println("SetPermissionMode sent successfully")
		}
		time.Sleep(2 * time.Second)
	}

	// Send message
	fmt.Printf("Sending message: %s\n", cfg.Message)
	_, err = session.SendMessage(ctx, cfg.Message)
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
		fmt.Printf("Turn complete - Success: %v, Cost: $%.6f\n",
			events.TurnComplete.Success, events.TurnComplete.Usage.CostUSD)
	}

	// Handle control_after plan mode trigger
	if cfg.PlanModeTrigger == "control_after" {
		fmt.Println("Switching to plan mode AFTER turn completes...")
		if err := session.SetPermissionMode(ctx, claude.PermissionModePlan); err != nil {
			fmt.Printf("Failed to set permission mode: %v\n", err)
		}
		time.Sleep(1 * time.Second)
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
	dirA := findLatestRecording("./recordings/experiment_a")
	dirB := findLatestRecording("./recordings/experiment_b")
	dirC := findLatestRecording("./recordings/experiment_c")

	fmt.Printf("\nExperiment A recording: %s\n", dirA)
	fmt.Printf("Experiment B recording: %s\n", dirB)
	fmt.Printf("Experiment C recording: %s\n", dirC)

	fmt.Println("\n" + strings.Repeat("=", 70))
	fmt.Println("Messages FROM CLI (what Claude CLI sends to us)")
	fmt.Println(strings.Repeat("=", 70))

	for _, exp := range []struct{ name, dir string }{
		{"A (Default → message → Plan)", dirA},
		{"B (Plan from start)", dirB},
		{"C (Default → Plan BEFORE message)", dirC},
	} {
		fmt.Printf("\n>>> Experiment %s:\n", exp.name)
		printJsonlSummary(filepath.Join(exp.dir, "from_cli.jsonl"))
	}

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

		summary := fmt.Sprintf("  [%d] type=%s", msgNum, msgType)
		if subtype != "" {
			summary += fmt.Sprintf(", subtype=%s", subtype)
		}

		switch msgType {
		case "system":
			if permMode, ok := msg["permissionMode"].(string); ok {
				summary += fmt.Sprintf(", permissionMode=%s", permMode)
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
	initA := readInitMessage(filepath.Join(dirA, "from_cli.jsonl"))
	initB := readInitMessage(filepath.Join(dirB, "from_cli.jsonl"))
	initC := readInitMessage(filepath.Join(dirC, "from_cli.jsonl"))

	fmt.Printf("\n1. Initial Permission Mode (from init message):\n")
	for name, init := range map[string]map[string]interface{}{
		"A": initA, "B": initB, "C": initC,
	} {
		if init != nil {
			fmt.Printf("   Experiment %s: %s\n", name, init["permissionMode"])
		} else {
			fmt.Printf("   Experiment %s: (no init message recorded)\n", name)
		}
	}

	fmt.Printf("\n2. Control Responses Received:\n")
	for name, dir := range map[string]string{"A": dirA, "B": dirB, "C": dirC} {
		count := countControlResponses(filepath.Join(dir, "from_cli.jsonl"))
		fmt.Printf("   Experiment %s: %d\n", name, count)
	}
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

// ============================================================================
// PERMISSION PROMPT TOOL ANALYSIS FUNCTIONS
// ============================================================================

func analyzePermissionPromptToolBehavior() {
	fmt.Println("\n" + strings.Repeat("=", 70))
	fmt.Println("PERMISSION PROMPT TOOL BEHAVIOR ANALYSIS")
	fmt.Println(strings.Repeat("=", 70))

	dirs := map[string]string{
		"D (stdio + allow)":   findLatestRecording("./recordings/experiment_d"),
		"E (stdio + deny)":    findLatestRecording("./recordings/experiment_e"),
		"F (control/no flag)": findLatestRecording("./recordings/experiment_f"),
		"G (stdio + plan)":    findLatestRecording("./recordings/experiment_g"),
		"H (stdio + bypass)":  findLatestRecording("./recordings/experiment_h"),
	}

	fmt.Println("\n1. can_use_tool Control Requests FROM CLI:")
	for name, dir := range dirs {
		if dir == "" {
			fmt.Printf("   %s: (no recording)\n", name)
			continue
		}
		count := countCanUseToolRequests(filepath.Join(dir, "messages.jsonl"))
		fmt.Printf("   %s: %d requests\n", name, count)
	}

	fmt.Println("\n2. Permission Responses Sent TO CLI:")
	for name, dir := range dirs {
		if dir == "" {
			continue
		}
		count := countPermissionResponses(filepath.Join(dir, "messages.jsonl"))
		fmt.Printf("   %s: %d responses\n", name, count)
	}

	fmt.Println("\n3. Tools Requiring Permission (from can_use_tool requests):")
	for name, dir := range dirs {
		if dir == "" {
			continue
		}
		tools := getToolsRequiringPermission(filepath.Join(dir, "messages.jsonl"))
		if len(tools) == 0 {
			fmt.Printf("   %s: (none)\n", name)
		} else {
			fmt.Printf("   %s: %v\n", name, tools)
		}
	}

	fmt.Println("\n4. Detailed can_use_tool Requests:")
	for name, dir := range dirs {
		if dir == "" {
			continue
		}
		fmt.Printf("\n   >>> %s:\n", name)
		printCanUseToolRequests(filepath.Join(dir, "messages.jsonl"))
	}

	fmt.Println("\n5. Detailed Permission Responses Sent:")
	for name, dir := range dirs {
		if dir == "" {
			continue
		}
		fmt.Printf("\n   >>> %s:\n", name)
		printPermissionResponsesSent(filepath.Join(dir, "messages.jsonl"))
	}

}

func countCanUseToolRequests(path string) int {
	f, err := os.Open(path)
	if err != nil {
		return 0
	}
	defer f.Close()

	count := 0
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var record map[string]interface{}
		json.Unmarshal(scanner.Bytes(), &record)

		if record["direction"] != "received" {
			continue
		}

		msg, ok := record["message"].(map[string]interface{})
		if !ok {
			continue
		}

		if msg["type"] == "control_request" {
			if req, ok := msg["request"].(map[string]interface{}); ok {
				if req["subtype"] == "can_use_tool" {
					count++
				}
			}
		}
	}
	return count
}

func countPermissionResponses(path string) int {
	f, err := os.Open(path)
	if err != nil {
		return 0
	}
	defer f.Close()

	count := 0
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var record map[string]interface{}
		json.Unmarshal(scanner.Bytes(), &record)

		if record["direction"] != "sent" {
			continue
		}

		msg, ok := record["message"].(map[string]interface{})
		if !ok {
			continue
		}

		if msg["type"] == "control_response" {
			if resp, ok := msg["response"].(map[string]interface{}); ok {
				if innerResp, ok := resp["response"].(map[string]interface{}); ok {
					if _, hasBehavior := innerResp["behavior"]; hasBehavior {
						count++
					}
				}
			}
		}
	}
	return count
}

func getToolsRequiringPermission(path string) []string {
	f, err := os.Open(path)
	if err != nil {
		return nil
	}
	defer f.Close()

	toolSet := make(map[string]bool)
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var record map[string]interface{}
		json.Unmarshal(scanner.Bytes(), &record)

		if record["direction"] != "received" {
			continue
		}

		msg, ok := record["message"].(map[string]interface{})
		if !ok {
			continue
		}

		if msg["type"] == "control_request" {
			if req, ok := msg["request"].(map[string]interface{}); ok {
				if req["subtype"] == "can_use_tool" {
					if toolName, ok := req["tool_name"].(string); ok {
						toolSet[toolName] = true
					}
				}
			}
		}
	}

	var tools []string
	for tool := range toolSet {
		tools = append(tools, tool)
	}
	return tools
}

func printCanUseToolRequests(path string) {
	f, err := os.Open(path)
	if err != nil {
		fmt.Printf("      Error: %v\n", err)
		return
	}
	defer f.Close()

	found := false
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var record map[string]interface{}
		json.Unmarshal(scanner.Bytes(), &record)

		if record["direction"] != "received" {
			continue
		}

		msg, ok := record["message"].(map[string]interface{})
		if !ok {
			continue
		}

		if msg["type"] == "control_request" {
			if req, ok := msg["request"].(map[string]interface{}); ok {
				if req["subtype"] == "can_use_tool" {
					found = true
					toolName := req["tool_name"]
					fmt.Printf("      tool_name: %v\n", toolName)
					if input, ok := req["input"].(map[string]interface{}); ok {
						inputJson, _ := json.Marshal(input)
						if len(inputJson) > 100 {
							fmt.Printf("      input: %s...\n", inputJson[:100])
						} else {
							fmt.Printf("      input: %s\n", inputJson)
						}
					}
					if suggestions, ok := req["permission_suggestions"]; ok && suggestions != nil {
						fmt.Printf("      permission_suggestions: %v\n", suggestions)
					}
				}
			}
		}
	}
	if !found {
		fmt.Printf("      (none)\n")
	}
}

func printPermissionResponsesSent(path string) {
	f, err := os.Open(path)
	if err != nil {
		fmt.Printf("      Error: %v\n", err)
		return
	}
	defer f.Close()

	found := false
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		var record map[string]interface{}
		json.Unmarshal(scanner.Bytes(), &record)

		if record["direction"] != "sent" {
			continue
		}

		msg, ok := record["message"].(map[string]interface{})
		if !ok {
			continue
		}

		if msg["type"] == "control_response" {
			if resp, ok := msg["response"].(map[string]interface{}); ok {
				if innerResp, ok := resp["response"].(map[string]interface{}); ok {
					if behavior, hasBehavior := innerResp["behavior"]; hasBehavior {
						found = true
						fmt.Printf("      behavior: %v\n", behavior)
						if message, ok := innerResp["message"]; ok {
							fmt.Printf("      message: %v\n", message)
						}
					}
				}
			}
		}
	}
	if !found {
		fmt.Printf("      (none)\n")
	}
}

