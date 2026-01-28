// Example: Basic usage of the Codex SDK with app-server protocol.
//
// This example demonstrates interaction with Codex using the app-server
// JSON-RPC protocol over stdio.
package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"sync/atomic"
)

// JSON-RPC types
type JSONRPCRequest struct {
	JSONRPC string      `json:"jsonrpc"`
	ID      int64       `json:"id"`
	Method  string      `json:"method"`
	Params  interface{} `json:"params"`
}

type JSONRPCMessage struct {
	JSONRPC string          `json:"jsonrpc,omitempty"`
	ID      *int64          `json:"id,omitempty"`
	Method  string          `json:"method,omitempty"`
	Params  json.RawMessage `json:"params,omitempty"`
	Result  json.RawMessage `json:"result,omitempty"`
	Error   *JSONRPCError   `json:"error,omitempty"`
}

type JSONRPCError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

// Request params
type InitializeParams struct {
	ClientInfo ClientInfo `json:"clientInfo"`
}

type ClientInfo struct {
	Name    string `json:"name"`
	Version string `json:"version"`
}

type ThreadStartParams struct {
	CWD string `json:"cwd,omitempty"`
}

type TurnStartParams struct {
	ThreadID string      `json:"threadId"`
	Input    []UserInput `json:"input"`
}

type UserInput struct {
	Type string `json:"type"`
	Text string `json:"text,omitempty"`
}

// Response types
type ThreadStartResponse struct {
	Thread struct {
		ID string `json:"id"`
	} `json:"thread"`
	Model string `json:"model"`
}

// Notification params
type TurnCompletedNotification struct {
	ThreadID string `json:"threadId"`
	Turn     struct {
		Status string `json:"status"`
	} `json:"turn"`
}

type AgentMessageDeltaNotification struct {
	Delta string `json:"delta"`
}

type CodexEventNotification struct {
	Msg json.RawMessage `json:"msg"`
}

type AgentMessageMsg struct {
	Message string `json:"message"`
}

func main() {
	// Start codex app-server
	cmd := exec.Command("codex", "app-server")
	stdin, err := cmd.StdinPipe()
	if err != nil {
		log.Fatalf("Failed to get stdin pipe: %v", err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		log.Fatalf("Failed to get stdout pipe: %v", err)
	}
	cmd.Stderr = os.Stderr

	if err := cmd.Start(); err != nil {
		log.Fatalf("Failed to start codex app-server: %v", err)
	}
	defer func() {
		stdin.Close()
		cmd.Wait()
	}()

	// Create reader/writer
	reader := bufio.NewReader(stdout)
	var reqID atomic.Int64

	send := func(method string, params interface{}) int64 {
		id := reqID.Add(1)
		req := JSONRPCRequest{
			JSONRPC: "2.0",
			ID:      id,
			Method:  method,
			Params:  params,
		}
		data, _ := json.Marshal(req)
		fmt.Fprintf(stdin, "%s\n", data)
		return id
	}

	recv := func() (*JSONRPCMessage, error) {
		line, err := reader.ReadString('\n')
		if err != nil {
			return nil, err
		}
		var msg JSONRPCMessage
		if err := json.Unmarshal([]byte(line), &msg); err != nil {
			return nil, err
		}
		return &msg, nil
	}

	// 1. Initialize
	fmt.Println("Initializing...")
	send("initialize", InitializeParams{
		ClientInfo: ClientInfo{
			Name:    "codex-basic-example",
			Version: "1.0.0",
		},
	})
	if msg, err := recv(); err != nil {
		log.Fatalf("Failed to receive initialize response: %v", err)
	} else if msg.Error != nil {
		log.Fatalf("Initialize error: %s", msg.Error.Message)
	} else {
		fmt.Println("Initialized successfully")
	}

	// 2. Start thread
	fmt.Println("\nStarting thread...")
	cwd, _ := os.Getwd()
	send("thread/start", ThreadStartParams{CWD: cwd})

	var threadID string
	// Receive response and notifications until we get the thread ID
	for {
		msg, err := recv()
		if err != nil {
			log.Fatalf("Failed to receive message: %v", err)
		}
		if msg.ID != nil && msg.Result != nil {
			// This is the response
			var resp ThreadStartResponse
			if err := json.Unmarshal(msg.Result, &resp); err != nil {
				log.Fatalf("Failed to parse thread/start response: %v", err)
			}
			threadID = resp.Thread.ID
			fmt.Printf("Thread started: %s (model: %s)\n", threadID, resp.Model)
			break
		}
		// Skip notifications during startup
	}

	// Wait for thread/started and mcp_startup_complete
	for {
		msg, err := recv()
		if err != nil {
			log.Fatalf("Failed to receive message: %v", err)
		}
		if msg.Method == "codex/event/mcp_startup_complete" {
			fmt.Println("MCP startup complete")
			break
		}
	}

	// 3. Start turn with prompt
	prompt := "What is 2+2? Reply with just the number."
	fmt.Printf("\nAsking: %s\n", prompt)
	send("turn/start", TurnStartParams{
		ThreadID: threadID,
		Input: []UserInput{
			{Type: "text", Text: prompt},
		},
	})

	// 4. Collect events until turn completes
	var fullResponse string
	for {
		msg, err := recv()
		if err != nil {
			if err == io.EOF {
				break
			}
			log.Fatalf("Failed to receive message: %v", err)
		}

		switch msg.Method {
		case "item/agentMessage/delta":
			var delta AgentMessageDeltaNotification
			if err := json.Unmarshal(msg.Params, &delta); err == nil {
				fullResponse += delta.Delta
				fmt.Printf("%s", delta.Delta)
			}

		case "codex/event/agent_message":
			var ev CodexEventNotification
			if err := json.Unmarshal(msg.Params, &ev); err == nil {
				var agentMsg AgentMessageMsg
				if err := json.Unmarshal(ev.Msg, &agentMsg); err == nil {
					if agentMsg.Message != "" && fullResponse == "" {
						fullResponse = agentMsg.Message
					}
				}
			}

		case "turn/completed":
			fmt.Printf("\n\nTurn completed!\n")
			fmt.Printf("Full response: %s\n", fullResponse)
			goto done
		}
	}

done:
	fmt.Println("\nSession complete!")
}
