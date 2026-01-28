// Package codex provides types and utilities for interacting with the Codex CLI
// via the app-server JSON-RPC protocol.
//
// The Codex app-server uses JSON-RPC 2.0 over stdio. This package provides:
//   - JSON-RPC request/response types
//   - Protocol-specific types for thread/turn management
//   - Notification types for streaming events
//
// Protocol documentation can be found in APP_SERVER_PROTOCOL.md.
//
// For a working example of how to use these types, see examples/codex_basic/main.go
// which demonstrates the full flow:
//  1. Start codex app-server subprocess
//  2. Send initialize request
//  3. Start a thread
//  4. Start turns and receive streaming responses
//
// Key JSON-RPC methods:
//   - initialize: Initialize the connection
//   - thread/start: Create a new conversation thread
//   - turn/start: Start processing user input
//   - turn/interrupt: Interrupt an in-progress turn
//
// Key notifications (server -> client):
//   - thread/started: Thread created
//   - turn/started, turn/completed: Turn lifecycle
//   - item/agentMessage/delta: Streaming text
package codex
