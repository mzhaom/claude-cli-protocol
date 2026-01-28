# Codex App-Server Protocol

This document describes the JSON-RPC protocol used by `codex app-server` for programmatic
interaction with Codex. This is the recommended protocol for SDK integration.

## Overview

The app-server uses JSON-RPC 2.0 over stdio (newline-delimited JSON). It provides:
- Bidirectional communication via stdin/stdout
- Request/response for operations (with `id`)
- Notifications for streaming events (without `id`)
- Support for multi-turn conversations within a thread

## Quick Start

```bash
# Start the app-server
codex app-server

# Send JSON-RPC messages on stdin, receive responses on stdout
```

## Protocol Flow

1. **Initialize** - Send `initialize` request with client info
2. **Start Thread** - Send `thread/start` to create a new conversation thread
3. **Start Turn** - Send `turn/start` with user input to begin processing
4. **Receive Events** - Listen for notifications as the model processes
5. **Turn Complete** - Receive `turn/completed` notification when done
6. **Continue or Close** - Send more turns or close stdin to exit

## Request Methods

### initialize

Initialize the connection with client information.

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "clientInfo": {
      "name": "my-sdk",
      "version": "1.0.0"
    }
  }
}
```

Response:
```json
{
  "id": 1,
  "result": {
    "userAgent": "my-sdk/0.92.0 (Mac OS 26.2.0; arm64) unknown (my-sdk; 1.0.0)"
  }
}
```

### thread/start

Start a new conversation thread.

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "thread/start",
  "params": {
    "model": "gpt-5.2-codex",
    "cwd": "/path/to/workspace",
    "approvalPolicy": "on-request",
    "sandbox": "workspace-write"
  }
}
```

Response includes thread info with `id` to use for subsequent turns.

### turn/start

Start a turn with user input.

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "turn/start",
  "params": {
    "threadId": "019c0603-df45-7fb0-91dd-ef01ddcfe3d8",
    "input": [
      {"type": "text", "text": "What is 2+2?"}
    ]
  }
}
```

### turn/interrupt

Interrupt an in-progress turn.

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "turn/interrupt",
  "params": {
    "threadId": "019c0603-df45-7fb0-91dd-ef01ddcfe3d8"
  }
}
```

## Notification Methods (Server → Client)

### thread/started

Sent when a thread is created.

```json
{
  "method": "thread/started",
  "params": {
    "thread": {
      "id": "019c0603-df45-7fb0-91dd-ef01ddcfe3d8",
      "preview": "",
      "modelProvider": "openai",
      "createdAt": 1769627443,
      "updatedAt": 1769627443,
      "path": "/Users/user/.codex/sessions/...",
      "cwd": "/path/to/workspace",
      "turns": []
    }
  }
}
```

### turn/started

Sent when a turn begins processing.

```json
{
  "method": "turn/started",
  "params": {
    "threadId": "019c0603-df45-7fb0-91dd-ef01ddcfe3d8",
    "turn": {
      "id": "0",
      "items": [],
      "status": "inProgress",
      "error": null
    }
  }
}
```

### turn/completed

Sent when a turn completes successfully.

```json
{
  "method": "turn/completed",
  "params": {
    "threadId": "019c0603-df45-7fb0-91dd-ef01ddcfe3d8",
    "turn": {
      "id": "0",
      "items": [],
      "status": "completed",
      "error": null
    }
  }
}
```

### item/started, item/completed

Sent when items (messages, commands, file changes) start and complete.

```json
{
  "method": "item/started",
  "params": {
    "item": {
      "type": "agentMessage",
      "id": "msg_...",
      "text": ""
    },
    "threadId": "...",
    "turnId": "0"
  }
}
```

### item/agentMessage/delta

Streaming text delta for agent messages.

```json
{
  "method": "item/agentMessage/delta",
  "params": {
    "threadId": "...",
    "turnId": "0",
    "itemId": "msg_...",
    "delta": "4"
  }
}
```

### codex/event/*

Low-level codex events (internal protocol):

- `codex/event/task_started` - Task processing started
- `codex/event/task_complete` - Task completed
- `codex/event/agent_message` - Full agent message
- `codex/event/agent_message_delta` - Message streaming delta
- `codex/event/token_count` - Token usage update
- `codex/event/mcp_startup_complete` - MCP servers ready

## Thread Item Types

Items within a turn can be:

- `userMessage` - User input
- `agentMessage` - Model response text
- `reasoning` - Model reasoning/thinking
- `commandExecution` - Shell command execution
- `fileChange` - File modifications
- `mcpToolCall` - MCP tool invocation
- `webSearch` - Web search
- `imageView` - Image attachment

## Sandbox Modes

- `read-only` - No write access
- `workspace-write` - Write access to workspace
- `danger-full-access` - Full system access (dangerous)

## Approval Policies

- `untrusted` - Require approval for everything
- `on-failure` - Approve unless command fails
- `on-request` - Approve on explicit request
- `never` - Auto-approve everything (use with caution)

## Schema Generation

Full JSON Schema definitions can be generated with:

```bash
codex app-server generate-json-schema --out schemas/
```

This produces:
- `v2/` - Current v2 API types (recommended)
- `ClientRequest.json` - All client requests
- `ServerNotification.json` - All server notifications

Note: v1 APIs (`newConversation`, `sendUserMessage`, etc.) are deprecated.
Use v2 APIs (`thread/start`, `turn/start`, etc.) for new development.

## Source of Truth

- Protocol definitions: `codex-rs/app-server-protocol/src/protocol/`
- v2 types: `v2.rs` (current API)
- Common types: `common.rs`
- v1 types: `v1.rs` (deprecated, do not use for new development)
