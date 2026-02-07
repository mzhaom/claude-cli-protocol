# Custom Tool Protocol: Python SDK MCP Tools

> **Captured**: 2026-02-06 | **SDK Version**: 0.1.31 | **CLI Version**: 2.1.33

This document describes the end-to-end protocol for defining and invoking custom Python tools with the Claude Agent SDK. The protocol spans three layers: the Python API, the MCP control protocol bridge, and the message-level conversation flow.

**Trace data**: See [`traces/custom_tool_trace.json`](traces/custom_tool_trace.json) for the complete captured trace.

---

## Table of Contents

1. [Overview](#1-overview)
2. [Python API](#2-python-api)
3. [CLI Invocation](#3-cli-invocation)
4. [MCP Server Lifecycle (Control Protocol)](#4-mcp-server-lifecycle-control-protocol)
5. [Message Flow (Conversation Layer)](#5-message-flow-conversation-layer)
6. [Permission Callback Flow](#6-permission-callback-flow)
7. [Tool Naming Convention](#7-tool-naming-convention)
8. [Sequence Diagram](#8-sequence-diagram)
9. [Complete Annotated Trace](#9-complete-annotated-trace)
10. [Wire Format Reference](#10-wire-format-reference)

---

## 1. Overview

Custom tools in the Claude Agent SDK are **in-process MCP (Model Context Protocol) servers** that run directly within your Python application. Unlike external MCP servers that run as separate subprocesses communicating via stdio, SDK MCP tools:

- Run in the **same Python process** as your application (no subprocess overhead)
- Communicate with the CLI via the **control protocol** (stdin/stdout JSON messages), not a separate transport
- Are discovered by the CLI through `tools/list` and invoked via `tools/call` — standard MCP JSONRPC methods
- Appear to the model as `mcp__<server_name>__<tool_name>` in the tool list

The key architectural insight is that there are **two protocol layers** operating simultaneously:

| Layer | Transport | Purpose |
|-------|-----------|---------|
| **Control Protocol** | CLI ↔ SDK via `control_request`/`control_response` on stdin/stdout | MCP JSONRPC bridge (initialize, tools/list, tools/call, permissions) |
| **Message Protocol** | CLI → SDK as NDJSON messages on stdout | Conversation flow (SystemMessage, AssistantMessage, UserMessage, ResultMessage) |

The control protocol layer is **invisible** at the message layer — the SDK handles it automatically. The captured trace in this document reveals both layers.

---

## 2. Python API

### 2.1 Define a Tool

```python
from claude_agent_sdk import tool

@tool("greet", "Greet someone by name", {"name": str})
async def greet_tool(args: dict[str, Any]) -> dict[str, Any]:
    return {
        "content": [{"type": "text", "text": f"Hello, {args['name']}! Welcome."}]
    }
```

The `@tool` decorator creates an `SdkMcpTool` instance with:
- `name` — tool identifier (used as `<tool_name>` in the wire name)
- `description` — human-readable description sent to the model
- `input_schema` — either a `{param: type}` dict (auto-converted to JSON Schema) or a full JSON Schema dict

The handler receives a single dict argument with the input parameters and returns a dict with:
- `content` — array of content blocks (typically `[{"type": "text", "text": "..."}]`)
- `is_error` (optional) — `True` if the tool execution failed

### 2.2 Create an MCP Server

```python
from claude_agent_sdk import create_sdk_mcp_server

demo_server = create_sdk_mcp_server("demo_tools", tools=[greet_tool])
```

This creates a `McpSdkServerConfig` containing:
- `type: "sdk"` — tells the CLI this is an in-process server
- `name` — server identifier
- `instance` — the underlying `mcp.server.lowlevel.server.Server` object with registered request handlers

### 2.3 Configure Options

```python
from claude_agent_sdk import ClaudeSDKClient
from claude_agent_sdk.types import ClaudeAgentOptions

options = ClaudeAgentOptions(
    mcp_servers={"demo_tools": demo_server},
    can_use_tool=can_use_tool_callback,  # optional permission callback
    model="haiku",
    max_turns=2,
)

client = ClaudeSDKClient(options)
```

---

## 3. CLI Invocation

When the SDK creates a `SubprocessCLITransport`, it translates the `mcp_servers` config into a `--mcp-config` CLI flag. For SDK servers (type `"sdk"`), the CLI config looks like:

```json
--mcp-config '{"mcpServers": {"demo_tools": {"type": "sdk"}}}'
```

The `"type": "sdk"` tells the CLI to route MCP traffic through the **control protocol** (stdin/stdout `control_request`/`control_response` messages) rather than spawning a subprocess.

### Environment Variables

| Variable | Value | Purpose |
|----------|-------|---------|
| `CLAUDE_AGENT_SDK_VERSION` | `0.1.31` | SDK version identifier |
| `CLAUDE_CODE_ENTRYPOINT` | `sdk-py-client` | Entry point identifier |

### Key CLI Flags

| Flag | Value | Purpose |
|------|-------|---------|
| `--output-format` | `stream-json` | Always set — enables NDJSON output |
| `--input-format` | `stream-json` | Streaming mode — bidirectional communication |
| `--mcp-config` | `{"mcpServers": {...}}` | MCP server configuration |
| `--permission-prompt-tool` | `stdio` | Routes permission prompts through control protocol |
| `--verbose` | (flag) | Always set |

---

## 4. MCP Server Lifecycle (Control Protocol)

The CLI communicates with SDK MCP servers through `mcp_message` control requests. Each request wraps a JSONRPC 2.0 message and is routed to the named server.

### 4.1 Control Request Envelope

All MCP communication uses this envelope format:

**CLI → SDK** (request):
```json
{
  "type": "control_request",
  "request_id": "<uuid>",
  "request": {
    "subtype": "mcp_message",
    "server_name": "demo_tools",
    "message": { /* JSONRPC 2.0 message */ }
  }
}
```

**SDK → CLI** (response):
```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "<uuid>",
    "response": {
      "mcp_response": { /* JSONRPC 2.0 response */ }
    }
  }
}
```

### 4.2 Phase 1: Initialize

The CLI sends an MCP `initialize` request with its client info and protocol version:

```json
// CLI → SDK: initialize
{
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-11-25",
    "capabilities": {},
    "clientInfo": {
      "name": "claude-code",
      "version": "2.1.33"
    }
  },
  "jsonrpc": "2.0",
  "id": 0
}
```

The SDK responds with server capabilities:

```json
// SDK → CLI: initialize response
{
  "jsonrpc": "2.0",
  "id": 0,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "tools": {}
    },
    "serverInfo": {
      "name": "demo_tools",
      "version": "1.0.0"
    }
  }
}
```

Then the CLI sends a `notifications/initialized` notification:

```json
// CLI → SDK: initialized notification
{
  "method": "notifications/initialized",
  "jsonrpc": "2.0"
}
```

The SDK acknowledges:

```json
// SDK → CLI: acknowledgement
{
  "jsonrpc": "2.0",
  "result": {}
}
```

> **Note**: In the captured trace, the CLI sends the initialize sequence **twice** (two full init+notification+tools/list cycles). This appears to be a CLI implementation detail — the SDK handles it idempotently.

### 4.3 Phase 2: Tool Discovery

The CLI discovers available tools via `tools/list`:

```json
// CLI → SDK: tools/list
{
  "method": "tools/list",
  "jsonrpc": "2.0",
  "id": 1
}
```

The SDK responds with the tool catalog:

```json
// SDK → CLI: tools/list response
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "tools": [
      {
        "name": "greet",
        "description": "Greet someone by name",
        "inputSchema": {
          "type": "object",
          "properties": {
            "name": {
              "type": "string"
            }
          },
          "required": ["name"]
        }
      }
    ]
  }
}
```

The CLI takes the tool name `"greet"` and prefixes it with `mcp__demo_tools__` to produce the wire name `mcp__demo_tools__greet` that appears in the `SystemMessage/init` tools list.

### 4.4 Phase 3: Tool Execution

When the model decides to use the tool, the CLI sends a `tools/call` request:

```json
// CLI → SDK: tools/call
{
  "method": "tools/call",
  "params": {
    "name": "greet",
    "arguments": {
      "name": "Alice"
    },
    "_meta": {
      "claudecode/toolUseId": "toolu_011ps2HcHPddjonXwz1ezAnE",
      "progressToken": 2
    }
  },
  "jsonrpc": "2.0",
  "id": 2
}
```

Note the `_meta` field contains:
- `claudecode/toolUseId` — links back to the `ToolUseBlock.id` in the conversation
- `progressToken` — MCP progress tracking token

The SDK executes the Python handler and returns the result:

```json
// SDK → CLI: tools/call response
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Hello, Alice! Welcome."
      }
    ]
  }
}
```

---

## 5. Message Flow (Conversation Layer)

These are the messages visible at the NDJSON stream level — what the SDK consumer sees when iterating over `client.receive_messages()`.

### 5.1 SystemMessage (init)

The first message establishes the session and lists available tools:

```json
{
  "_type": "SystemMessage",
  "subtype": "init",
  "data": {
    "type": "system",
    "subtype": "init",
    "cwd": "/private/tmp/<tmpdir>",
    "session_id": "00000000-0000-0000-0000-000000000000",
    "tools": [
      "Task", "TaskOutput", "Bash", "Glob", "Grep",
      "ExitPlanMode", "Read", "Edit", "Write", "NotebookEdit",
      "WebFetch", "WebSearch", "TaskStop", "AskUserQuestion",
      "Skill", "EnterPlanMode", "TaskCreate", "TaskGet",
      "TaskUpdate", "TaskList", "ToolSearch",
      "mcp__demo_tools__greet"
    ],
    "mcp_servers": [
      { "name": "demo_tools", "status": "connected" }
    ],
    "model": "claude-haiku-4-5-20251001",
    "permissionMode": "default",
    "claude_code_version": "2.1.33"
  }
}
```

Key observations:
- Custom tool `mcp__demo_tools__greet` appears at the end of the tools list
- The MCP server shows `"status": "connected"` (initialization succeeded)
- Built-in tools use plain names; MCP tools use the `mcp__<server>__<tool>` prefix

### 5.2 AssistantMessage (Tool Invocation)

The model decides to call the tool:

```json
{
  "_type": "AssistantMessage",
  "content": [
    {
      "_type": "ToolUseBlock",
      "id": "toolu_011ps2HcHPddjonXwz1ezAnE",
      "name": "mcp__demo_tools__greet",
      "input": {
        "name": "Alice"
      }
    }
  ],
  "model": "claude-haiku-4-5-20251001"
}
```

The `ToolUseBlock.id` is the correlation key that links this invocation to the tool result.

### 5.3 UserMessage (Tool Result)

After the tool executes (via the MCP control protocol), the result appears as a `UserMessage`:

```json
{
  "_type": "UserMessage",
  "content": [
    {
      "_type": "ToolResultBlock",
      "tool_use_id": "toolu_011ps2HcHPddjonXwz1ezAnE",
      "content": [
        {
          "type": "text",
          "text": "Hello, Alice! Welcome."
        }
      ],
      "is_error": null
    }
  ]
}
```

### 5.4 AssistantMessage (Text Response)

The model processes the tool result and generates a text response:

```json
{
  "_type": "AssistantMessage",
  "content": [
    {
      "_type": "TextBlock",
      "text": "Hello! I've greeted Alice for you. The greeting was successful: \"Hello, Alice! Welcome.\""
    }
  ],
  "model": "claude-haiku-4-5-20251001"
}
```

### 5.5 ResultMessage

The final message with session statistics:

```json
{
  "_type": "ResultMessage",
  "subtype": "success",
  "duration_ms": 1599,
  "duration_api_ms": 1580,
  "is_error": false,
  "num_turns": 2,
  "session_id": "00000000-0000-0000-0000-000000000000",
  "total_cost_usd": 0.0035969,
  "usage": {
    "input_tokens": 8,
    "cache_creation_input_tokens": 0,
    "cache_read_input_tokens": 31639,
    "output_tokens": 85,
    "server_tool_use": {
      "web_search_requests": 0,
      "web_fetch_requests": 0
    },
    "service_tier": "standard"
  }
}
```

---

## 6. Permission Callback Flow

When `can_use_tool` is set in `ClaudeAgentOptions`, the CLI sends a `can_use_tool` control request before executing any tool. This happens between the model's `ToolUseBlock` and the `tools/call` MCP request.

### 6.1 Permission Request (CLI → SDK)

```json
{
  "type": "control_request",
  "request_id": "<uuid>",
  "request": {
    "subtype": "can_use_tool",
    "tool_name": "mcp__demo_tools__greet",
    "input": {
      "name": "Alice"
    },
    "permission_suggestions": [
      {
        "type": "addRules",
        "rules": [{ "toolName": "mcp__demo_tools__greet" }],
        "behavior": "allow",
        "destination": "localSettings"
      }
    ],
    "tool_use_id": "toolu_011ps2HcHPddjonXwz1ezAnE"
  }
}
```

Key fields:
- `tool_name` — uses the wire name (with `mcp__` prefix)
- `input` — the arguments the model passed to the tool
- `permission_suggestions` — suggestions for remembering the permission decision
- `tool_use_id` — correlates with the `ToolUseBlock.id`

### 6.2 Permission Response (SDK → CLI)

To **allow** the tool:

```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "<uuid>",
    "response": {
      "behavior": "allow",
      "updatedInput": {
        "name": "Alice"
      }
    }
  }
}
```

To **deny** the tool:

```json
{
  "response": {
    "behavior": "deny",
    "message": "Tool not allowed"
  }
}
```

Note: `updatedInput` must always be present when allowing — if the callback doesn't modify the input, the original input is echoed back. The SDK handles this automatically.

---

## 7. Tool Naming Convention

Custom tools follow the pattern: `mcp__<server_name>__<tool_name>`

| Component | Example | Source |
|-----------|---------|--------|
| `mcp__` | Prefix | Fixed — identifies MCP tools |
| `<server_name>` | `demo_tools` | From `create_sdk_mcp_server("demo_tools", ...)` |
| `__` | Separator | Fixed — double underscore |
| `<tool_name>` | `greet` | From `@tool("greet", ...)` |
| **Wire name** | `mcp__demo_tools__greet` | Used in ToolUseBlock, can_use_tool, SystemMessage/init |

The `tools/call` JSONRPC request uses the **bare tool name** (`"greet"`), not the wire name. The CLI strips the prefix before routing to the MCP server.

---

## 8. Sequence Diagram

```
  Python App          SDK (Query)              CLI              Model (API)
      |                    |                    |                    |
      |  create tool +     |                    |                    |
      |  create server     |                    |                    |
      |  connect()         |                    |                    |
      |------------------->|  spawn subprocess  |                    |
      |                    |------------------->|                    |
      |                    |                    |                    |
      |                    |  SDK initialize    |                    |
      |                    |  control_request   |                    |
      |                    |------------------->|                    |
      |                    |<-------------------|                    |
      |                    |                    |                    |
      |                    |  MCP initialize    |                    |
      |                    |  control_request   |                    |
      |                    |<-------------------|                    |
      |                    |  {protocolVersion, |                    |
      |                    |   capabilities,    |                    |
      |                    |   serverInfo}      |                    |
      |                    |------------------->|                    |
      |                    |                    |                    |
      |                    |  MCP tools/list    |                    |
      |                    |<-------------------|                    |
      |                    |  [{name:"greet",   |                    |
      |                    |    inputSchema}]   |                    |
      |                    |------------------->|                    |
      |                    |                    |                    |
      |                    |                    |  SystemMessage/init|
      |                    |<-------------------|  (tools include    |
      |                    |                    |   mcp__demo_tools__|
      |                    |                    |   greet)           |
      |                    |                    |                    |
      |  send user msg     |  stdin: user msg   |                    |
      |------------------->|------------------->|  API call with     |
      |                    |                    |  tool definitions  |
      |                    |                    |------------------->|
      |                    |                    |                    |
      |                    |                    |  AssistantMessage  |
      |                    |                    |  ToolUseBlock:     |
      |                    |                    |  mcp__demo_tools__ |
      |                    |<-------------------|  greet {name:Alice}|
      |  AssistantMessage  |                    |                    |
      |<-------------------|                    |                    |
      |                    |                    |                    |
      |                    |  can_use_tool      |                    |
      |                    |  control_request   |                    |
      |                    |<-------------------|                    |
      |                    |  {behavior:"allow"}|                    |
      |                    |------------------->|                    |
      |                    |                    |                    |
      |                    |  MCP tools/call    |                    |
      |                    |  {name:"greet",    |                    |
      |                    |   args:{name:Alice}}                    |
      |                    |<-------------------|                    |
      |  greet_tool()      |                    |                    |
      |<- - - - - - - - - -|                    |                    |
      |  "Hello, Alice!"   |                    |                    |
      |- - - - - - - - - ->|  mcp_response:     |                    |
      |                    |  {content:[...]}   |                    |
      |                    |------------------->|                    |
      |                    |                    |                    |
      |                    |                    |  UserMessage:      |
      |                    |<-------------------|  ToolResultBlock   |
      |  UserMessage       |                    |                    |
      |<-------------------|                    |  API call with     |
      |                    |                    |  tool result       |
      |                    |                    |------------------->|
      |                    |                    |                    |
      |                    |                    |  AssistantMessage  |
      |                    |<-------------------|  TextBlock         |
      |  AssistantMessage  |                    |                    |
      |<-------------------|                    |                    |
      |                    |                    |                    |
      |                    |  ResultMessage     |                    |
      |                    |<-------------------|                    |
      |  ResultMessage     |                    |                    |
      |<-------------------|                    |                    |
      |                    |                    |                    |
```

---

## 9. Complete Annotated Trace

The following is the complete captured trace with annotations. The control protocol messages (Section 4) and the conversation messages (Section 5) are interleaved chronologically.

### Timeline

| Time (ms) | Layer | Direction | Event |
|-----------|-------|-----------|-------|
| 0 | Control | CLI → SDK | `mcp_message`: initialize |
| 0 | Control | SDK → CLI | initialize response |
| 1 | Control | CLI → SDK | `mcp_message`: notifications/initialized |
| 1 | Control | SDK → CLI | acknowledgement |
| 2 | Control | CLI → SDK | `mcp_message`: initialize (2nd time) |
| 2 | Control | SDK → CLI | initialize response |
| 3 | Control | CLI → SDK | `mcp_message`: tools/list |
| 3 | Control | SDK → CLI | tools/list response |
| 4 | Control | CLI → SDK | `mcp_message`: notifications/initialized (2nd) |
| 4 | Control | SDK → CLI | acknowledgement |
| 5 | Control | CLI → SDK | `mcp_message`: tools/list (2nd time) |
| 5 | Control | SDK → CLI | tools/list response |
| — | Message | CLI → SDK | `SystemMessage/init` |
| — | Message | SDK → CLI | User message (stdin) |
| ~790 | Message | CLI → SDK | `AssistantMessage` (ToolUseBlock) |
| 791 | Control | CLI → SDK | `can_use_tool` (permission check) |
| 791 | Control | SDK → CLI | allow response |
| 795 | Control | CLI → SDK | `mcp_message`: tools/call |
| 796 | Control | SDK → CLI | tools/call response |
| — | Message | CLI → SDK | `UserMessage` (ToolResultBlock) |
| — | Message | CLI → SDK | `AssistantMessage` (TextBlock) |
| — | Message | CLI → SDK | `ResultMessage` (success) |

### Observations

1. **Double initialization**: The CLI initializes the MCP server twice (two full cycles of initialize + notifications/initialized + tools/list). This is likely a CLI implementation detail for robustness.

2. **Permission before execution**: The `can_use_tool` control request happens **after** the model generates the ToolUseBlock but **before** the `tools/call` MCP request. This gives the SDK consumer a chance to inspect and modify the tool input before execution.

3. **Timing**: The entire MCP initialization (12 control messages) completes in ~5ms. The permission + tool call cycle takes ~6ms. The majority of time (~790ms) is spent waiting for the model API response.

4. **Correlation**: The `tool_use_id` (`toolu_011ps2HcHPddjonXwz1ezAnE`) appears in four places:
   - `ToolUseBlock.id` (AssistantMessage)
   - `ToolResultBlock.tool_use_id` (UserMessage)
   - `can_use_tool.tool_use_id` (control request)
   - `tools/call._meta.claudecode/toolUseId` (MCP request)

---

## 10. Wire Format Reference

For the complete protocol specification including all message types, control requests, and hook protocol, see:

- [`PROTOCOL_SPECIFICATION.md`](../../protocol/docs/PROTOCOL_SPECIFICATION.md) — Full auto-generated protocol docs
- [`protocol.proto`](../../protocol/docs/protocol.proto) — Protocol Buffers definition
- [`schemas/control-protocol.schema.json`](../../protocol/docs/schemas/control-protocol.schema.json) — JSON Schema for control protocol
- [`schemas/message.schema.json`](../../protocol/docs/schemas/message.schema.json) — JSON Schema for messages

### Relevant Sections in PROTOCOL_SPECIFICATION.md

| Section | Content |
|---------|---------|
| Section 5: Control Protocol | `SDKControlMcpMessageRequest` type definition |
| Section 7: Permission Protocol | `can_use_tool` request/response format, `PermissionResultAllow`/`PermissionResultDeny` |
| Section 8: MCP Server Configuration | `McpSdkServerConfig` for SDK-type servers |
