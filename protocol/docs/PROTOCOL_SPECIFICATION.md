# Claude CLI Protocol Specification

> **Generated**: 2026-01-17 02:58:30 UTC
> **SDK Version**: 0.1.20
> **CLI Version**: 2.1.9

This document specifies the exact wire protocol between the Claude Agent SDK for Python
and the Claude Code CLI. It is auto-generated from type definitions and captured protocol traces.

**See also:** [`protocol.proto`](protocol.proto) - Protocol Buffers definition with detailed comments

## Table of Contents

1. [CLI Invocation](#1-cli-invocation)
2. [Operating Modes](#2-operating-modes)
3. [Message Types](#3-message-types)
4. [Content Blocks](#4-content-blocks)
5. [Control Protocol](#5-control-protocol)
6. [Hook Protocol](#6-hook-protocol)
7. [Permission Protocol](#7-permission-protocol)
8. [MCP Server Configuration](#8-mcp-server-configuration)
9. [SDK Configuration](#9-sdk-configuration)

---

## 1. CLI Invocation

### Command Line Arguments

The SDK invokes the Claude Code CLI with the following arguments.
These were discovered by capturing actual CLI invocations from the SDK:

| Flag | Example Values | Triggered By |
|------|----------------|--------------|
| `--` | `dummy` | minimal_string_mode, with_system_prompt (+22 more) |
| `--add-dir` | `/extra/dir2`, `/extra/dir1` | with_add_dirs, with_add_dirs |
| `--agents` | `{"custom-agent": {"description": "A custom agent f...` | with_agents |
| `--allowedTools` | `Bash` | with_tools_config |
| `--append-system-prompt` | `Additional instructions.` | with_append_system_prompt |
| `--betas` | `beta-feature-1` | with_betas |
| `--continue` | (no value) | with_continue |
| `--disallowedTools` | `WebSearch` | with_tools_config |
| `--fallback-model` | `sonnet` | with_model_options |
| `--fork-session` | (no value) | with_fork_session |
| `--include-partial-messages` | (no value) | with_partial_messages |
| `--input-format` | `stream-json` | minimal_streaming_mode, with_partial_messages (+1 more) |
| `--json-schema` | `{"type": "object", "properties": {"result": {"type...` | with_json_schema |
| `--max-budget-usd` | `1.0` | with_limits |
| `--max-thinking-tokens` | `1000` | with_max_thinking_tokens |
| `--max-turns` | `5` | with_limits |
| `--mcp-config` | `{"mcpServers": {"test-server": {"type": "stdio", "...` | with_mcp_servers |
| `--model` | `haiku` | with_model_options |
| `--output-format` | `stream-json` | Always |
| `--permission-mode` | `bypassPermissions` | with_permission_mode |
| `--permission-prompt-tool` | `stdio` | with_permission_prompt_tool |
| `--print` | (no value) | minimal_string_mode, with_system_prompt (+22 more) |
| `--resume` | `session-123` | with_resume |
| `--setting-sources` | ``, `project`, `user,project,local` | Always |
| `--settings` | `{"key": "value"}`, `{"sandbox": {"enabled": true, "network": {"allow_h...` | with_settings, with_sandbox |
| `--system-prompt` | ``, `You are a helpful assistant.` | minimal_string_mode, minimal_streaming_mode (+24 more) |
| `--tools` | `Read,Write` | with_tools_config |
| `--verbose` | (no value) | Always |

### Environment Variables

The SDK sets these environment variables when invoking the CLI:

| Variable | Example Values | Triggered By |
|----------|----------------|--------------|
| `CLAUDE_AGENT_SDK_VERSION` | `0.1.20` | Always |
| `CLAUDE_CODE_ENABLE_SDK_FILE_CHECKPOINTING` | `true` | with_file_checkpointing |
| `CLAUDE_CODE_ENTRYPOINT` | `sdk-py` | Always |
| `PWD` | `/tmp/test` | with_cwd |

### CLI Capabilities (Discovered from Traces)

The following capabilities were discovered from captured protocol traces via `SystemInitData`:


**Available Tools** (19):

```
AskUserQuestion, Bash, Edit, EnterPlanMode, ExitPlanMode, Glob, Grep, KillShell, MCPSearch, NotebookEdit, Read, Skill, Task, TaskOutput, TodoWrite, WebFetch, WebSearch, Write, mcp__permission_test__test_action
```


**Permission Modes Observed**: `acceptEdits, bypassPermissions, default`


**Models Observed**: `claude-haiku-4-5-20251001, claude-sonnet-4-5-20250929`


**Slash Commands** (8):

```
compact, context, cost, init, pr-comments, release-notes, review, security-review
```


### Example CLI Invocations

The following shows actual CLI commands built for various `ClaudeAgentOptions` configurations:


**minimal_string_mode** (string mode):

Options:
```json
{
  "allowed_tools": [],
  "mcp_servers": {},
  "disallowed_tools": [],
  "betas": [],
  "add_dirs": [],
  "env": {},
  "extra_args": {},
  "plugins": []
}
```

Command:
```bash
g/anthropics/claude-sdk-protocol-docs/.venv/lib/python3.12/site-packages/claude_agent_sdk/_bundled/claude \
    --output-format \
    stream-json \
    --verbose \
    --system-prompt \
     \
    --setting-sources \
     \
    --print \
    -- \
    dummy
```


**minimal_streaming_mode** (streaming mode):

Options:
```json
{
  "allowed_tools": [],
  "mcp_servers": {},
  "disallowed_tools": [],
  "betas": [],
  "add_dirs": [],
  "env": {},
  "extra_args": {},
  "plugins": []
}
```

Command:
```bash
g/anthropics/claude-sdk-protocol-docs/.venv/lib/python3.12/site-packages/claude_agent_sdk/_bundled/claude \
    --output-format \
    stream-json \
    --verbose \
    --system-prompt \
     \
    --setting-sources \
     \
    --input-format \
    stream-json
```


**with_model_options** (string mode):

Options:
```json
{
  "allowed_tools": [],
  "mcp_servers": {},
  "disallowed_tools": [],
  "model": "haiku",
  "fallback_model": "sonnet",
  "betas": [],
  "add_dirs": [],
  "env": {},
  "extra_args": {},
  "plugins": []
}
```

Command:
```bash
g/anthropics/claude-sdk-protocol-docs/.venv/lib/python3.12/site-packages/claude_agent_sdk/_bundled/claude \
    --output-format \
    stream-json \
    --verbose \
    --system-prompt \
     \
    --model \
    haiku \
    --fallback-model \
    sonnet \
    --setting-sources \
     \
    --print \
    -- \
    dummy
```


**with_permission_mode** (string mode):

Options:
```json
{
  "allowed_tools": [],
  "mcp_servers": {},
  "permission_mode": "bypassPermissions",
  "disallowed_tools": [],
  "betas": [],
  "add_dirs": [],
  "env": {},
  "extra_args": {},
  "plugins": []
}
```

Command:
```bash
g/anthropics/claude-sdk-protocol-docs/.venv/lib/python3.12/site-packages/claude_agent_sdk/_bundled/claude \
    --output-format \
    stream-json \
    --verbose \
    --system-prompt \
     \
    --permission-mode \
    bypassPermissions \
    --setting-sources \
     \
    --print \
    -- \
    dummy
```


**Built-in Agents**: `Bash, Explore, Plan, code-agent, general-purpose, research-agent, statusline-setup`


**CLI Version (from traces)**: `2.1.9`


---

## 2. Operating Modes

### String Mode (One-shot)

For single queries without bidirectional communication:

```bash
claude --output-format stream-json --verbose \
       --system-prompt "..." \
       --print -- "Your prompt here"
```

- Prompt passed as CLI argument after `--print --`
- stdin is closed immediately after process starts
- Unidirectional: responses only flow from CLI to SDK

### Streaming Mode (Interactive)

For interactive sessions with bidirectional communication:

```bash
claude --output-format stream-json --verbose \
       --system-prompt "..." \
       --input-format stream-json
```

- Uses `--input-format stream-json` flag
- stdin remains open for sending messages and control requests
- Bidirectional: supports control protocol, hooks, and permission callbacks

### Context Window Usage Tracking

The `ResultMessage.usage` field provides detailed token usage information:

```json
{
  "usage": {
    "input_tokens": 100,
    "cache_creation_input_tokens": 2000,
    "cache_read_input_tokens": 15000,
    "output_tokens": 50,
    "server_tool_use": {
      "web_search_requests": 0,
      "web_fetch_requests": 0
    },
    "service_tier": "standard",
    "cache_creation": {
      "ephemeral_1h_input_tokens": 0,
      "ephemeral_5m_input_tokens": 333
    }
  }
}
```

Key fields:
- `input_tokens`: New tokens sent to the model
- `cache_creation_input_tokens`: Tokens used to create cache
- `cache_read_input_tokens`: Tokens read from cache
- `output_tokens`: Tokens generated by the model

Total context used = `input_tokens` + `cache_read_input_tokens` (cache creation is separate)

### Planner Mode

To enter planner mode, set `permission_mode` to `"plan"`:

```python
options = ClaudeAgentOptions(permission_mode="plan")
```

In plan mode:
- Claude reviews the plan before executing
- Use `set_permission_mode` control request to switch modes dynamically

---

## 3. Message Types

All messages are JSON objects sent as newline-delimited JSON (NDJSON) over stdout.
The `type` field discriminates between message types.

### Message Union

```
Message = UserMessage | AssistantMessage | SystemMessage | ResultMessage | StreamEvent
```


### UserMessage

User message.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `content` | `str | list[claude_agent_sdk.types.TextBlock | claude_agent_sdk.types.ThinkingBlock | claude_agent_sdk.types.ToolUseBlock | claude_agent_sdk.types.ToolResultBlock]` | Yes | |
| `uuid` | `str | None` | No | |
| `parent_tool_use_id` | `str | None` | No | |

### AssistantMessage

Assistant message with content blocks.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `content` | `Array<claude_agent_sdk.types.TextBlock | claude_agent_sdk.types.ThinkingBlock | claude_agent_sdk.types.ToolUseBlock | claude_agent_sdk.types.ToolResultBlock>` | Yes | |
| `model` | `string` | Yes | |
| `parent_tool_use_id` | `str | None` | No | |
| `error` | `"authentication_failed" | "billing_error" | "rate_limit" | "invalid_request" | "server_error" | "unknown" | null` | No | |

### SystemMessage

System message with metadata.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `string` | Yes | |
| `data` | `Record<string, any>` | Yes | |

### ResultMessage

Result message with cost and usage information.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `string` | Yes | |
| `duration_ms` | `integer` | Yes | |
| `duration_api_ms` | `integer` | Yes | |
| `is_error` | `boolean` | Yes | |
| `num_turns` | `integer` | Yes | |
| `session_id` | `string` | Yes | |
| `total_cost_usd` | `float | None` | No | |
| `usage` | `dict[str, typing.Any] | None` | No | |
| `result` | `str | None` | No | |
| `structured_output` | `any` | No | |

### StreamEvent

Stream event for partial message updates during streaming.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `uuid` | `string` | Yes | |
| `session_id` | `string` | Yes | |
| `event` | `Record<string, any>` | Yes | |
| `parent_tool_use_id` | `str | None` | No | |

---

## 4. Content Blocks

Content blocks appear in `UserMessage.content` and `AssistantMessage.content`.
The `type` field (on the wire, not the Python class) discriminates block types.

### ContentBlock Union

```
ContentBlock = TextBlock | ThinkingBlock | ToolUseBlock | ToolResultBlock
```


### TextBlock

Text content block.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `text` | `string` | Yes | |

### ThinkingBlock

Thinking content block.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `thinking` | `string` | Yes | |
| `signature` | `string` | Yes | |

### ToolUseBlock

Tool use content block.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `id` | `string` | Yes | |
| `name` | `string` | Yes | |
| `input` | `Record<string, any>` | Yes | |

### ToolResultBlock

Tool result content block.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tool_use_id` | `string` | Yes | |
| `content` | `str | list[dict[str, typing.Any]] | None` | No | |
| `is_error` | `bool | None` | No | |

---

## 5. Control Protocol

The control protocol enables bidirectional communication between SDK and CLI.
Control messages are sent via stdin (SDK to CLI) or stdout (CLI to SDK).

### Control Request Wrapper

All control requests are wrapped in `SDKControlRequest`:

```json
{
  "type": "control_request",
  "request_id": "req_1_abc123",
  "request": {
    "subtype": "<request_type>",
    ...
  }
}
```

### Control Response Wrapper

All control responses are wrapped in `SDKControlResponse`:

```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_1_abc123",
    "response": { ... }
  }
}
```

### Request ID Format

Request IDs follow the pattern: `req_{counter}_{random_hex}`
- `counter`: Incrementing integer per session
- `random_hex`: 4 bytes of random hex for uniqueness

### Control Request Types


#### SDKControlInterruptRequest

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `"interrupt"` | Yes | |

#### SDKControlPermissionRequest

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `"can_use_tool"` | Yes | |
| `tool_name` | `string` | Yes | |
| `input` | `Record<string, any>` | Yes | |
| `permission_suggestions` | `list[typing.Any] | None` | Yes | |
| `blocked_path` | `str | None` | Yes | |

#### SDKControlInitializeRequest

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `"initialize"` | Yes | |
| `hooks` | `dict[typing.Union[typing.Literal['PreToolUse'], typing.Literal['PostToolUse'], typing.Literal['UserPromptSubmit'], typing.Literal['Stop'], typing.Literal['SubagentStop'], typing.Literal['PreCompact']], typing.Any] | None` | Yes | |

#### SDKControlSetPermissionModeRequest

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `"set_permission_mode"` | Yes | |
| `mode` | `string` | Yes | |

#### SDKHookCallbackRequest

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `"hook_callback"` | Yes | |
| `callback_id` | `string` | Yes | |
| `input` | `any` | Yes | |
| `tool_use_id` | `str | None` | Yes | |

#### SDKControlMcpMessageRequest

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `"mcp_message"` | Yes | |
| `server_name` | `string` | Yes | |
| `message` | `any` | Yes | |

#### SDKControlRewindFilesRequest

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `"rewind_files"` | Yes | |
| `user_message_id` | `string` | Yes | |

### Control Response Types


#### ControlResponse

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `"success"` | Yes | |
| `request_id` | `string` | Yes | |
| `response` | `dict[str, typing.Any] | None` | Yes | |

#### ControlErrorResponse

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `subtype` | `"error"` | Yes | |
| `request_id` | `string` | Yes | |
| `error` | `string` | Yes | |

---

## 6. Hook Protocol

Hooks allow the SDK to intercept and modify CLI behavior at specific points.

### Hook Events

```python
HookEvent = "PreToolUse" | "PostToolUse" | "UserPromptSubmit" | "Stop" | "SubagentStop" | "PreCompact"
```

### Hook Input Types

The `hook_event_name` field discriminates between input types.


#### PreToolUseHookInput

Input data for PreToolUse hook events.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `session_id` | `string` | Yes | |
| `transcript_path` | `string` | Yes | |
| `cwd` | `string` | Yes | |
| `permission_mode` | `string` | No | |
| `hook_event_name` | `"PreToolUse"` | Yes | |
| `tool_name` | `string` | Yes | |
| `tool_input` | `Record<string, any>` | Yes | |

#### PostToolUseHookInput

Input data for PostToolUse hook events.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `session_id` | `string` | Yes | |
| `transcript_path` | `string` | Yes | |
| `cwd` | `string` | Yes | |
| `permission_mode` | `string` | No | |
| `hook_event_name` | `"PostToolUse"` | Yes | |
| `tool_name` | `string` | Yes | |
| `tool_input` | `Record<string, any>` | Yes | |
| `tool_response` | `any` | Yes | |

#### UserPromptSubmitHookInput

Input data for UserPromptSubmit hook events.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `session_id` | `string` | Yes | |
| `transcript_path` | `string` | Yes | |
| `cwd` | `string` | Yes | |
| `permission_mode` | `string` | No | |
| `hook_event_name` | `"UserPromptSubmit"` | Yes | |
| `prompt` | `string` | Yes | |

#### StopHookInput

Input data for Stop hook events.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `session_id` | `string` | Yes | |
| `transcript_path` | `string` | Yes | |
| `cwd` | `string` | Yes | |
| `permission_mode` | `string` | No | |
| `hook_event_name` | `"Stop"` | Yes | |
| `stop_hook_active` | `boolean` | Yes | |

#### SubagentStopHookInput

Input data for SubagentStop hook events.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `session_id` | `string` | Yes | |
| `transcript_path` | `string` | Yes | |
| `cwd` | `string` | Yes | |
| `permission_mode` | `string` | No | |
| `hook_event_name` | `"SubagentStop"` | Yes | |
| `stop_hook_active` | `boolean` | Yes | |

#### PreCompactHookInput

Input data for PreCompact hook events.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `session_id` | `string` | Yes | |
| `transcript_path` | `string` | Yes | |
| `cwd` | `string` | Yes | |
| `permission_mode` | `string` | No | |
| `hook_event_name` | `"PreCompact"` | Yes | |
| `trigger` | `"manual" | "auto"` | Yes | |
| `custom_instructions` | `str | None` | Yes | |

### Hook Output Types

**Important**: Python uses `async_` and `continue_` (with underscores) to avoid
keyword conflicts. These are automatically converted to `async` and `continue`
when sent to the CLI.


#### AsyncHookJSONOutput

Async hook output that defers hook execution.

    Fields:
        async_: Set to True to defer hook execution. Note: This is converted to
            "async" when sent to the CLI - use "async_" in your Python code.
        asyncTimeout: Optional timeout in milliseconds for the async operation.
    

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `async_` | `true` | Yes | |
| `asyncTimeout` | `integer` | No | |

#### SyncHookJSONOutput

Synchronous hook output with control and decision fields.

    This defines the structure for hook callbacks to control execution and provide
    feedback to Claude.

    Common Control Fields:
        continue_: Whether Claude should proceed after hook execution (default: True).
            Note: This is converted to "continue" when sent to the CLI.
        suppressOutput: Hide stdout from transcript mode (default: False).
        stopReason: Message shown when continue is False.

    Decision Fields:
        decision: Set to "block" to indicate blocking behavior.
        systemMessage: Warning message displayed to the user.
        reason: Feedback message for Claude about the decision.

    Hook-Specific Output:
        hookSpecificOutput: Event-specific controls (e.g., permissionDecision for
            PreToolUse, additionalContext for PostToolUse).

    Note: The CLI documentation shows field names without underscores ("async", "continue"),
    but Python code should use the underscore versions ("async_", "continue_") as they
    are automatically converted.
    

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `continue_` | `boolean` | No | |
| `suppressOutput` | `boolean` | No | |
| `stopReason` | `string` | No | |
| `decision` | `"block"` | No | |
| `systemMessage` | `string` | No | |
| `reason` | `string` | No | |
| `hookSpecificOutput` | `claude_agent_sdk.types.PreToolUseHookSpecificOutput | claude_agent_sdk.types.PostToolUseHookSpecificOutput | claude_agent_sdk.types.UserPromptSubmitHookSpecificOutput | claude_agent_sdk.types.SessionStartHookSpecificOutput` | No | |

---

## 7. Permission Protocol

The permission system controls tool execution approval.

### Wire Format Note

**IMPORTANT**: The CLI's Zod schema expects **camelCase** field names for permission responses
sent TO the CLI, even though the Python SDK type definitions use snake_case. When implementing
SDKs, ensure the JSON wire format uses:
- `updatedInput` (not `updated_input`)
- `updatedPermissions` (not `updated_permissions`)

**Critical behavior (per Python SDK implementation in `_internal/query.py`):**

1. **`updatedInput` MUST be an object, NEVER null**:
   - If the user's permission handler doesn't provide `updated_input`, fall back to the **original input** from the `can_use_tool` request
   - This ensures the tool receives valid input parameters
   - Use an empty object `{}` as a last resort if original input is also unavailable

2. **`updatedPermissions` CAN be omitted when nil**:
   - Unlike `updatedInput`, this field can be completely omitted if there are no permission updates
   - Only include it when you want to add/modify permission rules

**Correct wire format example:**
```json
{
  "behavior": "allow",
  "updatedInput": {"command": "echo hello"}
}
```

**With permission updates:**
```json
{
  "behavior": "allow",
  "updatedInput": {"command": "echo hello"},
  "updatedPermissions": [{"type": "setMode", "mode": "acceptEdits", "destination": "session"}]
}
```

### Permission Result Types


#### PermissionResultAllow

Allow permission result.

| Field | Type | Required | Wire Name | Description |
|-------|------|----------|-----------|-------------|
| `behavior` | `"allow"` | No | `behavior` | |
| `updated_input` | `dict[str, typing.Any] | None` | No | `updatedInput` | **MUST be object** (use original input as fallback, never null) |
| `updated_permissions` | `list[claude_agent_sdk.types.PermissionUpdate] | None` | No | `updatedPermissions` | Can be omitted when nil |

#### PermissionResultDeny

Deny permission result.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `behavior` | `"deny"` | No | |
| `message` | `string` | No | |
| `interrupt` | `boolean` | No | |

#### PermissionUpdate

Permission update configuration.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `type` | `"addRules" | "replaceRules" | "removeRules" | "setMode" | "addDirectories" | "removeDirectories"` | Yes | |
| `rules` | `list[claude_agent_sdk.types.PermissionRuleValue] | None` | No | |
| `behavior` | `"allow" | "deny" | "ask" | null` | No | |
| `mode` | `"default" | "acceptEdits" | "plan" | "bypassPermissions" | null` | No | |
| `directories` | `list[str] | None` | No | |
| `destination` | `"userSettings" | "projectSettings" | "localSettings" | "session" | null` | No | |

#### PermissionRuleValue

Permission rule value.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tool_name` | `string` | Yes | |
| `rule_content` | `str | None` | No | |

#### ToolPermissionContext

Context information for tool permission callbacks.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `signal` | `typing.Any | None` | No | |
| `suggestions` | `Array<PermissionUpdate>` | No | |

### Permission Modes

```python
PermissionMode = "default" | "acceptEdits" | "plan" | "bypassPermissions"
```

| Mode | Description |
|------|-------------|
| `default` | CLI prompts user for dangerous tool operations |
| `acceptEdits` | Auto-accept file modifications |
| `plan` | Review plan before execution |
| `bypassPermissions` | Allow all tools (dangerous!) |


---

## 8. MCP Server Configuration

The SDK supports multiple MCP server transport types.

### Server Config Union

```
McpServerConfig = McpStdioServerConfig | McpSSEServerConfig | McpHttpServerConfig | McpSdkServerConfig
```


#### McpStdioServerConfig

MCP stdio server configuration.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `type` | `"stdio"` | No | |
| `command` | `string` | Yes | |
| `args` | `Array<string>` | No | |
| `env` | `Record<string, string>` | No | |

#### McpSSEServerConfig

MCP SSE server configuration.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `type` | `"sse"` | Yes | |
| `url` | `string` | Yes | |
| `headers` | `Record<string, string>` | No | |

#### McpHttpServerConfig

MCP HTTP server configuration.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `type` | `"http"` | Yes | |
| `url` | `string` | Yes | |
| `headers` | `Record<string, string>` | No | |

#### McpSdkServerConfig

SDK MCP server configuration.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `type` | `"sdk"` | Yes | |
| `name` | `string` | Yes | |
| `instance` | `any` | Yes | |

---

## 9. SDK Configuration

### ClaudeAgentOptions

The main configuration object for SDK queries.


#### ClaudeAgentOptions

Query options for Claude SDK.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tools` | `list[str] | claude_agent_sdk.types.ToolsPreset | None` | No | |
| `allowed_tools` | `Array<string>` | No | |
| `system_prompt` | `str | claude_agent_sdk.types.SystemPromptPreset | None` | No | |
| `mcp_servers` | `dict[str, claude_agent_sdk.types.McpStdioServerConfig | claude_agent_sdk.types.McpSSEServerConfig | claude_agent_sdk.types.McpHttpServerConfig | claude_agent_sdk.types.McpSdkServerConfig] | str | pathlib.Path` | No | |
| `permission_mode` | `"default" | "acceptEdits" | "plan" | "bypassPermissions" | null` | No | |
| `continue_conversation` | `boolean` | No | |
| `resume` | `str | None` | No | |
| `max_turns` | `int | None` | No | |
| `max_budget_usd` | `float | None` | No | |
| `disallowed_tools` | `Array<string>` | No | |
| `model` | `str | None` | No | |
| `fallback_model` | `str | None` | No | |
| `betas` | `Array<"context-1m-2025-08-07">` | No | |
| `permission_prompt_tool_name` | `str | None` | No | |
| `cwd` | `str | pathlib.Path | None` | No | |
| `cli_path` | `str | pathlib.Path | None` | No | |
| `settings` | `str | None` | No | |
| `add_dirs` | `Array<str | pathlib.Path>` | No | |
| `env` | `Record<string, string>` | No | |
| `extra_args` | `Record<string, str | None>` | No | |
| `max_buffer_size` | `int | None` | No | |
| `debug_stderr` | `any` | No | |
| `stderr` | `collections.abc.Callable[[str], None] | None` | No | |
| `can_use_tool` | `collections.abc.Callable[[str, dict[str, typing.Any], claude_agent_sdk.types.ToolPermissionContext], collections.abc.Awaitable[claude_agent_sdk.types.PermissionResultAllow | claude_agent_sdk.types.PermissionResultDeny]] | None` | No | |
| `hooks` | `dict[typing.Union[typing.Literal['PreToolUse'], typing.Literal['PostToolUse'], typing.Literal['UserPromptSubmit'], typing.Literal['Stop'], typing.Literal['SubagentStop'], typing.Literal['PreCompact']], list[claude_agent_sdk.types.HookMatcher]] | None` | No | |
| `user` | `str | None` | No | |
| `include_partial_messages` | `boolean` | No | |
| `fork_session` | `boolean` | No | |
| `agents` | `dict[str, claude_agent_sdk.types.AgentDefinition] | None` | No | |
| `setting_sources` | `list[typing.Literal['user', 'project', 'local']] | None` | No | |
| `sandbox` | `claude_agent_sdk.types.SandboxSettings | None` | No | |
| `plugins` | `Array<SdkPluginConfig>` | No | |
| `max_thinking_tokens` | `int | None` | No | |
| `output_format` | `dict[str, typing.Any] | None` | No | |
| `enable_file_checkpointing` | `boolean` | No | |

#### SandboxSettings

Sandbox settings configuration.

    This controls how Claude Code sandboxes bash commands for filesystem
    and network isolation.

    **Important:** Filesystem and network restrictions are configured via permission
    rules, not via these sandbox settings:
    - Filesystem read restrictions: Use Read deny rules
    - Filesystem write restrictions: Use Edit allow/deny rules
    - Network restrictions: Use WebFetch allow/deny rules

    Attributes:
        enabled: Enable bash sandboxing (macOS/Linux only). Default: False
        autoAllowBashIfSandboxed: Auto-approve bash commands when sandboxed. Default: True
        excludedCommands: Commands that should run outside the sandbox (e.g., ["git", "docker"])
        allowUnsandboxedCommands: Allow commands to bypass sandbox via dangerouslyDisableSandbox.
            When False, all commands must run sandboxed (or be in excludedCommands). Default: True
        network: Network configuration for sandbox.
        ignoreViolations: Violations to ignore.
        enableWeakerNestedSandbox: Enable weaker sandbox for unprivileged Docker environments
            (Linux only). Reduces security. Default: False

    Example:
        ```python
        sandbox_settings: SandboxSettings = {
            "enabled": True,
            "autoAllowBashIfSandboxed": True,
            "excludedCommands": ["docker"],
            "network": {
                "allowUnixSockets": ["/var/run/docker.sock"],
                "allowLocalBinding": True
            }
        }
        ```
    

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `enabled` | `boolean` | Yes | |
| `autoAllowBashIfSandboxed` | `boolean` | Yes | |
| `excludedCommands` | `Array<string>` | Yes | |
| `allowUnsandboxedCommands` | `boolean` | Yes | |
| `network` | `SandboxNetworkConfig` | Yes | |
| `ignoreViolations` | `SandboxIgnoreViolations` | Yes | |
| `enableWeakerNestedSandbox` | `boolean` | Yes | |

#### SandboxNetworkConfig

Network configuration for sandbox.

    Attributes:
        allowUnixSockets: Unix socket paths accessible in sandbox (e.g., SSH agents).
        allowAllUnixSockets: Allow all Unix sockets (less secure).
        allowLocalBinding: Allow binding to localhost ports (macOS only).
        httpProxyPort: HTTP proxy port if bringing your own proxy.
        socksProxyPort: SOCKS5 proxy port if bringing your own proxy.
    

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `allowUnixSockets` | `Array<string>` | Yes | |
| `allowAllUnixSockets` | `boolean` | Yes | |
| `allowLocalBinding` | `boolean` | Yes | |
| `httpProxyPort` | `integer` | Yes | |
| `socksProxyPort` | `integer` | Yes | |

#### AgentDefinition

Agent definition configuration.

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `description` | `string` | Yes | |
| `prompt` | `string` | Yes | |
| `tools` | `list[str] | None` | No | |
| `model` | `"sonnet" | "opus" | "haiku" | "inherit" | null` | No | |