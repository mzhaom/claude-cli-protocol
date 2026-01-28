# Codex CLI Wire Protocol

This document describes the wire protocol used for communication between a client and the Codex CLI agent.

## Source of Truth

The authoritative protocol definitions are in the Codex Rust codebase:

| Component | Source File | Description |
|-----------|-------------|-------------|
| Submission/Op types | `codex-rs/protocol/src/protocol.rs:56-248` | Operations sent to Codex |
| EventMsg types | `codex-rs/protocol/src/protocol.rs:614-793` | Events from Codex |
| Approval types | `codex-rs/protocol/src/approvals.rs` | Exec/Patch approval requests |
| User input types | `codex-rs/protocol/src/user_input.rs` | UserInput variants |
| Policy enums | `codex-rs/protocol/src/protocol.rs:250-356` | AskForApproval, SandboxPolicy |
| Protocol spec | `codex-rs/docs/protocol_v1.md` | High-level protocol documentation |

## Transport Layer

- **Format**: NDJSON (Newline-Delimited JSON)
- **Pattern**: SQ (Submission Queue) / EQ (Event Queue)
- **Direction**: Bidirectional asynchronous communication
- **Transport**: stdin/stdout pipes when using `codex exec`

Each message is a single JSON object followed by a newline character (`\n`).

## Message Structure

### Submission (Client → Server)

Submissions are requests from the client to Codex:

```json
{
  "id": "sub_123456",
  "op": {
    "type": "user_input",
    "items": [{"type": "text", "text": "Hello"}]
  }
}
```

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Unique submission ID for correlation with events |
| `op` | Op | Operation payload (tagged union with `type` field) |

### Event (Server → Client)

Events are messages from Codex to the client:

```json
{
  "id": "sub_123456",
  "msg": {
    "type": "agent_message",
    "message": "Hello! How can I help you?"
  }
}
```

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Correlated submission ID |
| `msg` | EventMsg | Event payload (tagged union with `type` field) |

## Operations (Op)

Operations are sent from the client to Codex. All operations use `snake_case` type tags.

Source: `codex-rs/protocol/src/protocol.rs:72-248`

### Session Control

| Type | Description | Fields |
|------|-------------|--------|
| `interrupt` | Abort current task | (none) |
| `shutdown` | Terminate session | (none) |
| `compact` | Summarize conversation context | (none) |
| `undo` | Undo last turn | (none) |
| `thread_rollback` | Drop last N user turns | `num_turns: u32` |

### User Input

| Type | Description | Fields |
|------|-------------|--------|
| `user_input` | Basic user input | `items: UserInput[]`, `final_output_json_schema?: object` |
| `user_turn` | Full turn with config | `items`, `cwd`, `approval_policy`, `sandbox_policy`, `model`, `effort?`, `summary`, `final_output_json_schema?` |
| `override_turn_context` | Update session defaults | `cwd?`, `approval_policy?`, `sandbox_policy?`, `model?`, `effort?`, `summary?` |

### Approvals

| Type | Description | Fields |
|------|-------------|--------|
| `exec_approval` | Approve/deny command execution | `id: string`, `decision: ReviewDecision` |
| `patch_approval` | Approve/deny file patches | `id: string`, `decision: ReviewDecision` |
| `resolve_elicitation` | Resolve MCP elicitation | `server_name`, `request_id`, `decision: ElicitationAction` |

### Information Requests

| Type | Description | Fields |
|------|-------------|--------|
| `list_mcp_tools` | List available MCP tools | (none) |
| `list_custom_prompts` | List custom prompts | (none) |
| `list_skills` | List skills for cwds | `cwds?: PathBuf[]`, `force_reload?: bool` |
| `list_models` | List available models | (none) |

### Other

| Type | Description | Fields |
|------|-------------|--------|
| `review` | Request code review | `review_request: ReviewRequest` |
| `add_to_history` | Add entry to history | `text: string` |
| `get_history_entry_request` | Get history entry | `offset: usize`, `log_id: u64` |
| `refresh_mcp_servers` | Refresh MCP servers | `config: McpServerRefreshConfig` |
| `run_user_shell_command` | Execute shell command | `command: string` |

## Events (EventMsg)

Events are sent from Codex to the client. All events use `snake_case` type tags.

Source: `codex-rs/protocol/src/protocol.rs:625-793`

### Session Events

| Type | Description | Key Fields |
|------|-------------|------------|
| `session_configured` | Session initialized | `session_id`, `model`, `cwd`, `tools[]` |
| `shutdown_complete` | Session terminated | (none) |

### Turn Lifecycle

| Type | Aliases | Description | Key Fields |
|------|---------|-------------|------------|
| `task_started` | `turn_started` | Turn began | `model_context_window?` |
| `task_complete` | `turn_complete` | Turn finished | `last_agent_message?` |
| `turn_aborted` | | Turn interrupted | `message?` |

**Note**: For v1 wire compatibility, `TurnStarted` serializes as `task_started` and `TurnComplete` as `task_complete`. Deserializers accept both tags.

### Agent Output

| Type | Description | Key Fields |
|------|-------------|------------|
| `agent_message` | Complete agent text | `message: string` |
| `agent_message_delta` | Streaming text chunk | `delta: string` |
| `agent_reasoning` | Complete reasoning | `text: string` |
| `agent_reasoning_delta` | Streaming reasoning chunk | `delta: string` |
| `agent_reasoning_raw_content` | Raw chain-of-thought | `text: string` |
| `agent_reasoning_raw_content_delta` | Streaming raw reasoning | `delta: string` |
| `user_message` | User input echoed | `message: string`, `images?` |

### Approval Requests

| Type | Description | Key Fields |
|------|-------------|------------|
| `exec_approval_request` | Request command approval | `call_id`, `turn_id`, `command[]`, `cwd`, `reason?`, `proposed_execpolicy_amendment?`, `parsed_cmd[]` |
| `apply_patch_approval_request` | Request patch approval | `call_id`, `turn_id`, `changes: Map<PathBuf, FileChange>`, `reason?`, `grant_root?` |
| `elicitation_request` | MCP elicitation request | `server_name`, `id`, `message` |

### Command Execution

| Type | Description | Key Fields |
|------|-------------|------------|
| `exec_command_begin` | Command starting | `call_id`, `process_id?`, `turn_id`, `command[]`, `cwd`, `parsed_cmd[]`, `source` |
| `exec_command_output_delta` | Command output chunk | `call_id`, `stdout?` (base64), `stderr?` (base64) |
| `exec_command_end` | Command finished | `call_id`, `process_id?`, `turn_id`, `command[]`, `cwd`, `exit_code`, `timed_out` |
| `terminal_interaction` | Terminal interaction | `call_id`, `stdin_sent?`, `stdout_observed?` |

### Patch Application

| Type | Description | Key Fields |
|------|-------------|------------|
| `patch_apply_begin` | Patch starting | `call_id`, `turn_id`, `changes`, `reason?` |
| `patch_apply_end` | Patch finished | `call_id`, `turn_id`, `changes`, `success`, `error?` |

### MCP Events

| Type | Description | Key Fields |
|------|-------------|------------|
| `mcp_startup_update` | MCP server startup progress | `server_name`, `status` |
| `mcp_startup_complete` | All MCP servers ready | `servers[]` |
| `mcp_tool_call_begin` | MCP tool starting | `call_id`, `invocation: {server, tool, arguments?}` |
| `mcp_tool_call_end` | MCP tool finished | `call_id`, `invocation`, `duration`, `result` |
| `mcp_list_tools_response` | List of MCP tools | `tools[]` |

### Status Events

| Type | Description | Key Fields |
|------|-------------|------------|
| `error` | Turn failed | `message`, `codex_error_info?` |
| `warning` | Non-fatal warning | `message` |
| `token_count` | Token usage update | `info?: TokenUsageInfo`, `rate_limits?` |
| `background_event` | Informational message | `message` |
| `stream_error` | Connection/retry info | `message`, `retry_in_ms?` |

### Context Events

| Type | Description | Key Fields |
|------|-------------|------------|
| `context_compacted` | History summarized | (none) |
| `thread_rolled_back` | Turns removed | `num_turns`, `success`, `error?` |

### Other Events

| Type | Description | Key Fields |
|------|-------------|------------|
| `deprecation_notice` | Deprecation warning | `message` |
| `undo_started` | Undo in progress | (none) |
| `undo_completed` | Undo finished | `success`, `error?` |
| `list_custom_prompts_response` | Custom prompts list | `prompts[]` |
| `list_skills_response` | Skills list | `skills[]` |
| `skills_update_available` | Skills may have changed | (none) |
| `turn_diff` | Diff for turn | `diff` |
| `get_history_entry_response` | History entry | `entry?` |
| `entered_review_mode` | Entered review mode | `target`, `user_facing_hint?` |
| `exited_review_mode` | Exited review mode | `review_output?` |

## Policy Enums

### AskForApproval (Approval Policy)

Source: `codex-rs/protocol/src/protocol.rs:250-289`

| Value | Description |
|-------|-------------|
| `untrusted` | Only auto-approve known safe read-only commands |
| `on-failure` | Auto-approve, ask only if command fails |
| `on-request` | Ask user for approval (default) |
| `never` | Never ask, return errors immediately |

### SandboxPolicy

Source: `codex-rs/protocol/src/protocol.rs:310-356`

| Value | Description |
|-------|-------------|
| `danger-full-access` | No restrictions |
| `read-only` | Read-only access to filesystem |
| `external-sandbox` | Already in container, honors network setting |
| `workspace-write` | Read all, write to cwd + /tmp |

The `workspace-write` variant has additional fields:
- `writable_roots`: Additional writable directories
- `network_access`: Boolean for network access
- `exclude_tmpdir_env_var`: Exclude $TMPDIR from writable
- `exclude_slash_tmp`: Exclude /tmp from writable

### ReviewDecision

| Value | Description |
|-------|-------------|
| `approved` | Allow execution |
| `approved_for_session` | Allow for session duration |
| `approved_with_amendment` | Allow + update execpolicy rules |
| `denied` | Reject, continue |
| `abort` | Reject, stop session |

### ElicitationAction

| Value | Description |
|-------|-------------|
| `accept` | Accept the elicitation |
| `decline` | Decline the elicitation |
| `cancel` | Cancel the elicitation |

## User Input Types

Source: `codex-rs/protocol/src/user_input.rs`

```json
// Text input
{"type": "text", "text": "Hello, world"}

// Pre-encoded image (data URL)
{"type": "image", "image_url": "data:image/png;base64,..."}

// Local image path (converted to base64 during serialization)
{"type": "local_image", "path": "/path/to/image.png"}

// Skill selection
{"type": "skill", "name": "my-skill", "path": "/path/to/SKILL.md"}
```

## File Change Types

Used in patch approval requests:

```json
{
  "/path/to/file.txt": {
    "type": "update",
    "unified_diff": "--- a/file.txt\n+++ b/file.txt\n...",
    "move_path": null
  }
}
```

| Type | Description | Fields |
|------|-------------|--------|
| `add` | New file | `content: string` |
| `update` | Modified file | `unified_diff: string`, `move_path?: string` |
| `delete` | Removed file | (none) |

## Error Types

Source: `codex-rs/protocol/src/protocol.rs:863-891`

| Code | Description |
|------|-------------|
| `context_window_exceeded` | Model context full |
| `usage_limit_exceeded` | Rate limit or quota |
| `http_connection_failed` | Network issue |
| `response_stream_connection_failed` | Can't connect to model |
| `response_stream_disconnected` | Connection dropped mid-turn |
| `response_too_many_failed_attempts` | Retry exhausted |
| `unauthorized` | Auth failed |
| `bad_request` | Invalid request |
| `sandbox_error` | Execution environment issue |
| `internal_server_error` | Server error |
| `thread_rollback_failed` | Couldn't undo |
| `other` | Unknown error |

## Example Session Flow

```
Client                                  Codex
   |                                      |
   |-- Submission{id:"1", op:UserInput} ->|
   |                                      |
   |<- Event{id:"1", msg:SessionConfigured}
   |<- Event{id:"1", msg:TurnStarted}     |
   |<- Event{id:"1", msg:AgentMessageDelta}
   |<- Event{id:"1", msg:AgentMessageDelta}
   |<- Event{id:"1", msg:ExecApprovalRequest}
   |                                      |
   |-- Submission{id:"2", op:ExecApproval}->
   |                                      |
   |<- Event{id:"1", msg:ExecCommandBegin}|
   |<- Event{id:"1", msg:ExecCommandOutputDelta}
   |<- Event{id:"1", msg:ExecCommandEnd}  |
   |<- Event{id:"1", msg:AgentMessage}    |
   |<- Event{id:"1", msg:TurnComplete}    |
   |                                      |
   |-- Submission{id:"3", op:Shutdown} -->|
   |                                      |
   |<- Event{id:"3", msg:ShutdownComplete}|
```

## References

- [Codex Protocol V1 Spec](https://github.com/openai/codex/blob/main/codex-rs/docs/protocol_v1.md)
- [Codex MCP Interface](https://github.com/openai/codex/blob/main/codex-rs/docs/codex_mcp_interface.md)
