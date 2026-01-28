# Claude CLI Protocol Analysis: Plan Mode Experiments

## Experiment Overview

Three experiments comparing different approaches to entering plan mode:

| Experiment | Description | Initial Mode | Control Message |
|------------|-------------|--------------|-----------------|
| A | Default mode → Send message → Switch to Plan | `default` | After turn completes |
| B | Start directly in Plan mode | `plan` | None |
| C | Default mode → Switch to Plan BEFORE user message | `default` → `plan` | Before user message |

**Key Change**: All experiments run WITHOUT `--dangerously-skip-permissions` flag to see actual permission mode behavior.

## Key Findings

### 1. Permission Mode is Correctly Reported

Without `--dangerously-skip-permissions`, the CLI correctly reports the actual permission mode:

| Experiment | Reported Permission Mode | Notes |
|------------|-------------------------|-------|
| A | `default` | Started in default mode |
| B | `plan` | Started directly in plan mode |
| C | `plan` | Mode changed before init via control message |

### 2. Control Message Before User Message Works!

**This is a key discovery**: Sending a `set_permission_mode` control message BEFORE the first user message successfully changes the mode. The CLI processes the control message and applies it before sending the init message.

- In Experiment C, the control message was sent first
- The init message (triggered by user message) shows `permissionMode: "plan"`
- No control response was recorded (it may be processed internally before the recorder initializes)

### 3. Plan Mode Affects Claude's Response Style

Claude's responses differ significantly based on the active permission mode:

**Default Mode (Experiment A)**:
```
"2 + 2 = 4"
```
Cost: $0.006632, Messages: 11

**Plan Mode (Experiments B & C)**:
```
"The answer is 4.

However, I notice that plan mode is currently active. This appears to be a simple
arithmetic question rather than a task that requires planning and implementation.

If you'd like to exit plan mode and have a regular conversation, you can do so..."
```
Cost: $0.08952 (B) / $0.02505 (C), Messages: 45/60

### 4. Message Flow Comparison

| Experiment | FROM CLI Messages | TO CLI Messages | Control Responses |
|------------|-------------------|-----------------|-------------------|
| A (Default→Plan) | 11 | 1 | 2 |
| B (Plan from start) | 45 | 1 | 0 |
| C (Plan before msg) | 60 | 1 | 0 |

### 5. Control Response Structure

When switching mode via control message (Experiment A - AFTER turn):
```json
// Response 1: Mode confirmation
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_1769536533672486000_6ccb9e9b619fb3c1",
    "response": {
      "mode": "plan"
    }
  }
}

// Response 2: Acknowledgment
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_1769536533672486000_6ccb9e9b619fb3c1"
  }
}
```

### 6. Protocol Timing Insights

1. **CLI sends init AFTER first user message**: The system init message is only sent after the CLI receives the first user message, not at session start.

2. **Control messages work pre-init**: Control messages sent before the first user message are processed and affect the init state.

3. **Control responses come post-turn**: When switching mode after a turn completes, control responses are sent after the result message.

## Protocol Flow Diagrams

### Experiment A: Default → Message → Switch to Plan
```
Client                           CLI
  |                               |
  |-- start session (default) --->|
  |                               | (waiting for input)
  |-- user message -------------->|
  |                               |
  |<-- system init (default) -----|
  |<-- stream_event (start) ------|
  |<-- stream_event (deltas) -----|
  |<-- assistant ------------------|
  |<-- stream_event (stop) -------|
  |<-- result (success) ----------|
  |                               |
  |-- control_request ----------->|
  |   (set_permission_mode:plan)  |
  |                               |
  |<-- control_response (mode) ---|
  |<-- control_response (ack) ----|
```

### Experiment C: Default → Switch → Message (Pre-init control)
```
Client                           CLI
  |                               |
  |-- start session (default) --->|
  |                               | (waiting for input)
  |-- control_request ----------->|
  |   (set_permission_mode:plan)  |
  |                               | (mode updated internally)
  |-- user message -------------->|
  |                               |
  |<-- system init (plan!) -------|  <-- Mode already changed!
  |<-- stream_event (start) ------|
  |<-- stream_event (deltas) -----|
  |<-- assistant ------------------|
  |<-- stream_event (stop) -------|
  |<-- result (success) ----------|
```

## Practical Implications

1. **To enter plan mode efficiently**: Send `set_permission_mode` control message BEFORE the first user message. This is equivalent to starting with `--permission-mode plan` flag.

2. **Mode affects LLM behavior**: Plan mode adds system instructions that change how Claude responds - it becomes more cautious and explains the mode context.

3. **Cost implications**: Plan mode responses are significantly more expensive due to longer responses explaining the mode context.

4. **Dual control response**: When switching modes after a turn, expect TWO control_response messages.

## Files Generated

- `recordings/experiment_a/` - Default mode with post-turn mode switch
- `recordings/experiment_b/` - Direct plan mode start
- `recordings/experiment_c/` - Default mode with pre-message mode switch

---

# Permission Prompt Tool Analysis

## Overview

The `--permission-prompt-tool stdio` flag routes permission prompts through the stdio protocol as `can_use_tool` control requests, enabling programmatic permission handling by the SDK.

## New Experiments

| Experiment | Description | Flag | Permission Mode | Handler |
|------------|-------------|------|-----------------|---------|
| D | Permission prompt tool + Bash (Allow) | `--permission-prompt-tool stdio` | default | AllowAll |
| E | Permission prompt tool + Bash (Deny) | `--permission-prompt-tool stdio` | default | DenyAll |
| F | Control - WITHOUT flag | (none) | default | AllowAll |
| G | Permission prompt tool + Plan mode | `--permission-prompt-tool stdio` | plan | AllowAll |
| H | Permission prompt tool + Bypass | `--permission-prompt-tool stdio` | bypassPermissions | AllowAll |

## Expected Behavior

### Without `--permission-prompt-tool stdio` (Experiment F - Control)

```
Client                              CLI
  |-- user message (run bash) ----->|
  |<-- assistant (tool_use: Bash) --|
  |                                  |
  |   (CLI handles permission       |
  |    internally - no stdio msg)   |
  |                                  |
  |<-- user (tool_result) ----------|
  |<-- result ----------------------|
```

- No `can_use_tool` control request through stdio
- SDK's `PermissionHandler` is NOT invoked for built-in tools
- CLI handles permissions internally (mechanism not observed in this experiment)

### With `--permission-prompt-tool stdio` (Experiments D, E)

```
Client                              CLI
  |-- user message (run bash) ----->|
  |<-- assistant (tool_use: Bash) --|
  |                                  |
  |<-- control_request --------------|
  |    type: control_request         |
  |    request.subtype: can_use_tool |
  |    request.tool_name: Bash       |
  |    request.input: {command: ...} |
  |                                  |
  |-- control_response ------------->|
  |    response.behavior: allow/deny |
  |                                  |
  |<-- user (tool_result) ----------|
  |<-- result ----------------------|
```

- All permission prompts flow through stdio
- SDK receives `can_use_tool` control requests
- SDK responds with `control_response` containing `behavior: "allow"` or `behavior: "deny"`

## Key Questions to Answer

1. **Does `--permission-prompt-tool stdio` send `can_use_tool` for built-in tools?**
   - Expected: Yes (Experiments D, E should show can_use_tool requests)
   - Control (F) should show none

2. **How does denial affect Claude's behavior?**
   - Expected: Claude receives tool error and may explain the denial

3. **How does it interact with Plan mode?**
   - Expected: Still sends can_use_tool, but Claude is in plan mode context

4. **Does `bypassPermissions` override the stdio flag?**
   - Expected: No can_use_tool requests when using bypassPermissions

## Message Types

### can_use_tool Control Request (FROM CLI)

```json
{
  "type": "control_request",
  "request_id": "req_...",
  "request": {
    "subtype": "can_use_tool",
    "tool_name": "Bash",
    "input": {
      "command": "echo 'hello world'"
    },
    "permission_suggestions": [...],
    "blocked_path": null
  }
}
```

### Permission Response - Allow (TO CLI)

**IMPORTANT**: The CLI's Zod schema expects **camelCase** field names for permission responses,
despite the Python SDK type definitions using snake_case. This is a key protocol detail.

```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_...",
    "response": {
      "behavior": "allow",
      "updatedInput": {"command": "echo hello"}
    }
  }
}
```

**Critical wire format notes (per Python SDK behavior):**

1. **Field names MUST be camelCase**: `updatedInput`, `updatedPermissions` (not snake_case)

2. **`updatedInput` MUST be an object, NEVER null**:
   - If user doesn't provide `updated_input`, SDK must fall back to the **original input** from the request
   - This ensures the tool receives valid input parameters
   - The Python SDK does this in `_internal/query.py`:
     ```python
     "updatedInput": (
         response.updated_input
         if response.updated_input is not None
         else original_input
     ),
     ```

3. **`updatedPermissions` CAN be omitted when nil**:
   - Unlike `updatedInput`, this field can be omitted entirely if there are no permission updates
   - Only include it when you want to add permission rules
   - The Python SDK only adds this field when `updated_permissions is not None`

**Example with permission updates:**
```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_...",
    "response": {
      "behavior": "allow",
      "updatedInput": {"command": "echo hello"},
      "updatedPermissions": [
        {"type": "setMode", "mode": "acceptEdits", "destination": "session"}
      ]
    }
  }
}
```

**Common mistakes that cause Zod validation errors:**
- Using `null` for `updatedInput` → Use original input as fallback
- Using snake_case (`updated_input`) → Use camelCase (`updatedInput`)
- Omitting `updatedInput` entirely → Must be present as an object

### Permission Response - Deny (TO CLI)

```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_...",
    "response": {
      "behavior": "deny",
      "message": "Permission denied",
      "interrupt": false
    }
  }
}
```

## Practical Implications

1. **Programmatic Permission Control**: With `--permission-prompt-tool stdio`, SDKs can implement custom permission policies without user interaction.

2. **Automation Use Cases**: Essential for CI/CD pipelines and automated agents that need to make permission decisions programmatically.

3. **MCP vs Built-in Tools**: Without the flag, only MCP tools trigger the SDK's permission handler. With the flag, ALL tools go through the SDK.

4. **Security Consideration**: The SDK becomes responsible for permission decisions - must implement appropriate security policies.

## Files Generated

- `recordings/experiment_d/` - Permission prompt tool with allow handler
- `recordings/experiment_e/` - Permission prompt tool with deny handler
- `recordings/experiment_f/` - Control without permission prompt tool flag
- `recordings/experiment_g/` - Permission prompt tool with plan mode
- `recordings/experiment_h/` - Permission prompt tool with bypass permissions

## Findings

### Experiment Results (2026-01-27)

| Experiment | can_use_tool Requests | Permission Responses | Cost | Tools Requested |
|------------|----------------------|---------------------|------|-----------------|
| D (stdio + allow) | 3 | 3 | $0.021507 | Edit, Write |
| E (stdio + deny) | 2 | 2 | $0.016977 | Edit, Bash |
| F (control/no flag) | 0 | 0 | $0.013281 | (none) |
| G (stdio + plan) | 2 | 2 | $0.128360 | ExitPlanMode |
| H (stdio + bypass) | 0 | 0 | $0.013111 | (none - bypassed) |

### Key Discovery: `--permission-prompt-tool stdio` Works for Built-in Tools

**Critical finding**: The `--permission-prompt-tool stdio` flag triggers `can_use_tool` control requests for ALL tool invocations that require permission:

1. **With AllowAllPermissionHandler (Experiment D)**: 3 requests (Edit×2, Write×1), all approved
2. **With DefaultPermissionHandler/Deny (Experiment E)**: 2 requests (Edit, Bash), both denied - Claude tried fallback
3. **Without the flag (Experiment F)**: 0 requests - CLI handles via TTY or auto-handles
4. **With bypassPermissions mode (Experiment H)**: 0 requests - bypass takes precedence

### Detailed Findings

#### Experiment D - Allow Handler with stdio flag
Claude successfully fixed the broken Python file:
1. **Edit tool** (×2): Attempted to fix `hello.py` with colon - ALLOWED
2. **Write tool**: Wrote the fixed file - ALLOWED

```json
// Example can_use_tool request from CLI:
{
  "type": "control_request",
  "request": {
    "subtype": "can_use_tool",
    "tool_name": "Edit",
    "input": {"file_path": "/tmp/.../hello.py", ...},
    "permission_suggestions": [{"type": "setMode", "mode": "acceptEdits", "destination": "session"}]
  }
}
```

#### Experiment E - Deny Handler with stdio flag
Claude attempted to fix the broken Python file but was blocked:
1. **Edit tool**: Tried to fix `hello.py` - DENIED
2. **Bash tool**: Tried `sed` command as fallback - DENIED

When denied, Claude receives an error and may attempt alternative approaches.

#### Experiment G - Plan Mode with stdio flag
The `ExitPlanMode` tool uses `can_use_tool` for **plan approval communication**:
- Claude created a plan to fix the Python file
- `ExitPlanMode` was called with the full plan content in `input.plan` field
- Called twice (cost: $0.128360) - Claude refined the plan
- Responding with `allow` means: "Yes, proceed with implementation"

**Note**: This is semantic plan approval, not permission for a dangerous action. The SDK can review the plan content before approving.

#### Experiment F - Control (No stdio flag)
Without `--permission-prompt-tool stdio`:
- No `can_use_tool` requests flow through stdio
- CLI handles permissions internally (mechanism not observed)
- Lowest cost ($0.013281) - direct execution path

#### Experiment H - BypassPermissions
With `--permission-mode bypassPermissions`:
- All permissions are automatically granted
- The stdio flag has no effect - no permission checks occur
- This confirms `bypassPermissions` takes precedence over stdio flag

### Permission Suggestions

The CLI provides helpful `permission_suggestions` in `can_use_tool` requests:
- `setMode: acceptEdits` - Suggests switching to accept edits mode
- `addDirectories` - Suggests adding the working directory to allowed paths

SDKs can use these to implement smart permission policies.

### Revised Understanding of Protocol

```
With --permission-prompt-tool stdio:

1. default mode + stdio:
   CLI → can_use_tool (control_request) → SDK
   SDK → allow/deny (control_response) → CLI
   CLI → execute or show error to Claude

2. plan mode + stdio:
   Same as above, but even ExitPlanMode requires permission

3. bypassPermissions + stdio:
   CLI → execute tool directly (no permission check)

Without --permission-prompt-tool stdio:

1. default mode:
   CLI → internal permission handling → execute
   (no can_use_tool requests through stdio)

2. bypassPermissions:
   CLI → execute directly
```

### Practical Implications

1. **For Full Programmatic Control**: Use `--permission-prompt-tool stdio` with a `PermissionHandler`
   - Implement allow/deny logic in the SDK
   - Can make decisions based on tool name, input, context

2. **For Auto-Approval**: Use `--permission-mode bypassPermissions` OR `AllowAllPermissionHandler()`
   - Both achieve the same result
   - `bypassPermissions` is slightly more efficient (no protocol round-trip)

3. **For Selective Control**: Use `--permission-prompt-tool stdio` with custom handler
   - Allow some tools (Read, Glob)
   - Deny others (Bash, Write)
   - Use `permission_suggestions` to inform decisions

4. **Plan Mode Consideration**: `ExitPlanMode` uses `can_use_tool` for plan approval
   - This is semantic plan approval, not permission for dangerous actions
   - The plan content is included in `input.plan`
   - SDK can programmatically review plans before approving
   - Deny = ask Claude to revise; Allow = proceed with implementation
