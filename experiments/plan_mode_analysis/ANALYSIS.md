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
