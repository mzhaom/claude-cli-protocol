# ClaudeSession Integration Tests

This directory contains comprehensive integration tests for the ClaudeSession API with full session trace recording.

## Test Structure

The test suite validates three key scenarios:

### Scenario 1: Bypass Permission Mode
- **Test**: Multi-step research task with bypass permissions
- **Flow**:
  1. Search latest news about US tariff rates (China/Japan/EU)
  2. Save results to CSV file
  3. Create Python code for HTML chart visualization
- **Validates**:
  - Tool execution without permission prompts
  - Session recording captures all events
  - Hook callbacks fire correctly (onReady, onToolStart, onTurnComplete)
  - Files are created (CSV, Python)

### Scenario 2: Default Permission Mode
- **Test**: Same task with permission approval flow
- **Flow**: Same 3 steps as Scenario 1
- **Validates**:
  - Permission requests are received via `onPermissionRequest`
  - Auto-approval mechanism works
  - Control request/response messages in trace
  - Same output as bypass mode after approval

### Scenario 3: Plan Mode
- **Test**: Combined request with plan mode handling
- **Flow**: Single message combining all 3 tasks
- **Validates**:
  - Plan presentation via ExitPlanMode tool
  - Plan approval flow
  - AskUserQuestion handling
  - Task execution after plan approval

## Test Artifacts

All test runs create a persistent directory structure:

```
/tmp/claude-session-tests-{timestamp}/
├── scenario-1-bypass-permissions/
│   ├── traces/
│   │   ├── session-*.jsonl          # Message log
│   │   └── session-*.meta.json      # Session metadata
│   ├── *.csv                        # Generated CSV files
│   └── *.py                         # Generated Python scripts
├── scenario-2-default-permissions/
│   └── [same structure]
└── scenario-3-plan-mode/
    └── [same structure]
```

**Important**: Test directories are **preserved after test completion** for manual inspection.

## Running Tests

```bash
# Run all integration tests
npm run test:integration

# Run only ClaudeSession tests
npx vitest run src/main/session/protocol/__tests__/ClaudeSession.integration.test.ts
```

## Session Recording Format

Each test generates session recordings in JSONL format:

### `.jsonl` file
One JSON object per line, containing all protocol messages (sent/received):
- User messages
- Assistant responses
- Stream events
- Control requests/responses
- Tool results

### `.meta.json` file
Session metadata and turn summaries:
```json
{
  "sessionId": "...",
  "model": "claude-haiku-4-5-20251001",
  "permissionMode": "bypassPermissions",
  "totalTurns": 3,
  "totalMessages": 42,
  "turns": [
    {
      "turnNumber": 1,
      "userMessage": "...",
      "assistantText": "...",
      "toolsUsed": ["WebSearch"],
      "cost": 0.00123,
      "tokens": { "input": 100, "output": 200 }
    }
  ]
}
```

## Test Features

### In-Memory Hook Tracking
Each test uses callbacks to track events without file I/O:
- `onReady` - Session initialization
- `onToolStart` - Tool execution begins
- `onToolComplete` - Tool execution completes
- `onTurnComplete` - Turn finishes with metrics
- `onPermissionRequest` - Permission requested (Scenario 2)

### Assertions
Tests validate:
- ✅ All turns complete successfully
- ✅ Expected tools were used (WebSearch, Write)
- ✅ Session recordings contain all data
- ✅ Permission flows work correctly
- ✅ Files are created in test directory
- ✅ Hook events fire in correct order

### Isolation
- Each test uses a deterministic subdirectory
- Tests run with isolated Claude CLI instances
- No side effects between tests

## Prerequisites

- Claude CLI installed and available in PATH
- Valid API key configured
- Node.js and npm dependencies installed

## Timeout Configuration

- Scenario 1 & 2: 120 seconds (research tasks take time)
- Scenario 3: 150 seconds (plan mode may need more time)

## Notes

- Tests use **haiku model by default** for cost efficiency
- Real API calls are made (WebSearch for tariff news)
- Tests can be skipped if Claude CLI is not available
- Traces are invaluable for debugging protocol issues
