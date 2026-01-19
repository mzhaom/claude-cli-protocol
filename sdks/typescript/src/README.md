# Claude Code CLI Session API

High-level API for building UIs on top of Claude Code CLI. Handles multi-turn conversations and detailed tool lifecycle events.

## Quick Start

### Event-Driven (Callbacks)

```typescript
import { ClaudeSession } from './protocol'

const session = new ClaudeSession({
  model: 'haiku',
  onText: (chunk) => appendToChat(chunk.text),
  onToolComplete: (tool) => executeTool(tool),
  onTurnComplete: (turn) => console.log(`Cost: $${turn.usage.costUSD}`)
})

session.start()
session.sendMessage('Hello!')
```

### Imperative (Async/Await)

```typescript
const session = new ClaudeSession({ model: 'haiku' })
session.start()

const result = await session.ask('What is 2+2?')
console.log(`Cost: $${result.usage.costUSD}`)
session.stop()
```

## Core Concepts

**Multi-Turn Sessions**: Each turn = one user message + Claude's complete response.

**Tool Execution Model**: Claude CLI automatically executes ALL tools (Read, Write, Bash, WebSearch, etc.). You receive results via `onCliToolResult` callback - no manual execution needed!

**Tool Lifecycle**: `onToolStart` → `onToolProgress` → `onToolComplete` → `onCliToolResult` (CLI auto-executed)

**Initialization**: `start()` → `sendMessage()` (triggers init) → `onReady()` fires. Use `ask()` to avoid thinking about this.

## API Reference

### SessionConfig Options

| Option | Type | Description |
|--------|------|-------------|
| `model` | `string` | Model: `'haiku'`, `'sonnet'`, `'opus'` |
| `cwd` | `string` | Working directory |
| `permissionMode` | `string` | `'default'` \| `'acceptEdits'` \| `'plan'` \| `'bypassPermissions'` |
| **Lifecycle Callbacks** | | |
| `onReady` | `(info) => void` | Session initialized (after first message) |
| `onText` | `(chunk) => void` | Text streaming (incremental) |
| `onThinking` | `(chunk) => void` | Thinking streaming (incremental) |
| `onToolStart` | `(tool) => void` | Tool execution started |
| `onToolProgress` | `(tool) => void` | Tool input streaming (advanced - shows partial JSON) |
| `onCliToolResult` | `(result) => void` | **Tool result from CLI** - CLI auto-executed and sent result |
| `onTurnComplete` | `(turn) => void` | Turn finished with usage/cost info |
| `onError` | `(error) => void` | Error occurred |
| **Recording** | | |
| `recordMessages` | `boolean` | Enable message recording (streams to disk automatically) |
| `recordingDir` | `string` | Directory for recordings (default: `./.claude-sessions`) |

### Methods

#### Core Methods

```typescript
session.start()                                    // Start CLI process
session.sendMessage(content: string)               // Send user message
session.stop()                                     // Stop session
```

#### Imperative API

```typescript
session.ask(content: string, timeout?: number)     // Send message + wait for completion
session.waitForTurn(turnNumber?: number)           // Wait for specific turn
```

#### Getters

```typescript
session.getSessionInfo()        // Session metadata (model, tools, etc.)
session.getTurnHistory()        // Complete turn history
session.getCurrentTurnNumber()  // Current turn number
```

#### Recording

```typescript
session.getRecording()                  // Get recording metadata + turns
session.getRecordingPath()              // Get path to session directory
session.getRecordingSummary()           // Get statistics
ClaudeSession.loadRecording(dirPath)    // Load recording from directory (static)
```

Note: Recordings are automatically streamed to disk. No need to call save manually.

## Event Types

### TextChunk
```typescript
{ turnNumber: number, text: string, fullText: string }
```

### ToolComplete (Critical)
```typescript
{ turnNumber: number, id: string, name: string, input: Record<string, unknown> }
```

### TurnComplete
```typescript
{
  turnNumber: number,
  success: boolean,
  durationMs: number,
  usage: { inputTokens, outputTokens, cacheReadTokens, costUSD }
}
```

See [ClaudeSession.ts](./ClaudeSession.ts) for all event types.

## Examples

### Multi-Turn Chat

```typescript
const session = new ClaudeSession({
  model: 'haiku',
  onText: (chunk) => process.stdout.write(chunk.text),
  onTurnComplete: (turn) => console.log(`\n[$${turn.usage.costUSD.toFixed(4)}]`)
})

session.start()
session.sendMessage('What is 2+2?')

// Later...
session.sendMessage('What about 3+3?')
```

### Tool Execution (CLI Auto-Execution)

**IMPORTANT**: Claude CLI automatically executes ALL tools. You do NOT need to manually execute tools.

```typescript
const session = new ClaudeSession({
  onToolStart: (tool) => {
    console.log(`🔧 ${tool.name} started`)
  },

  onToolComplete: (tool) => {
    console.log(`📝 ${tool.name} input ready`)
    // NOTE: CLI will auto-execute the tool - no action needed!
  },

  onCliToolResult: (result) => {
    // CLI sends back the tool result after auto-execution
    console.log(`✅ ${result.toolName} executed by CLI`)
    console.log(`   Result: ${typeof result.content === 'string' ? result.content.substring(0, 100) : 'structured'}`)
    console.log(`   Error: ${result.isError}`)
  }
})
```

**Tool Flow**:
1. Claude decides to use a tool (e.g., Read, Write, WebSearch)
2. `onToolStart` fires - tool execution begins
3. `onToolComplete` fires - tool input is complete
4. **CLI auto-executes the tool** (Read file, write file, search web, etc.)
5. `onCliToolResult` fires - you receive the tool result from CLI
6. Claude sees the result and continues

### Chat UI State Management

```typescript
class ChatUI {
  private session: ClaudeSession
  private turns = []

  constructor(updateUI: (turns) => void) {
    this.session = new ClaudeSession({
      onText: (chunk) => {
        this.turns[chunk.turnNumber - 1].text = chunk.fullText
        updateUI(this.turns)
      },
      onTurnComplete: (turn) => {
        this.turns[turn.turnNumber - 1].complete = true
        updateUI(this.turns)
      }
    })
  }

  ask(question: string) {
    this.turns.push({ text: '', complete: false })
    this.session.sendMessage(question)
  }
}
```

### Progress Indicators

```typescript
const session = new ClaudeSession({
  onToolStart: (tool) => showSpinner(tool.id),

  onToolProgress: (tool) => {
    const progress = Math.min(tool.partialInput.length / 1000, 0.9)
    updateProgressBar(tool.id, progress)
  },

  onCliToolResult: (result) => {
    updateProgressBar(result.toolUseId, 1.0)
    hideSpinner(result.toolUseId)
  }
})
```

## Best Practices

### 1. Always Handle Tool Execution Errors

```typescript
onToolComplete: async (tool) => {
  try {
    const result = await executeTool(tool)
    session.sendToolResults([{ toolUseId: tool.id, content: result, isError: false }])
  } catch (error) {
    session.sendToolResults([{ toolUseId: tool.id, content: error.message, isError: true }])
  }
}
```

### 2. Track Turn State When Sending Messages

```typescript
sendMessage(msg: string) {
  const turnNumber = session.getCurrentTurnNumber() + 1
  turnStates.set(turnNumber, { started: true, text: '', tools: [] })
  session.sendMessage(msg)
}
```

### 3. Clean Up Resources

```typescript
// On window close or component unmount
session.stop()
```

### 4. Use Imperative API for Simple Flows

```typescript
// Instead of managing callbacks:
const result = await session.ask('What is 2+2?')
console.log(result.usage.costUSD)
```

### 5. Enable Partial Messages for Rich UX

```typescript
const session = new ClaudeSession({
  includePartialMessages: true,  // Shows tool input streaming in real-time
  onToolProgress: (tool) => updateUI(tool.partialInput)
})
```

## Advanced

### Custom Tool Runner

```typescript
async function executeTool(tool: ToolComplete): Promise<string> {
  switch (tool.name) {
    case 'Read': return await fs.readFile(tool.input.file_path, 'utf-8')
    case 'Write':
      await fs.writeFile(tool.input.file_path, tool.input.content)
      return 'File written'
    case 'Bash': return (await exec(tool.input.command)).stdout
    default: throw new Error(`Unknown tool: ${tool.name}`)
  }
}
```

### Session Recording

```typescript
const session = new ClaudeSession({
  recordMessages: true,
  recordingDir: './recordings',

  onTurnComplete: () => {
    const summary = session.getRecordingSummary()
    console.log(`Total cost: $${summary.totalCost}`)
    console.log(`Recording saved to: ${session.getRecording()?.metadata.sessionId}`)
  }
})
```

Recordings are automatically streamed to disk as messages are sent/received.
Each session creates a directory with:
- `meta.json` - Session metadata and turn summaries
- `to_cli.jsonl` - Messages sent to CLI
- `from_cli.jsonl` - Messages received from CLI

### Complete Examples

See [examples.ts](./examples.ts) for 6 complete working examples:
1. Multi-Turn Chat
2. Tool Lifecycle Tracking
3. Chat UI Controller (React/Electron)
4. Interactive Tool Approval
5. Session Analytics
6. Progress Indicators

## Testing

```bash
npm test -- src/main/session/protocol
```

All 94 unit tests pass. See `__tests__/` for examples.

## Architecture

```
High-Level:  ClaudeSession (public API)
Mid-Level:   TurnManager, ProcessManager, EventCoordinator
Low-Level:   MessageDiscriminator, StreamAccumulator, SessionState
```

Most users only need `ClaudeSession`. See [index.ts](./index.ts) for advanced modules.
