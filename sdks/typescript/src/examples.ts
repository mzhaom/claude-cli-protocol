/**
 * Usage Examples for ClaudeSession API
 *
 * These examples demonstrate:
 * - Multi-turn conversations
 * - Tool lifecycle events (start, progress, complete)
 * - Real-time streaming
 */

import { ClaudeSession, SessionConfig, ToolStart, ToolProgress, ToolComplete, TurnComplete, TextChunk } from './ClaudeSession'

// ============================================================================
// Example 1: Simple Multi-Turn Chat
// ============================================================================

export function example1_MultiTurnChat() {
  const session = new ClaudeSession({
    model: 'haiku',

    onReady: (info) => {
      // NOTE: This is called AFTER the first message is sent
      console.log('Session ready:', info.sessionId)
      console.log('Model:', info.model)
      console.log('Tools:', info.tools.length)
    },

    onText: (chunk) => {
      // Real-time text streaming
      process.stdout.write(chunk.text)
    },

    onTurnComplete: (turn) => {
      console.log(`\n[Turn ${turn.turnNumber} complete - ${turn.usage.costUSD.toFixed(4)} USD]`)
    }
  })

  session.start()

  // IMPORTANT: The first sendMessage() triggers the init event (calls onReady)
  // Multiple turns in the same session
  session.sendMessage('What is 2 + 2?')

  // Wait for turn to complete, then ask follow-up
  setTimeout(() => {
    session.sendMessage('What about 3 + 3?')
  }, 3000)

  setTimeout(() => {
    session.sendMessage('Sum the previous two answers')
  }, 6000)
}

// ============================================================================
// Example 2: Tool Lifecycle Tracking
// ============================================================================

/**
 * @deprecated This example shows outdated manual tool execution.
 * See example7_CliAutoExecution() for the correct approach.
 * CLI now auto-executes all tools - you don't need to call sendToolResults().
 */
export function example2_ToolLifecycle() {
  const session = new ClaudeSession({
    model: 'sonnet',

    onToolStart: (tool) => {
      console.log(`\n🔧 Tool started: ${tool.name} (${tool.id})`)
      console.log(`   Turn ${tool.turnNumber}`)
      showToolSpinner(tool.id, 'starting')
    },

    onToolProgress: (tool) => {
      // Real-time input streaming
      console.log(`   Input streaming: ${tool.inputChunk}`)
      updateToolProgress(tool.id, tool.partialInput)
    },

    onToolComplete: (tool) => {
      console.log(`\n✅ Tool complete: ${tool.name}`)
      console.log(`   Input:`, JSON.stringify(tool.input, null, 2))

      // Now execute the tool
      executeTool(tool).then(result => {
        session.sendToolResults([{
          toolUseId: tool.id,
          content: result,
          isError: false
        }])
      })
    },

    onToolResult: (result) => {
      console.log(`\n📤 Tool result sent: ${result.toolUseId}`)
      console.log(`   Turn ${result.turnNumber}`)
    }
  })

  session.start()
  session.sendMessage('List all .ts files in the current directory')
}

// ============================================================================
// Example 3: Chat UI with Complete State Management
// ============================================================================

export class ChatUIController {
  private session: ClaudeSession
  private uiState: UIState = {
    turns: [],
    currentTurn: null,
    activeTools: new Map()
  }

  constructor(private updateUI: (state: UIState) => void) {
    this.session = new ClaudeSession({
      model: 'haiku',

      onReady: (info) => {
        this.updateUI({
          ...this.uiState,
          sessionInfo: info,
          status: 'ready'
        })
      },

      onText: (chunk) => {
        if (this.uiState.currentTurn) {
          this.uiState.currentTurn.assistantText = chunk.fullText
          this.updateUI(this.uiState)
        }
      },

      onThinking: (chunk) => {
        if (this.uiState.currentTurn) {
          this.uiState.currentTurn.assistantThinking = chunk.fullThinking
          this.updateUI(this.uiState)
        }
      },

      onToolStart: (tool) => {
        const toolState: ToolState = {
          id: tool.id,
          name: tool.name,
          status: 'starting',
          startTime: tool.timestamp
        }

        this.uiState.activeTools.set(tool.id, toolState)
        this.uiState.currentTurn?.tools.push(toolState)
        this.updateUI(this.uiState)
      },

      onToolProgress: (tool) => {
        const toolState = this.uiState.activeTools.get(tool.id)
        if (toolState) {
          toolState.status = 'streaming'
          toolState.partialInput = tool.partialInput
          this.updateUI(this.uiState)
        }
      },

      onToolComplete: (tool) => {
        const toolState = this.uiState.activeTools.get(tool.id)
        if (toolState) {
          toolState.status = 'executing'
          toolState.input = tool.input
          this.updateUI(this.uiState)

          // Execute tool
          this.executeTool(tool)
        }
      },

      onToolResult: (result) => {
        const toolState = this.uiState.activeTools.get(result.toolUseId)
        if (toolState) {
          toolState.status = 'complete'
          toolState.result = result.content
          this.uiState.activeTools.delete(result.toolUseId)
          this.updateUI(this.uiState)
        }
      },

      onTurnComplete: (turn) => {
        if (this.uiState.currentTurn) {
          this.uiState.currentTurn.status = 'complete'
          this.uiState.currentTurn.usage = turn.usage
          this.uiState.currentTurn = null
          this.updateUI(this.uiState)
        }
      },

      onError: (error) => {
        console.error('Session error:', error)
        this.updateUI({
          ...this.uiState,
          error: error.error.message
        })
      }
    })
  }

  start() {
    this.session.start()
  }

  sendMessage(message: string) {
    // Initialize turn state when sending message
    const turnNumber = this.session.getCurrentTurnNumber() + 1
    this.uiState.currentTurn = {
      turnNumber,
      userMessage: message,
      assistantText: '',
      assistantThinking: '',
      tools: [],
      status: 'streaming'
    }
    this.uiState.turns.push(this.uiState.currentTurn)
    this.updateUI(this.uiState)

    this.session.sendMessage(message)
  }

  private async executeTool(tool: ToolComplete) {
    try {
      const result = await executeToolImpl(tool.name, tool.input)
      this.session.sendToolResults([{
        toolUseId: tool.id,
        content: result,
        isError: false
      }])
    } catch (error) {
      this.session.sendToolResults([{
        toolUseId: tool.id,
        content: error instanceof Error ? error.message : String(error),
        isError: true
      }])
    }
  }

  stop() {
    this.session.stop()
  }
}

interface UIState {
  sessionInfo?: any
  status?: string
  turns: TurnState[]
  currentTurn: TurnState | null
  activeTools: Map<string, ToolState>
  error?: string
}

interface TurnState {
  turnNumber: number
  userMessage: string | any
  assistantText: string
  assistantThinking: string
  tools: ToolState[]
  status: 'streaming' | 'complete'
  usage?: any
}

interface ToolState {
  id: string
  name: string
  status: 'starting' | 'streaming' | 'executing' | 'complete'
  startTime: Date
  partialInput?: string
  input?: Record<string, unknown>
  result?: string
}

// ============================================================================
// Example 4: Interactive Tool Approval with Progress
// ============================================================================

export class InteractiveToolSession {
  private session: ClaudeSession
  private pendingApprovals = new Map<string, ToolComplete>()

  constructor() {
    this.session = new ClaudeSession({
      model: 'sonnet',

      onText: (chunk) => {
        process.stdout.write(chunk.text)
      },

      onToolStart: (tool) => {
        console.log(`\n🔧 ${tool.name} starting...`)
      },

      onToolProgress: (tool) => {
        // Show partial input as it streams
        console.log(`   ${tool.inputChunk}`)
      },

      onToolComplete: async (tool) => {
        console.log(`\n📋 ${tool.name} ready to execute`)
        console.log('Input:', JSON.stringify(tool.input, null, 2))

        // Ask user for approval
        const approved = await this.askApproval(tool)

        if (approved) {
          const result = await executeTool(tool)
          this.session.sendToolResults([{
            toolUseId: tool.id,
            content: result,
            isError: false
          }])
        } else {
          this.session.sendToolResults([{
            toolUseId: tool.id,
            content: 'User denied permission',
            isError: true
          }])
        }
      },

      onTurnComplete: (turn) => {
        console.log(`\n✓ Turn ${turn.turnNumber} complete`)
        console.log(`  Cost: $${turn.usage.costUSD.toFixed(4)}`)
        console.log(`  Tokens: ${turn.usage.inputTokens} → ${turn.usage.outputTokens}`)
      }
    })
  }

  start() {
    this.session.start()
  }

  ask(question: string) {
    this.session.sendMessage(question)
  }

  private async askApproval(tool: ToolComplete): Promise<boolean> {
    // In real implementation, show UI dialog
    return new Promise((resolve) => {
      console.log('\nApprove this tool? (y/n)')
      // Simulate user input
      setTimeout(() => resolve(true), 1000)
    })
  }
}

// ============================================================================
// Example 5: Conversation History and Analytics
// ============================================================================

export class ConversationAnalytics {
  private session: ClaudeSession
  private analytics = {
    totalTurns: 0,
    totalTokens: 0,
    totalCost: 0,
    toolsByName: new Map<string, number>(),
    averageResponseTime: 0,
    responseTimes: [] as number[]
  }

  constructor() {
    this.session = new ClaudeSession({
      model: 'haiku',

      onText: (chunk) => {
        process.stdout.write(chunk.text)
      },

      onToolComplete: (tool) => {
        // Track tool usage
        const count = this.analytics.toolsByName.get(tool.name) || 0
        this.analytics.toolsByName.set(tool.name, count + 1)

        // Auto-execute (no approval needed)
        executeTool(tool).then(result => {
          this.session.sendToolResults([{
            toolUseId: tool.id,
            content: result,
            isError: false
          }])
        })
      },

      onTurnComplete: (turn) => {
        // Update analytics
        this.analytics.totalTurns++
        this.analytics.totalTokens += turn.usage.inputTokens + turn.usage.outputTokens
        this.analytics.totalCost += turn.usage.costUSD
        this.analytics.responseTimes.push(turn.durationMs)
        this.analytics.averageResponseTime =
          this.analytics.responseTimes.reduce((a, b) => a + b, 0) / this.analytics.responseTimes.length

        console.log(`\n[${turn.durationMs}ms, $${turn.usage.costUSD.toFixed(4)}]`)
      }
    })
  }

  start() {
    this.session.start()
  }

  ask(question: string) {
    this.session.sendMessage(question)
  }

  getAnalytics() {
    return {
      ...this.analytics,
      toolsUsed: Array.from(this.analytics.toolsByName.entries())
        .map(([name, count]) => ({ name, count }))
        .sort((a, b) => b.count - a.count)
    }
  }

  printReport() {
    console.log('\n=== Session Analytics ===')
    console.log(`Turns: ${this.analytics.totalTurns}`)
    console.log(`Tokens: ${this.analytics.totalTokens}`)
    console.log(`Cost: $${this.analytics.totalCost.toFixed(4)}`)
    console.log(`Avg Response Time: ${this.analytics.averageResponseTime.toFixed(0)}ms`)
    console.log('\nTools Used:')
    this.getAnalytics().toolsUsed.forEach(({ name, count }) => {
      console.log(`  ${name}: ${count}`)
    })
  }
}

// ============================================================================
// Example 6: Real-time Progress Indicators
// ============================================================================

/**
 * @deprecated This example shows outdated manual tool execution.
 * See example7_CliAutoExecution() for the correct approach.
 */
export function example6_ProgressIndicators() {
  let currentToolId: string | null = null

  const session = new ClaudeSession({
    model: 'sonnet',

    onText: (chunk) => {
      // Show text with typing indicator
      updateTypingIndicator(chunk.text)
    },

    onThinking: (chunk) => {
      // Show thinking process
      updateThinkingBubble(chunk.thinking)
    },

    onToolStart: (tool) => {
      currentToolId = tool.id
      showToolProgressBar(tool.id, tool.name, 0)
    },

    onToolProgress: (tool) => {
      // Estimate progress based on input size
      const progress = Math.min(tool.partialInput.length / 1000, 0.9)
      updateToolProgressBar(tool.id, progress)
    },

    onToolComplete: (tool) => {
      updateToolProgressBar(tool.id, 1.0)
      currentToolId = null

      // Execute tool
      executeTool(tool).then(result => {
        session.sendToolResults([{
          toolUseId: tool.id,
          content: result,
          isError: false
        }])
      })
    },

    onTurnComplete: (turn) => {
      showTurnSummary({
        duration: turn.durationMs,
        tokens: turn.usage.inputTokens + turn.usage.outputTokens,
        cost: turn.usage.costUSD
      })
    }
  })

  session.start()
  session.sendMessage('Analyze all TypeScript files and create a summary report')
}

// ============================================================================
// Mock Helper Functions
// ============================================================================

function showToolSpinner(id: string, status: string) {
  console.log(`   [${status}]`)
}

function updateToolProgress(id: string, partialInput: string) {
  // Update UI
}

async function executeTool(tool: ToolComplete): Promise<string> {
  // Mock tool execution
  console.log(`   Executing ${tool.name}...`)
  await new Promise(resolve => setTimeout(resolve, 500))
  return 'Tool result here'
}

async function executeToolImpl(name: string, input: Record<string, unknown>): Promise<string> {
  // Mock implementation
  return `Result from ${name}`
}

function updateTypingIndicator(text: string) {
  process.stdout.write(text)
}

function updateThinkingBubble(thinking: string) {
  console.log(`💭 ${thinking}`)
}

function showToolProgressBar(id: string, nameOrProgress: string | number, progress?: number) {
  if (typeof nameOrProgress === 'string') {
    console.log(`\n[${nameOrProgress}] ${'█'.repeat(0)}${'░'.repeat(20)}`)
  } else {
    const filled = Math.floor(nameOrProgress * 20)
    console.log(`[${'█'.repeat(filled)}${'░'.repeat(20 - filled)}] ${(nameOrProgress * 100).toFixed(0)}%`)
  }
}

function updateToolProgressBar(id: string, progress: number) {
  showToolProgressBar(id, progress)
}

function showTurnSummary(summary: { duration: number, tokens: number, cost: number }) {
  console.log(`\n✓ ${summary.duration}ms | ${summary.tokens} tokens | $${summary.cost.toFixed(4)}`)
}

// ============================================================================
// Example 7: Understanding CLI Auto-Execution
// ============================================================================

export function example7_CliAutoExecution() {
  console.log('\\n=== Example 7: CLI Auto-Execution Model ===\\n')

  const session = new ClaudeSession({
    model: 'haiku',

    onToolStart: (tool) => {
      console.log(`🔧 Tool started: ${tool.name} (${tool.id})`)
    },

    onToolComplete: (tool) => {
      console.log(`📝 Tool input complete: ${tool.name}`)
      console.log(`   Input:`, JSON.stringify(tool.input).substring(0, 100))
      // NOTE: You do NOT need to execute the tool here!
      // The CLI will execute it automatically and send the result back
    },

    onCliToolResult: (result) => {
      // This is where CLI sends back the tool result after auto-execution
      console.log(`✅ CLI executed: ${result.toolName}`)
      if (typeof result.content === 'string') {
        console.log(`   Result: ${result.content.substring(0, 100)}...`)
      }
      console.log(`   Error: ${result.isError}`)
    },

    onText: (chunk) => {
      process.stdout.write(chunk.text)
    },

    onTurnComplete: (turn) => {
      console.log(`\\n[Turn ${turn.turnNumber} complete - $${turn.usage.costUSD.toFixed(4)}]`)
    }
  })

  session.start()

  // Example 1: WebSearch (CLI-executed)
  console.log('Example 1: Triggering WebSearch...')
  session.sendMessage('Search for latest news about AI')

  setTimeout(() => {
    // Example 2: Write tool (CLI-executed)
    console.log('\\nExample 2: Triggering Write tool...')
    session.sendMessage('Create a file called example.txt with "Hello World"')
  }, 10000)

  setTimeout(() => {
    // Example 3: Read tool (CLI-executed)
    console.log('\\nExample 3: Triggering Read tool...')
    session.sendMessage('Read the contents of example.txt')
  }, 20000)

  setTimeout(() => {
    session.stop()
    console.log('\\n=== Key Points ===')
    console.log('1. ALL tools are built-in to Claude CLI')
    console.log('2. CLI auto-executes ALL tools (WebSearch, Write, Read, Bash, etc.)')
    console.log('3. Use onCliToolResult to receive tool execution results')
    console.log('4. You do NOT need to manually execute tools or call sendToolResults()')
    console.log('5. The tool lifecycle is: onToolStart → onToolComplete → onCliToolResult')
  }, 30000)
}
