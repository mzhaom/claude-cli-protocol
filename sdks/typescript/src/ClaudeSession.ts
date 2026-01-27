/**
 * High-Level Session API for Claude Code CLI
 *
 * Handles multi-turn conversations and tool lifecycle events.
 *
 * Usage:
 * ```typescript
 * const session = new ClaudeSession({
 *   model: 'haiku',
 *   onReady: (info) => {
 *     // NOTE: onReady is called AFTER first message is sent
 *     console.log('Session initialized:', info.sessionId)
 *   },
 *   onText: (text) => updateUI(text),
 *   onToolStart: (tool) => showToolStarted(tool),
 *   onToolProgress: (tool) => showToolProgress(tool),
 *   onToolComplete: (tool) => showToolDone(tool),
 *   onTurnComplete: (turn) => showTurnComplete(turn),
 * })
 *
 * session.start()
 * // First message triggers the init event (which calls onReady)
 * session.sendMessage('Hello!')
 * // ... multiple turns possible
 * session.sendMessage('What about this?')
 * ```
 */

import { spawn, ChildProcess } from 'child_process'
import { createInterface, Interface } from 'readline'
import { EventEmitter } from 'events'
import { MessageDiscriminator } from './MessageDiscriminator'
import { StreamAccumulator } from './StreamAccumulator'
import { SessionRecorder, SessionRecording } from './SessionRecorder'
import { PermissionHandler, PermissionDecision, PermissionRequestData } from './PermissionHandler'
import {
  SystemMessage,
  AssistantMessage,
  UserMessage,
  ResultMessage,
  StreamEvent,
  ToolUseBlock,
  TextBlock,
  ThinkingBlock,
  ContentBlock,
  ToolResultBlock,
  ProtocolMessage,
  ControlRequest,
  ControlResponse
} from './types'

// ============================================================================
// Public API Types
// ============================================================================

export interface SessionConfig {
  /** Model to use: 'haiku' (fast, cheap), 'sonnet' (balanced), 'opus' (most capable) */
  model?: string

  /** Working directory for file operations (defaults to process.cwd()) */
  cwd?: string

  /**
   * Permission mode for tool execution:
   * - 'default': Prompt user for each tool (requires onPermissionRequest)
   * - 'acceptEdits': Auto-approve file edits, prompt for other tools
   * - 'plan': Present plan before execution, then prompt
   * - 'bypassPermissions': Auto-approve all tools (use with caution)
   */
  permissionMode?: 'default' | 'acceptEdits' | 'plan' | 'bypassPermissions'

  /**
   * Disable loading of plugins (faster startup, useful for tests).
   * When true, adds --no-plugins flag to the CLI.
   */
  disablePlugins?: boolean

  // Recording
  /** Enable message recording for debugging, session replay, or trace analysis */
  recordMessages?: boolean

  /** Directory to save recordings (defaults to .claude-sessions) */
  recordingDir?: string

  // Session lifecycle
  /**
   * Called when session is initialized.
   *
   * IMPORTANT: Due to Claude CLI protocol design, this is called AFTER
   * the first user message is sent, not immediately after start().
   *
   * Use this to get sessionId, available tools, and session metadata.
   */
  onReady?: (info: SessionInfo) => void

  /**
   * Called when a turn completes (assistant finished responding).
   *
   * CRITICAL: Contains usage metrics (tokens, cost) and turn result.
   * Use this to track costs, update UI state, or log metrics.
   */
  onTurnComplete?: (turn: TurnComplete) => void

  // Message streaming
  /**
   * Called for each text chunk during streaming (real-time, incremental).
   *
   * CORE FEATURE: Use this for live chat UI updates.
   * Provides both the new chunk and accumulated full text.
   */
  onText?: (chunk: TextChunk) => void

  /**
   * Called for thinking chunks (extended thinking feature).
   * Shows Claude's reasoning process before generating response.
   */
  onThinking?: (chunk: ThinkingChunk) => void

  // Tool lifecycle
  /**
   * Called when a tool call starts.
   * Use to show "Tool X is running..." in UI.
   */
  onToolStart?: (tool: ToolStart) => void

  /**
   * Called as tool input streams in.
   *
   * ADVANCED: Rarely needed - shows partial JSON input parameters.
   * Most users should use onToolStart instead.
   */
  onToolProgress?: (tool: ToolProgress) => void

  /**
   * Called when CLI sends back tool results (CLI auto-executed the tool).
   *
   * IMPORTANT: Claude CLI automatically executes ALL tools (Read, Write, Bash, WebSearch, etc.).
   * This callback fires when the CLI sends the tool result back to you.
   * You do NOT need to execute tools manually.
   */
  onCliToolResult?: (result: CliToolResult) => void

  // Permission handling
  /**
   * Called when tool execution requires user permission (in 'default' mode).
   *
   * REQUIRED when permissionMode: 'default'.
   * Return a decision: allow, deny, modify input, or always allow.
   * Can be async (return Promise) for UI dialogs.
   */
  onPermissionRequest?: (request: PermissionRequestData) => Promise<PermissionDecision> | PermissionDecision

  // Errors
  /**
   * Called on errors (parsing failures, CLI crashes, turn errors).
   * Use for error handling, logging, or user notifications.
   */
  onError?: (error: SessionError) => void
}

/** Information about the session */
export interface SessionInfo {
  sessionId: string
  model: string
  cwd: string
  tools: string[]
  permissionMode: string
}

/** Text chunk received during streaming */
export interface TextChunk {
  turnNumber: number
  text: string          // New text received
  fullText: string      // Complete text so far
}

/** Thinking chunk received during streaming */
export interface ThinkingChunk {
  turnNumber: number
  thinking: string      // New thinking received
  fullThinking: string  // Complete thinking so far
}

/** Tool call started */
export interface ToolStart {
  turnNumber: number
  id: string
  name: string
  timestamp: Date
}

/** Tool input streaming in */
export interface ToolProgress {
  turnNumber: number
  id: string
  name: string
  partialInput: string  // JSON string (incomplete)
  inputChunk: string    // New chunk received
}

/** Tool result received FROM Claude CLI (CLI auto-executed the tool) */
export interface CliToolResult {
  turnNumber: number
  toolUseId: string
  toolName: string
  content: string | ContentBlock[]
  isError: boolean
}

/** Turn completed */
export interface TurnComplete {
  turnNumber: number
  success: boolean
  durationMs: number
  usage: {
    inputTokens: number
    outputTokens: number
    cacheReadTokens: number
    costUSD: number
  }
  error?: string
}

/** Session error */
export interface SessionError {
  turnNumber?: number
  error: Error
  context: string
}

// ============================================================================
// Turn State (internal)
// ============================================================================

interface TurnState {
  turnNumber: number
  userMessage: string | ContentBlock[]
  startTime: Date

  // Accumulated content
  fullText: string
  fullThinking: string

  // Active tools
  tools: Map<string, {
    id: string
    name: string
    partialInput: string
    input?: Record<string, unknown>
    startTime: Date
  }>

  // Tool input accumulation by block index (for streaming)
  toolInputByIndex?: Map<number, string>
}

// ============================================================================
// ClaudeSession - Main API
// ============================================================================

export class ClaudeSession extends EventEmitter {
  private config: SessionConfig
  private process?: ChildProcess
  private readline?: Interface
  private accumulator: StreamAccumulator
  private recorder?: SessionRecorder
  private sessionId?: string
  private sessionInfo?: SessionInfo
  private isStarted = false
  private isStopping = false

  // Multi-turn state
  private currentTurnNumber = 0
  private currentTurn?: TurnState
  private turns: TurnState[] = []

  // Promise-based turn completion tracking
  private turnCompletionResolvers = new Map<number, {
    resolve: (result: TurnComplete) => void
    reject: (error: Error) => void
    timeout?: NodeJS.Timeout
  }>()

  // Promise-based session ready tracking
  private readyResolver?: {
    resolve: (info: SessionInfo) => void
    reject: (error: Error) => void
    timeout?: NodeJS.Timeout
  }

  constructor(config: SessionConfig = {}) {
    super()
    this.config = {
      model: 'haiku',
      recordMessages: false,
      ...config
    }

    this.accumulator = new StreamAccumulator()
    this.setupAccumulatorEvents()

    // Initialize recorder if enabled
    if (this.config.recordMessages) {
      // Set default recordingDir if not provided
      const recordingDir = this.config.recordingDir ||
        `${this.config.cwd || process.cwd()}/.claude-sessions`

      this.recorder = new SessionRecorder({
        saveDir: recordingDir
      })
    }
  }

  /**
   * Start the Claude CLI session
   */
  start(): void {
    if (this.isStarted) {
      throw new Error('Session already started')
    }

    const args = [
      '--print',
      '--input-format', 'stream-json',
      '--output-format', 'stream-json',
      '--verbose',  // Required for stream-json
      '--model', this.config.model!
    ]

    if (this.config.permissionMode) {
      args.push('--permission-mode', this.config.permissionMode)
    }

    // Disable plugins for faster startup (useful for tests)
    if (this.config.disablePlugins) {
      args.push('--plugin-dir', '/dev/null')
    }

    // Always include partial messages for tool progress tracking
    args.push('--include-partial-messages')

    this.process = spawn('claude', args, {
      cwd: this.config.cwd || process.cwd(),
      stdio: ['pipe', 'pipe', 'pipe']
    })

    this.readline = createInterface({
      input: this.process.stdout!,
      crlfDelay: Infinity
    })

    this.readline.on('line', (line) => this.handleLine(line))
    this.process.stderr!.on('data', (data) => this.handleStderr(data))
    this.process.on('exit', (code) => this.handleExit(code))

    this.isStarted = true
  }

  /**
   * Send a message to Claude (starts a new turn)
   */
  sendMessage(content: string | ContentBlock[]): void {
    if (!this.process) {
      throw new Error('Session not started')
    }

    // Start new turn
    this.currentTurnNumber++
    this.currentTurn = {
      turnNumber: this.currentTurnNumber,
      userMessage: content,
      startTime: new Date(),
      fullText: '',
      fullThinking: '',
      tools: new Map()
    }
    this.turns.push(this.currentTurn)

    // Send message
    const message = {
      type: 'user' as const,
      message: {
        role: 'user' as const,
        content
      }
    }

    // Record sent message
    if (this.recorder) {
      this.recorder.recordSent(message, this.currentTurnNumber)
      this.recorder.startTurn(this.currentTurnNumber, content)
    }

    this.process.stdin!.write(JSON.stringify(message) + '\n')
  }


  /**
   * Set the permission mode
   * - If session not started: Updates config for CLI spawn
   * - If session started: Sends control request to change mode dynamically
   */
  setPermissionMode(mode: 'default' | 'acceptEdits' | 'plan' | 'bypassPermissions'): void {
    if (!this.process) {
      // Session not started - update config so CLI spawns with correct flag
      this.config.permissionMode = mode
      return
    }

    // Session already started - send control request to change mode dynamically
    const controlRequest = {
      type: 'control_request' as const,
      request_id: `req_${Date.now()}_${Math.random().toString(36).substring(2, 11)}`,
      request: {
        subtype: 'set_permission_mode' as const,
        mode
      }
    }

    // Record control request
    if (this.recorder) {
      this.recorder.recordSent(controlRequest, this.currentTurnNumber)
    }

    this.process.stdin!.write(JSON.stringify(controlRequest) + '\n')
  }

  /**
   * Interrupt the current conversation/turn
   *
   * Sends an interrupt control request to stop the current execution.
   * The session remains active and can continue with new messages.
   */
  interrupt(): void {
    if (!this.process) {
      return
    }

    const controlRequest = {
      type: 'control_request' as const,
      request_id: `req_${Date.now()}_${Math.random().toString(36).substring(2, 11)}`,
      request: {
        subtype: 'interrupt' as const
      }
    }

    // Record control request
    if (this.recorder) {
      this.recorder.recordSent(controlRequest, this.currentTurnNumber)
    }

    this.process.stdin!.write(JSON.stringify(controlRequest) + '\n')
  }

  /**
   * Stop the session gracefully
   */
  stop(): void {
    if (!this.process) {
      return
    }

    this.isStopping = true

    // Close readline first
    if (this.readline) {
      this.readline.close()
      this.readline = undefined
    }

    // Close stdin to signal graceful shutdown
    if (this.process.stdin) {
      this.process.stdin.end()
    }

    // Graceful shutdown sequence: stdin close → SIGTERM → SIGKILL
    // Give process 500ms to exit from stdin close
    const sigtermTimeout = setTimeout(() => {
      if (this.process && !this.process.killed) {
        // Send SIGTERM for graceful shutdown
        this.process.kill('SIGTERM')

        // If SIGTERM doesn't work, force kill after another 500ms
        const sigkillTimeout = setTimeout(() => {
          if (this.process && !this.process.killed) {
            this.process.kill('SIGKILL')
          }
        }, 500)

        // Clean up SIGKILL timeout if process exits
        this.process.once('exit', () => {
          clearTimeout(sigkillTimeout)
        })
      }
    }, 500)

    // Clean up timeouts if process exits naturally
    this.process.once('exit', () => {
      clearTimeout(sigtermTimeout)
      this.process = undefined
      this.isStarted = false
    })
  }

  /**
   * Get current session info
   */
  getSessionInfo(): SessionInfo | undefined {
    return this.sessionInfo
  }

  /**
   * Get turn history
   */
  getTurnHistory(): ReadonlyArray<TurnState> {
    return this.turns
  }

  /**
   * Get current turn number
   */
  getCurrentTurnNumber(): number {
    return this.currentTurnNumber
  }

  /**
   * Get message recording (if enabled)
   */
  getRecording(): SessionRecording | undefined {
    return this.recorder?.getRecording()
  }

  /**
   * Get path to the recording directory (if recording is enabled)
   */
  getRecordingPath(): string | undefined {
    return this.recorder?.getRecordingPath()
  }

  /**
   * Get recording summary statistics
   */
  getRecordingSummary() {
    return this.recorder?.getSummary()
  }

  /**
   * Load a previous recording (static method)
   * @param dirPath - Path to session directory containing meta.json, to_cli.jsonl, from_cli.jsonl
   */
  static async loadRecording(dirPath: string): Promise<SessionRecording> {
    return SessionRecorder.load(dirPath)
  }

  /**
   * Wait for a specific turn to complete (imperative API)
   *
   * @param turnNumber - Turn number to wait for (defaults to next turn)
   * @param timeout - Timeout in milliseconds (default: 30000)
   * @returns Promise that resolves with turn result
   *
   * @example
   * ```typescript
   * session.start()
   * session.sendMessage('Hello!')
   * const result = await session.waitForTurn()  // Wait for turn 1
   * console.log('Cost:', result.usage.costUSD)
   * ```
   */
  waitForTurn(turnNumber?: number, timeout = 30000): Promise<TurnComplete> {
    const targetTurn = turnNumber ?? this.currentTurnNumber

    return new Promise((resolve, reject) => {
      // Set up timeout
      const timeoutId = setTimeout(() => {
        this.turnCompletionResolvers.delete(targetTurn)
        reject(new Error(`Timeout waiting for turn ${targetTurn} after ${timeout}ms`))
      }, timeout)

      // Store resolver
      this.turnCompletionResolvers.set(targetTurn, {
        resolve,
        reject,
        timeout: timeoutId
      })
    })
  }

  /**
   * Send message and wait for completion (imperative API)
   *
   * @param content - Message content
   * @param timeout - Timeout in milliseconds (default: 30000)
   * @returns Promise that resolves with turn result
   *
   * @example
   * ```typescript
   * const result = await session.ask('What is 2+2?')
   * console.log('Answer received, cost:', result.usage.costUSD)
   * ```
   */
  async ask(content: string | ContentBlock[], timeout = 30000): Promise<TurnComplete> {
    this.sendMessage(content)
    return this.waitForTurn(this.currentTurnNumber, timeout)
  }

  // ============================================================================
  // Private: Message Handling
  // ============================================================================

  private handleLine(line: string): void {
    try {
      const msg = JSON.parse(line)

      // Record received message
      if (this.recorder) {
        this.recorder.recordReceived(msg as ProtocolMessage, this.currentTurnNumber)
      }

      const type = MessageDiscriminator.discriminate(msg)

      switch (type) {
        case 'system':
          this.handleSystem(msg as SystemMessage)
          break
        case 'stream_event':
          this.accumulator.handleEvent(msg as StreamEvent)
          break
        case 'assistant':
          this.handleAssistant(msg as AssistantMessage)
          break
        case 'user':
          this.handleUser(msg as UserMessage)
          break
        case 'result':
          this.handleResult(msg as ResultMessage)
          break
        case 'control_request':
          this.handleControlRequest(msg as ControlRequest)
          break
      }
    } catch (error) {
      this.emitError(new Error(`Failed to parse message: ${error}`), 'message_parsing')
    }
  }

  private async handleSystem(msg: SystemMessage): Promise<void> {
    if (msg.subtype === 'init') {
      this.sessionId = msg.session_id
      this.sessionInfo = {
        sessionId: msg.session_id,
        model: msg.model || this.config.model || 'unknown',
        cwd: msg.cwd || this.config.cwd || process.cwd(),
        tools: msg.tools || [],
        permissionMode: msg.permissionMode || 'default'
      }

      // Initialize recorder with session info
      if (this.recorder) {
        await this.recorder.initialize({
          sessionId: msg.session_id,
          model: msg.model || this.config.model || 'unknown',
          cwd: msg.cwd || this.config.cwd || process.cwd(),
          tools: msg.tools || [],
          claudeCodeVersion: msg.claude_code_version,
          permissionMode: msg.permissionMode || 'default'
        })
      }

      if (this.config.onReady) {
        this.config.onReady(this.sessionInfo)
      }
    }
  }

  private handleAssistant(msg: AssistantMessage): void {
    // This is the final complete message
    // Extract text from the message and emit it
    const textBlocks = msg.message.content
      .filter((b): b is TextBlock => b.type === 'text')

    if (textBlocks.length > 0 && this.currentTurn) {
      const fullText = textBlocks.map(b => b.text).join('')

      // Only emit if we have new text (not already emitted via streaming)
      if (fullText !== this.currentTurn.fullText) {
        const newText = fullText.substring(this.currentTurn.fullText.length)
        this.currentTurn.fullText = fullText

        // Record text update
        if (this.recorder) {
          this.recorder.updateTurnText(this.currentTurnNumber, fullText)
        }

        if (this.config.onText && newText) {
          this.config.onText({
            turnNumber: this.currentTurnNumber,
            text: newText,
            fullText
          })
        }
      }
    }

    // Extract and emit final tool uses (if not already emitted during streaming)
    const tools = msg.message.content
      .filter((b): b is ToolUseBlock => b.type === 'tool_use')

    tools.forEach(tool => {
      // Check if we already handled this tool during streaming
      if (this.currentTurn?.tools.has(tool.id)) {
        const toolState = this.currentTurn.tools.get(tool.id)!

        // Store the parsed input
        if (!toolState.input) {
          toolState.input = tool.input
        }
      } else {
        // Tool not seen during streaming (rare - happens when tool execution is instant)
        // Add to tools map and emit all events now
        const toolState = {
          id: tool.id,
          name: tool.name,
          partialInput: '',
          input: tool.input,
          startTime: new Date()
        }
        if (this.currentTurn) {
          this.currentTurn.tools.set(tool.id, toolState)
        }

        if (this.config.onToolStart) {
          this.config.onToolStart({
            turnNumber: this.currentTurnNumber,
            id: tool.id,
            name: tool.name,
            timestamp: toolState.startTime
          })
        }

        // Also emit tool complete since we have the full input
        if (this.config.onToolComplete) {
          this.config.onToolComplete({
            turnNumber: this.currentTurnNumber,
            id: tool.id,
            name: tool.name,
            input: tool.input,
            timestamp: new Date()
          })
        }
      }
    })
  }

  private handleUser(msg: UserMessage): void {
    // Process tool_result blocks FROM CLI (CLI auto-executed tools)
    // These come back as "user" messages from the CLI
    const toolResults = msg.message.content
      .filter((b): b is ToolResultBlock => b.type === 'tool_result')

    toolResults.forEach(result => {
      if (!this.currentTurn) return

      // Find the corresponding tool in current turn by searching all turns
      // (tool_result might come in a different turn than tool_use in some cases)
      let toolName = 'unknown'

      // First try current turn
      const toolState = this.currentTurn.tools.get(result.tool_use_id)
      if (toolState) {
        toolName = toolState.name
      } else {
        // Search all turns for this tool_use_id
        for (const turn of this.turns) {
          const tool = turn.tools.get(result.tool_use_id)
          if (tool) {
            toolName = tool.name
            break
          }
        }
      }

      if (this.config.onCliToolResult) {
        this.config.onCliToolResult({
          turnNumber: this.currentTurnNumber,
          toolUseId: result.tool_use_id,
          toolName,
          content: result.content,
          isError: result.is_error || false
        })
      }
    })
  }

  private handleResult(msg: ResultMessage): void {
    if (!this.currentTurn) {
      return
    }

    const duration = new Date().getTime() - this.currentTurn.startTime.getTime()

    const result: TurnComplete = {
      turnNumber: this.currentTurnNumber,
      success: !msg.is_error,
      durationMs: msg.duration_ms || duration,
      usage: {
        inputTokens: msg.usage.input_tokens,
        outputTokens: msg.usage.output_tokens,
        cacheReadTokens: msg.usage.cache_read_input_tokens,
        costUSD: msg.total_cost_usd
      },
      error: msg.is_error ? msg.result : undefined
    }

    // Record turn completion
    if (this.recorder) {
      this.recorder.completeTurn(this.currentTurnNumber, {
        duration: result.durationMs,
        cost: result.usage.costUSD,
        tokens: {
          input: result.usage.inputTokens,
          output: result.usage.outputTokens
        }
      })
    }

    if (this.config.onTurnComplete) {
      this.config.onTurnComplete(result)
    }

    // Resolve any waiting promises for this turn
    const resolver = this.turnCompletionResolvers.get(this.currentTurnNumber)
    if (resolver) {
      if (resolver.timeout) {
        clearTimeout(resolver.timeout)
      }
      this.turnCompletionResolvers.delete(this.currentTurnNumber)

      if (msg.is_error) {
        resolver.reject(new Error(msg.result))
      } else {
        resolver.resolve(result)
      }
    }

    if (msg.is_error && this.config.onError) {
      this.emitError(new Error(msg.result), 'turn_error')
    }
  }

  private async handleControlRequest(msg: ControlRequest): Promise<void> {
    // Extract permission request data
    const permissionData = PermissionHandler.extractPermissionRequest(msg)

    if (!permissionData) {
      // Not a permission request, ignore for now
      return
    }

    if (!this.config.onPermissionRequest) {
      // No handler configured, auto-deny
      const response = PermissionHandler.buildResponse(msg.request_id, {
        action: 'deny',
        message: 'No permission handler configured'
      })
      this.sendControlResponse(response)
      return
    }

    try {
      // Call user's permission handler
      const decision = await this.config.onPermissionRequest(permissionData)

      // Build and send response
      const response = PermissionHandler.buildResponse(msg.request_id, decision)
      this.sendControlResponse(response)
    } catch (error) {
      // Error in permission handler, deny
      this.emitError(new Error(`Permission handler error: ${error}`), 'permission_handling')
      const response = PermissionHandler.buildResponse(msg.request_id, {
        action: 'deny',
        message: 'Permission handler error'
      })
      this.sendControlResponse(response)
    }
  }

  /**
   * Send a control response back to CLI
   */
  private sendControlResponse(response: ControlResponse): void {
    if (!this.process) {
      throw new Error('Session not started')
    }

    // Record sent message
    if (this.recorder) {
      this.recorder.recordSent(response as any, this.currentTurnNumber)
    }

    this.process.stdin!.write(JSON.stringify(response) + '\n')
  }

  private handleStderr(data: Buffer): void {
    // Log stderr but don't treat as error (CLI uses stderr for some logging)
    console.error('Claude CLI stderr:', data.toString())
  }

  private handleExit(code: number | null): void {
    // Don't report errors if we're intentionally stopping
    if (this.isStopping) {
      return
    }

    // Report unexpected exits (non-zero exit codes)
    if (code !== 0 && code !== null) {
      this.emitError(new Error(`Claude CLI exited unexpectedly with code ${code}`), 'process_exit')
    }
  }

  // ============================================================================
  // Private: Stream Accumulation Events
  // ============================================================================

  private setupAccumulatorEvents(): void {
    this.accumulator.on('text_chunk', (data: { index: number; text: string }) => {
      if (!this.currentTurn) return

      const text = data.text
      const previousText = this.currentTurn.fullText
      this.currentTurn.fullText += text

      // Record text update
      if (this.recorder) {
        this.recorder.updateTurnText(this.currentTurnNumber, this.currentTurn.fullText)
      }

      if (this.config.onText) {
        this.config.onText({
          turnNumber: this.currentTurnNumber,
          text,
          fullText: this.currentTurn.fullText
        })
      }
    })

    this.accumulator.on('thinking_chunk', (thinking: string) => {
      if (!this.currentTurn) return

      this.currentTurn.fullThinking += thinking

      if (this.config.onThinking) {
        this.config.onThinking({
          turnNumber: this.currentTurnNumber,
          thinking,
          fullThinking: this.currentTurn.fullThinking
        })
      }
    })

    this.accumulator.on('tool_input_chunk', (data: { index: number; chunk: string }) => {
      if (!this.currentTurn) return

      // The accumulator emits { index, chunk } - we need to map index to the actual tool
      // For now, track tools by index in addition to id
      const { index, chunk } = data

      // We'll need to find the tool by index from the block_start events
      // For now, store the partial input by index
      if (!this.currentTurn.toolInputByIndex) {
        this.currentTurn.toolInputByIndex = new Map()
      }

      const existing = this.currentTurn.toolInputByIndex.get(index) || ''
      this.currentTurn.toolInputByIndex.set(index, existing + chunk)
    })

    this.accumulator.on('block_complete', (data: { index: number; block: ContentBlock }) => {
      if (!this.currentTurn) return

      const { index, block } = data

      // Check if this is a tool use block
      if (block.type === 'tool_use') {
        const toolUseBlock = block as ToolUseBlock

        // Register tool if not already registered
        let tool = this.currentTurn.tools.get(toolUseBlock.id)
        if (!tool) {
          tool = {
            id: toolUseBlock.id,
            name: toolUseBlock.name,
            partialInput: '',
            startTime: new Date()
          }
          this.currentTurn.tools.set(toolUseBlock.id, tool)

          // Record tool usage
          if (this.recorder) {
            this.recorder.addToolToTurn(this.currentTurnNumber, toolUseBlock.name)
          }

          if (this.config.onToolStart) {
            this.config.onToolStart({
              turnNumber: this.currentTurnNumber,
              id: toolUseBlock.id,
              name: toolUseBlock.name,
              timestamp: tool.startTime
            })
          }
        }

        // Set the parsed input
        tool.input = toolUseBlock.input

        // Emit tool complete event
        if (this.config.onToolComplete) {
          this.config.onToolComplete({
            turnNumber: this.currentTurnNumber,
            id: toolUseBlock.id,
            name: toolUseBlock.name,
            input: toolUseBlock.input,
            timestamp: new Date()
          })
        }
      }
    })
  }

  private emitError(error: Error, context: string): void {
    const sessionError: SessionError = {
      turnNumber: this.currentTurnNumber || undefined,
      error,
      context
    }

    if (this.config.onError) {
      this.config.onError(sessionError)
    } else {
      console.error('[ClaudeSession Error]', context, ':', error)
    }
  }
}
