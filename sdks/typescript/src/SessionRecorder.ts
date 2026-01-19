/**
 * Session Recording and Persistence
 *
 * Streams messages to disk in real-time as they are sent/received.
 * Writes metadata once at initialization, then appends messages to separate JSONL files.
 */

import * as fs from 'fs/promises'
import * as path from 'path'
import { ProtocolMessage } from './types'

// ============================================================================
// Types
// ============================================================================

/** A recorded message with metadata */
export interface RecordedMessage {
  /** Unique ID for this message */
  id: string

  /** Timestamp when message was recorded */
  timestamp: Date

  /** Direction: sent to CLI or received from CLI */
  direction: 'sent' | 'received'

  /** Turn number this message belongs to */
  turnNumber?: number

  /** The actual protocol message */
  message: ProtocolMessage | UserMessageSent | ControlRequestSent
}

/** User message sent to CLI (not a ProtocolMessage) */
export interface UserMessageSent {
  type: 'user'
  message: {
    role: 'user'
    content: string | any[]
  }
}

/** Control request sent to CLI */
export interface ControlRequestSent {
  type: 'control_request'
  request_id: string
  request: {
    subtype: string
    [key: string]: any
  }
}

/** Complete session recording */
export interface SessionRecording {
  /** Recording metadata */
  metadata: {
    sessionId: string
    model: string
    startTime: Date
    endTime?: Date
    cwd: string
    tools: string[]
    totalTurns: number
    totalMessages: number
    claudeCodeVersion?: string
    permissionMode?: string
  }

  /** All recorded messages in order (deprecated - not stored in memory) */
  messages: RecordedMessage[]

  /** Turn summaries */
  turns: TurnSummary[]
}

/** Summary of a turn */
export interface TurnSummary {
  turnNumber: number
  startTime: Date
  endTime?: Date
  userMessage: string | any
  assistantText: string
  toolsUsed: string[]
  duration?: number
  cost?: number
  tokens?: {
    input: number
    output: number
  }
}

// ============================================================================
// SessionRecorder
// ============================================================================

export class SessionRecorder {
  private turns: Map<number, TurnSummary> = new Map()
  private messageCounter = 0
  private sentMessageCount = 0
  private receivedMessageCount = 0

  private sessionId?: string
  private model?: string
  private startTime?: Date
  private cwd?: string
  private tools?: string[]
  private claudeCodeVersion?: string
  private permissionMode?: string

  // File handles for streaming
  private sessionDir?: string
  private toCliPath?: string
  private fromCliPath?: string
  private metadataWritten = false

  // Buffer for messages received before session directory is created
  private pendingSentMessages: RecordedMessage[] = []
  private pendingReceivedMessages: RecordedMessage[] = []

  constructor(
    private options: {
      /** Directory to save recordings (defaults to .claude-sessions in cwd) */
      saveDir?: string
    } = {}
  ) {}

  /**
   * Initialize recording with session info
   * Creates session directory and writes metadata file immediately
   * Only captures on first call - subsequent calls don't overwrite
   */
  async initialize(info: {
    sessionId: string
    model: string
    cwd: string
    tools: string[]
    claudeCodeVersion?: string
    permissionMode?: string
  }): Promise<void> {
    // Only initialize once - don't overwrite with subsequent init messages
    if (this.sessionId) {
      return
    }

    this.sessionId = info.sessionId
    this.model = info.model
    this.cwd = info.cwd
    this.tools = info.tools
    this.claudeCodeVersion = info.claudeCodeVersion
    this.permissionMode = info.permissionMode
    this.startTime = new Date()

    // Create session directory
    await this.createSessionDirectory()

    // Flush any pending messages that were buffered before directory was created
    await this.flushPendingMessages()

    // Write metadata file immediately
    await this.writeMetadata()
  }

  /**
   * Flush pending messages that were buffered before session directory was created
   */
  private async flushPendingMessages(): Promise<void> {
    // Flush sent messages in order
    for (const msg of this.pendingSentMessages) {
      if (this.toCliPath) {
        await this.appendToFile(this.toCliPath, msg)
      }
    }
    this.pendingSentMessages = []

    // Flush received messages in order
    for (const msg of this.pendingReceivedMessages) {
      if (this.fromCliPath) {
        await this.appendToFile(this.fromCliPath, msg)
      }
    }
    this.pendingReceivedMessages = []
  }

  /**
   * Record a message sent to CLI
   * Appends to to_cli.jsonl immediately, or buffers if directory not yet created
   */
  recordSent(message: UserMessageSent | ControlRequestSent, turnNumber?: number): void {
    const recorded: RecordedMessage = {
      id: this.generateId(),
      timestamp: new Date(),
      direction: 'sent',
      turnNumber,
      message
    }

    this.sentMessageCount++

    // Append to to_cli.jsonl asynchronously (fire and forget)
    if (this.toCliPath) {
      this.appendToFile(this.toCliPath, recorded).catch(err => {
        console.error('Failed to record sent message:', err)
      })
    } else {
      // Buffer until session directory is created
      this.pendingSentMessages.push(recorded)
    }
  }

  /**
   * Record a message received from CLI
   * Appends to from_cli.jsonl immediately, or buffers if directory not yet created
   */
  recordReceived(message: ProtocolMessage, turnNumber?: number): void {
    const recorded: RecordedMessage = {
      id: this.generateId(),
      timestamp: new Date(),
      direction: 'received',
      turnNumber,
      message
    }

    this.receivedMessageCount++

    // Append to from_cli.jsonl asynchronously (fire and forget)
    if (this.fromCliPath) {
      this.appendToFile(this.fromCliPath, recorded).catch(err => {
        console.error('Failed to record received message:', err)
      })
    } else {
      // Buffer until session directory is created
      this.pendingReceivedMessages.push(recorded)
    }
  }

  /**
   * Start tracking a new turn
   */
  startTurn(turnNumber: number, userMessage: string | any): void {
    this.turns.set(turnNumber, {
      turnNumber,
      startTime: new Date(),
      userMessage,
      assistantText: '',
      toolsUsed: [],
      tokens: { input: 0, output: 0 }
    })
  }

  /**
   * Update turn with assistant text
   */
  updateTurnText(turnNumber: number, text: string): void {
    const turn = this.turns.get(turnNumber)
    if (turn) {
      turn.assistantText = text
    }
  }

  /**
   * Add tool use to turn
   */
  addToolToTurn(turnNumber: number, toolName: string): void {
    const turn = this.turns.get(turnNumber)
    if (turn && !turn.toolsUsed.includes(toolName)) {
      turn.toolsUsed.push(toolName)
    }
  }

  /**
   * Complete a turn with final stats
   * Updates metadata file with latest turn information
   */
  completeTurn(
    turnNumber: number,
    stats: {
      duration: number
      cost: number
      tokens: { input: number; output: number }
    }
  ): void {
    const turn = this.turns.get(turnNumber)
    if (turn) {
      turn.endTime = new Date()
      turn.duration = stats.duration
      turn.cost = stats.cost
      turn.tokens = stats.tokens
    }

    // Update metadata file with latest turn stats (fire and forget)
    this.writeMetadata().catch(err => {
      console.error('Failed to update metadata:', err)
    })
  }

  /**
   * Get all recorded messages
   * @deprecated Messages are no longer stored in memory - use getRecordingPath() to read from disk
   */
  getMessages(): RecordedMessage[] {
    console.warn('getMessages() is deprecated - messages are streamed to disk, not stored in memory')
    return []
  }

  /**
   * Get messages for a specific turn
   * @deprecated Messages are no longer stored in memory - use getRecordingPath() to read from disk
   */
  getMessagesByTurn(turnNumber: number): RecordedMessage[] {
    console.warn('getMessagesByTurn() is deprecated - messages are streamed to disk, not stored in memory')
    return []
  }

  /**
   * Get complete recording (metadata + turns only, no messages)
   */
  getRecording(): SessionRecording {
    return {
      metadata: {
        sessionId: this.sessionId || 'unknown',
        model: this.model || 'unknown',
        startTime: this.startTime || new Date(),
        endTime: new Date(),
        cwd: this.cwd || process.cwd(),
        tools: this.tools || [],
        totalTurns: this.turns.size,
        totalMessages: this.sentMessageCount + this.receivedMessageCount,
        claudeCodeVersion: this.claudeCodeVersion,
        permissionMode: this.permissionMode
      },
      messages: [], // No longer stored in memory
      turns: Array.from(this.turns.values())
    }
  }

  /**
   * Get path to the session recording directory
   */
  getRecordingPath(): string | undefined {
    return this.sessionDir
  }

  /**
   * Save recording to disk
   * @deprecated Recording is now streamed automatically - this method just returns the directory path
   */
  async save(filePath?: string): Promise<string> {
    if (filePath) {
      console.warn('save(filePath) is deprecated - recordings are now streamed to a session directory')
    }
    return this.sessionDir || ''
  }

  /**
   * Load recording from disk
   *
   * @param dirPath - Path to session directory containing meta.json, to_cli.jsonl, from_cli.jsonl
   */
  static async load(dirPath: string): Promise<SessionRecording> {
    const metaPath = path.join(dirPath, 'meta.json')
    const toCliPath = path.join(dirPath, 'to_cli.jsonl')
    const fromCliPath = path.join(dirPath, 'from_cli.jsonl')

    // Load metadata
    const metaJson = await fs.readFile(metaPath, 'utf-8')
    const metadata = JSON.parse(metaJson, this.dateReviver)

    // Load messages from both files
    const messages: RecordedMessage[] = []

    // Load sent messages
    try {
      const toCliContent = await fs.readFile(toCliPath, 'utf-8')
      const sentMessages = toCliContent
        .split('\n')
        .filter(line => line.trim())
        .map(line => JSON.parse(line, this.dateReviver))
      messages.push(...sentMessages)
    } catch (err) {
      // File might not exist yet
    }

    // Load received messages
    try {
      const fromCliContent = await fs.readFile(fromCliPath, 'utf-8')
      const receivedMessages = fromCliContent
        .split('\n')
        .filter(line => line.trim())
        .map(line => JSON.parse(line, this.dateReviver))
      messages.push(...receivedMessages)
    } catch (err) {
      // File might not exist yet
    }

    // Sort by timestamp to maintain order
    messages.sort((a, b) => a.timestamp.getTime() - b.timestamp.getTime())

    return {
      metadata: {
        sessionId: metadata.sessionId,
        model: metadata.model,
        startTime: metadata.startTime,
        endTime: metadata.endTime,
        cwd: metadata.cwd,
        tools: metadata.tools,
        totalTurns: metadata.totalTurns,
        totalMessages: metadata.totalMessages,
        claudeCodeVersion: metadata.claudeCodeVersion,
        permissionMode: metadata.permissionMode
      },
      messages,
      turns: metadata.turns || []
    }
  }

  /**
   * JSON reviver function to restore Date objects
   */
  private static dateReviver(key: string, value: any): any {
    if (key === 'timestamp' || key === 'startTime' || key === 'endTime') {
      return value ? new Date(value) : undefined
    }
    return value
  }

  /**
   * Get summary statistics
   */
  getSummary(): {
    totalMessages: number
    sentMessages: number
    receivedMessages: number
    totalTurns: number
    totalCost: number
    totalTokens: { input: number; output: number }
    duration: number
  } {
    let totalCost = 0
    let totalInputTokens = 0
    let totalOutputTokens = 0

    this.turns.forEach(turn => {
      totalCost += turn.cost || 0
      totalInputTokens += turn.tokens?.input || 0
      totalOutputTokens += turn.tokens?.output || 0
    })

    const duration = this.startTime
      ? Date.now() - this.startTime.getTime()
      : 0

    return {
      totalMessages: this.sentMessageCount + this.receivedMessageCount,
      sentMessages: this.sentMessageCount,
      receivedMessages: this.receivedMessageCount,
      totalTurns: this.turns.size,
      totalCost,
      totalTokens: {
        input: totalInputTokens,
        output: totalOutputTokens
      },
      duration
    }
  }

  /**
   * Export messages as JSONL (alias for save with custom path)
   *
   * @deprecated Recording is now streamed automatically - use getRecordingPath() instead
   */
  async exportJSONL(filePath: string): Promise<void> {
    console.warn('exportJSONL() is deprecated - recordings are now streamed automatically')
  }

  /**
   * Clear all recorded data
   */
  clear(): void {
    this.turns.clear()
    this.messageCounter = 0
    this.sentMessageCount = 0
    this.receivedMessageCount = 0
  }

  // ============================================================================
  // Private Methods
  // ============================================================================

  private generateId(): string {
    return `msg_${++this.messageCounter}_${Date.now()}`
  }

  private async createSessionDirectory(): Promise<void> {
    const baseDir = this.options.saveDir || path.join(process.cwd(), '.claude-sessions')
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-')
    const sessionId = (this.sessionId || 'unknown').substring(0, 8)

    this.sessionDir = path.join(baseDir, `session-${sessionId}-${timestamp}`)
    this.toCliPath = path.join(this.sessionDir, 'to_cli.jsonl')
    this.fromCliPath = path.join(this.sessionDir, 'from_cli.jsonl')

    await fs.mkdir(this.sessionDir, { recursive: true })
  }

  private async writeMetadata(): Promise<void> {
    if (!this.sessionDir) {
      return
    }

    const metaPath = path.join(this.sessionDir, 'meta.json')

    const metadata = {
      sessionId: this.sessionId || 'unknown',
      model: this.model || 'unknown',
      startTime: this.startTime || new Date(),
      endTime: new Date(),
      cwd: this.cwd || process.cwd(),
      tools: this.tools || [],
      totalTurns: this.turns.size,
      totalMessages: this.sentMessageCount + this.receivedMessageCount,
      claudeCodeVersion: this.claudeCodeVersion,
      permissionMode: this.permissionMode,
      turns: Array.from(this.turns.values())
    }

    await fs.writeFile(metaPath, JSON.stringify(metadata, null, 2), 'utf-8')
    this.metadataWritten = true
  }

  private async appendToFile(filePath: string, message: RecordedMessage): Promise<void> {
    const line = JSON.stringify(message) + '\n'
    await fs.appendFile(filePath, line, 'utf-8')
  }
}
