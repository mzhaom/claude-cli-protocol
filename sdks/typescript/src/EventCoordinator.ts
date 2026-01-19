/**
 * EventCoordinator - Coordinates events between components
 *
 * Responsibilities:
 * - Wire StreamAccumulator events to user callbacks
 * - Wire SessionState transitions to user callbacks
 * - Coordinate turn lifecycle events
 * - Central event routing hub
 */

import { StreamAccumulator } from './StreamAccumulator'
import { TurnManager, TurnState } from './TurnManager'
import { SessionRecorder } from './SessionRecorder'

// Import callback types from ClaudeSession
export interface TextChunk {
  turnNumber: number
  text: string
  fullText: string
}

export interface ThinkingChunk {
  turnNumber: number
  thinking: string
  fullThinking: string
}

export interface ToolStart {
  turnNumber: number
  id: string
  name: string
  timestamp: Date
}

export interface ToolProgress {
  turnNumber: number
  id: string
  name: string
  partialInput: string
  inputChunk: string
}

export interface ToolComplete {
  turnNumber: number
  id: string
  name: string
  input: Record<string, unknown>
}

export interface EventCallbacks {
  onText?: (chunk: TextChunk) => void
  onThinking?: (chunk: ThinkingChunk) => void
  onToolStart?: (tool: ToolStart) => void
  onToolProgress?: (tool: ToolProgress) => void
  onToolComplete?: (tool: ToolComplete) => void
}

/**
 * Coordinates events between StreamAccumulator, TurnManager, and user callbacks
 */
export class EventCoordinator {
  constructor(
    private accumulator: StreamAccumulator,
    private turnManager: TurnManager,
    private callbacks: EventCallbacks,
    private recorder?: SessionRecorder
  ) {
    this.setupAccumulatorEvents()
  }

  /**
   * Setup event handlers for StreamAccumulator
   */
  private setupAccumulatorEvents(): void {
    // Text chunks
    this.accumulator.on('text_chunk', (data) => {
      const currentTurn = this.turnManager.getCurrentTurn()
      if (!currentTurn) return

      const previousText = currentTurn.fullText
      currentTurn.fullText += data.text

      // Record text update
      if (this.recorder) {
        this.recorder.updateTurnText(currentTurn.turnNumber, currentTurn.fullText)
      }

      if (this.callbacks.onText) {
        this.callbacks.onText({
          turnNumber: currentTurn.turnNumber,
          text: data.text,
          fullText: currentTurn.fullText
        })
      }
    })

    // Thinking chunks
    this.accumulator.on('thinking_chunk', (data) => {
      const currentTurn = this.turnManager.getCurrentTurn()
      if (!currentTurn) return

      currentTurn.fullThinking += data.thinking

      if (this.callbacks.onThinking) {
        this.callbacks.onThinking({
          turnNumber: currentTurn.turnNumber,
          thinking: data.thinking,
          fullThinking: currentTurn.fullThinking
        })
      }
    })

    // Tool input chunks
    this.accumulator.on('tool_input_chunk', (data) => {
      const currentTurn = this.turnManager.getCurrentTurn()
      if (!currentTurn) return

      let tool = currentTurn.tools.get(data.toolId)

      if (!tool) {
        // Tool starting
        tool = {
          id: data.toolId,
          name: data.name,
          partialInput: '',
          startTime: new Date()
        }
        currentTurn.tools.set(data.toolId, tool)

        // Record tool usage
        if (this.recorder) {
          this.recorder.addToolToTurn(currentTurn.turnNumber, data.name)
        }

        if (this.callbacks.onToolStart) {
          this.callbacks.onToolStart({
            turnNumber: currentTurn.turnNumber,
            id: data.toolId,
            name: data.name,
            timestamp: tool.startTime
          })
        }
      }

      const chunk = data.partialInput.substring(tool.partialInput.length)
      tool.partialInput = data.partialInput

      if (this.callbacks.onToolProgress) {
        this.callbacks.onToolProgress({
          turnNumber: currentTurn.turnNumber,
          id: data.toolId,
          name: data.name,
          partialInput: data.partialInput,
          inputChunk: chunk
        })
      }
    })

    // Block complete
    this.accumulator.on('block_complete', (data) => {
      const currentTurn = this.turnManager.getCurrentTurn()
      if (!currentTurn) return

      // Check if this is a tool use block
      if (data.block.type === 'tool_use') {
        const tool = currentTurn.tools.get(data.block.id)

        if (tool && !tool.input) {
          tool.input = data.block.input

          if (this.callbacks.onToolComplete) {
            this.callbacks.onToolComplete({
              turnNumber: currentTurn.turnNumber,
              id: data.block.id,
              name: data.block.name,
              input: data.block.input
            })
          }
        }
      }
    })
  }

  /**
   * Update callbacks (for dynamic callback changes)
   */
  updateCallbacks(callbacks: EventCallbacks): void {
    this.callbacks = { ...this.callbacks, ...callbacks }
  }
}
