/**
 * TurnManager - Manages turn lifecycle and state
 *
 * Responsibilities:
 * - Track current turn number
 * - Manage turn state (start time, messages, tools, results)
 * - Handle turn completion promises (imperative API)
 * - Maintain turn history
 */

import { ContentBlock } from './types'

export interface TurnState {
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

  // Results
  toolResults: Array<{
    toolUseId: string
    content: string
    isError: boolean
  }>
}

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

interface TurnCompletionResolver {
  resolve: (result: TurnComplete) => void
  reject: (error: Error) => void
  timeout?: NodeJS.Timeout
}

export class TurnManager {
  private currentTurnNumber = 0
  private currentTurn?: TurnState
  private turns: TurnState[] = []

  // Promise-based turn completion tracking
  private turnCompletionResolvers = new Map<number, TurnCompletionResolver>()

  /**
   * Start a new turn
   */
  startTurn(content: string | ContentBlock[]): TurnState {
    this.currentTurnNumber++
    this.currentTurn = {
      turnNumber: this.currentTurnNumber,
      userMessage: content,
      startTime: new Date(),
      fullText: '',
      fullThinking: '',
      tools: new Map(),
      toolResults: []
    }
    this.turns.push(this.currentTurn)
    return this.currentTurn
  }

  /**
   * Get current turn state
   */
  getCurrentTurn(): TurnState | undefined {
    return this.currentTurn
  }

  /**
   * Get current turn number
   */
  getCurrentTurnNumber(): number {
    return this.currentTurnNumber
  }

  /**
   * Get turn by number
   */
  getTurn(turnNumber: number): TurnState | undefined {
    return this.turns.find(t => t.turnNumber === turnNumber)
  }

  /**
   * Get all turns
   */
  getTurns(): ReadonlyArray<TurnState> {
    return this.turns
  }

  /**
   * Complete a turn with result
   */
  completeTurn(result: TurnComplete): void {
    const resolver = this.turnCompletionResolvers.get(result.turnNumber)
    if (resolver) {
      if (resolver.timeout) {
        clearTimeout(resolver.timeout)
      }
      this.turnCompletionResolvers.delete(result.turnNumber)

      if (!result.success && result.error) {
        resolver.reject(new Error(result.error))
      } else {
        resolver.resolve(result)
      }
    }
  }

  /**
   * Wait for a specific turn to complete (imperative API)
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
   * Clear all turn data (for testing/reset)
   */
  clear(): void {
    this.currentTurnNumber = 0
    this.currentTurn = undefined
    this.turns = []

    // Reject any pending promises
    this.turnCompletionResolvers.forEach((resolver) => {
      if (resolver.timeout) {
        clearTimeout(resolver.timeout)
      }
      resolver.reject(new Error('Turn manager cleared'))
    })
    this.turnCompletionResolvers.clear()
  }
}
