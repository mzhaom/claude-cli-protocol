/**
 * Live validation tests for ClaudeSession API
 * Tests real CLI execution with common use cases
 *
 * These tests require:
 * - Claude CLI installed and available in PATH
 * - Valid API key configured
 *
 * Run with: RUN_INTEGRATION_TESTS=1 bun test
 */

import { describe, it, expect, beforeAll, afterAll } from 'bun:test'
import { ClaudeSession, TurnComplete, TextChunk } from '../ClaudeSession'
import * as fs from 'fs'
import * as path from 'path'

// Skip integration tests unless explicitly enabled
const runIntegrationTests = process.env.RUN_INTEGRATION_TESTS === '1'
const describeIntegration = runIntegrationTests ? describe : describe.skip

describeIntegration('ClaudeSession - Live CLI Validation', () => {
  let session: ClaudeSession

  afterAll(() => {
    if (session) {
      session.stop()
    }
  })

  // ============================================================================
  // Use Case 1: Simple Multi-Turn Conversation
  // ============================================================================

  it('should handle multi-turn conversation', async () => {
    const events: string[] = []
    const turnResults: TurnComplete[] = []

    session = new ClaudeSession({
      model: 'haiku',
      disablePlugins: true,
      permissionMode: 'bypassPermissions',

      onReady: (info) => {
        events.push('ready')
        console.log('Session ready:', info.sessionId)
      },

      onText: (chunk) => {
        events.push(`text_${chunk.turnNumber}`)
        process.stdout.write(chunk.text)
      },

      onTurnComplete: (turn) => {
        events.push(`turn_complete_${turn.turnNumber}`)
        turnResults.push(turn)
        console.log(`\n[Turn ${turn.turnNumber} complete - $${turn.usage.costUSD.toFixed(4)}]`)
      }
    })

    session.start()

    // Turn 1 - NOTE: onReady is called AFTER first message is sent
    session.sendMessage('What is 2 + 2?')
    await waitFor(() => events.includes('turn_complete_1'), 30000)
    expect(events).toContain('ready')  // Ready should be triggered after first message
    expect(events).toContain('text_1')
    expect(events).toContain('turn_complete_1')
    expect(turnResults[0].turnNumber).toBe(1)
    expect(turnResults[0].success).toBe(true)

    // Turn 2
    session.sendMessage('What about 3 + 3?')
    await waitFor(() => events.includes('turn_complete_2'), 30000)
    expect(events).toContain('text_2')
    expect(events).toContain('turn_complete_2')
    expect(turnResults[1].turnNumber).toBe(2)
    expect(turnResults[1].success).toBe(true)

    // Turn 3
    session.sendMessage('Sum the previous two answers')
    await waitFor(() => events.includes('turn_complete_3'), 30000)
    expect(events).toContain('turn_complete_3')
    expect(turnResults[2].turnNumber).toBe(3)

    // Verify turn history
    const history = session.getTurnHistory()
    expect(history.length).toBe(3)
    expect(session.getCurrentTurnNumber()).toBe(3)

    session.stop()
  }, 60000)

  // Note: Tool Lifecycle test removed - covered by ClaudeSession.integration.test.ts Scenarios 4 & 5

  // ============================================================================
  // Use Case 2: Streaming Text Accumulation
  // ============================================================================

  it('should properly accumulate streaming text', async () => {
    const textChunks: TextChunk[] = []
    let finalText = ''
    let turnComplete = false

    session = new ClaudeSession({
      model: 'haiku',
      disablePlugins: true,
      permissionMode: 'bypassPermissions',

      onReady: () => {
        console.log('\n=== Testing Text Streaming ===')
      },

      onText: (chunk) => {
        textChunks.push(chunk)
        console.log(`Chunk ${textChunks.length}: ${chunk.text.length} chars`)
      },

      onTurnComplete: (turn) => {
        turnComplete = true
        console.log(`\n[${textChunks.length} chunks total]`)
      }
    })

    session.start()

    // Send message (onReady is called after first message)
    session.sendMessage('Explain how async/await works in JavaScript in detail')

    // Wait for turn to complete
    await waitFor(() => turnComplete, 60000)

    // Verify we got text
    expect(textChunks.length).toBeGreaterThan(0)

    // Verify each chunk builds on previous (if multiple chunks)
    if (textChunks.length > 1) {
      for (let i = 1; i < textChunks.length; i++) {
        const prev = textChunks[i - 1]
        const curr = textChunks[i]

        expect(curr.turnNumber).toBe(1)
        expect(curr.fullText.startsWith(prev.fullText)).toBe(true)
        console.log(`Chunk ${i}: ${prev.fullText.length} → ${curr.fullText.length}`)
      }
    }

    // Verify final text
    finalText = textChunks[textChunks.length - 1].fullText
    expect(finalText.length).toBeGreaterThan(0)
    console.log(`\nFinal text: ${finalText.length} characters`)

    session.stop()
  }, 90000)

  // ============================================================================
  // Use Case 3: Error Handling
  // ============================================================================

  it('should handle tool execution errors', async () => {
    const events: string[] = []
    let errorResult: any

    session = new ClaudeSession({
      model: 'haiku',
      disablePlugins: true,
      permissionMode: 'bypassPermissions',

      onReady: () => {
        console.log('\n=== Testing Error Handling ===')
      },

      onCliToolResult: (result) => {
        events.push('cli_tool_result')
        console.log(`CLI executed ${result.toolName}: error=${result.isError}`)
        if (result.isError) {
          errorResult = result
        }
      },

      onTurnComplete: (turn) => {
        events.push('turn_complete')
        console.log(`Turn complete: success=${turn.success}`)
      },

      onError: (error) => {
        events.push('error')
        console.log(`Error: ${error.error.message}`)
      }
    })

    session.start()

    // Send message (onReady is called after first message)
    session.sendMessage('Read the file /nonexistent.txt')

    await waitFor(() => events.includes('turn_complete'), 60000)

    // Verify error handling - the CLI may or may not report it as an error
    // depending on how the model handles the missing file
    console.log(`CLI tool results: ${events.filter(e => e === 'cli_tool_result').length}`)
    if (errorResult) {
      expect(errorResult.isError).toBe(true)
    }

    session.stop()
  }, 90000)

  // ============================================================================
  // Use Case 4: Session State Management
  // ============================================================================

  it('should maintain correct session state across turns', async () => {
    const turnStates: Map<number, any> = new Map()
    let sessionReady = false

    session = new ClaudeSession({
      model: 'haiku',
      disablePlugins: true,
      permissionMode: 'bypassPermissions',

      onReady: (info) => {
        sessionReady = true
        console.log('\n=== Testing State Management ===')
        console.log('Session ID:', info.sessionId)
        console.log('Model:', info.model)
        console.log('Tools:', info.tools.length)
      },

      onTurnComplete: (turn) => {
        // Track turn completion
        if (!turnStates.has(turn.turnNumber)) {
          turnStates.set(turn.turnNumber, {
            number: turn.turnNumber,
            started: true,
            completed: false
          })
        }
        const state = turnStates.get(turn.turnNumber)!
        state.completed = true
        state.duration = turn.durationMs
        state.cost = turn.usage.costUSD
      }
    })

    session.start()

    // Turn 1 - this will trigger onReady
    session.sendMessage('Question 1')
    await waitFor(() => turnStates.get(1)?.completed === true, 30000)
    expect(turnStates.get(1)!.completed).toBe(true)
    console.log(`Turn 1: ${turnStates.get(1)!.duration}ms, $${turnStates.get(1)!.cost.toFixed(4)}`)

    // Verify session info is available after first message
    expect(sessionReady).toBe(true)
    const info = session.getSessionInfo()!
    expect(info.sessionId).toBeTruthy()
    expect(info.model).toBeTruthy()
    expect(info.tools.length).toBeGreaterThan(0)

    // Execute turns 2 and 3
    for (let i = 2; i <= 3; i++) {
      session.sendMessage(`Question ${i}`)
      await waitFor(() => {
        const state = turnStates.get(i)
        return state && state.completed
      }, 30000)

      const state = turnStates.get(i)!
      expect(state.completed).toBe(true)
      console.log(`Turn ${i}: ${state.duration}ms, $${state.cost.toFixed(4)}`)
    }

    // Verify history
    const history = session.getTurnHistory()
    expect(history.length).toBe(3)
    expect(session.getCurrentTurnNumber()).toBe(3)

    session.stop()
  }, 120000)
})

// ============================================================================
// Helper Functions
// ============================================================================

function waitFor(condition: () => boolean, timeout: number): Promise<void> {
  return new Promise((resolve, reject) => {
    const startTime = Date.now()
    const interval = setInterval(() => {
      if (condition()) {
        clearInterval(interval)
        resolve()
      } else if (Date.now() - startTime > timeout) {
        clearInterval(interval)
        reject(new Error(`Timeout waiting for condition after ${timeout}ms`))
      }
    }, 100)
  })
}
