/**
 * Example: Session Recording and Persistence
 *
 * Demonstrates:
 * - Recording all messages
 * - Auto-saving to disk
 * - Loading and inspecting recordings
 * - Session resume (conceptual)
 */

import { ClaudeSession } from '../ClaudeSession'
import * as path from 'path'

console.log('=== Session Recording Example ===\n')

// ============================================================================
// Example 1: Basic Recording
// ============================================================================

async function example1_BasicRecording() {
  console.log('Example 1: Basic Recording\n')

  const session = new ClaudeSession({
    model: 'haiku',
    recordMessages: true,  // Enable recording

    onReady: (info) => {
      console.log('✓ Session ready:', info.sessionId)
    },

    onText: (chunk) => {
      process.stdout.write(chunk.text)
    },

    onTurnComplete: async (turn) => {
      console.log(`\n[Turn ${turn.turnNumber} complete]`)

      // Get recording summary
      const summary = session.getRecordingSummary()
      console.log('\nRecording Summary:')
      console.log(`  Total messages: ${summary?.totalMessages}`)
      console.log(`  Sent: ${summary?.sentMessages}`)
      console.log(`  Received: ${summary?.receivedMessages}`)
      console.log(`  Cost: $${summary?.totalCost.toFixed(4)}`)

      // Get recording directory (already streamed)
      const dirPath = session.getRecordingPath()
      console.log(`\n✓ Recording directory: ${dirPath}`)

      session.stop()
    }
  })

  session.start()
  session.sendMessage('What is 2+2?')
}

// ============================================================================
// Example 2: Auto-Save Recording
// ============================================================================

async function example2_AutoSave() {
  console.log('\n\nExample 2: Auto-Streaming Recording\n')

  const session = new ClaudeSession({
    model: 'haiku',
    recordMessages: true,  // Streams to disk automatically
    recordingDir: path.join(process.cwd(), 'recordings'),

    onReady: (info) => {
      console.log('✓ Recording enabled with auto-streaming')
      console.log(`✓ Recording to: ${session.getRecording()?.metadata.sessionId}`)
    },

    onText: (chunk) => {
      process.stdout.write(chunk.text)
    },

    onTurnComplete: (turn) => {
      console.log(`\n[Turn ${turn.turnNumber} complete - streamed to disk]`)

      if (turn.turnNumber === 2) {
        session.stop()
      } else {
        // Continue with another turn
        setTimeout(() => {
          session.sendMessage('What about 3+3?')
        }, 1000)
      }
    }
  })

  session.start()
  session.sendMessage('What is 2+2?')
}

// ============================================================================
// Example 3: Inspect Recording
// ============================================================================

async function example3_InspectRecording() {
  console.log('\n\nExample 3: Inspect Recording\n')

  // Assume we have a recording file
  const recordingPath = process.argv[2]

  if (!recordingPath) {
    console.log('Usage: provide recording file path as argument')
    return
  }

  try {
    const recording = await ClaudeSession.loadRecording(recordingPath)

    console.log('=== Session Recording ===')
    console.log(`Session ID: ${recording.metadata.sessionId}`)
    console.log(`Model: ${recording.metadata.model}`)
    console.log(`Duration: ${recording.metadata.endTime ?
      new Date(recording.metadata.endTime).getTime() - new Date(recording.metadata.startTime).getTime() : 0}ms`)
    console.log(`Total Turns: ${recording.metadata.totalTurns}`)
    console.log(`Total Messages: ${recording.metadata.totalMessages}`)
    console.log()

    console.log('=== Turns ===')
    recording.turns.forEach(turn => {
      console.log(`\nTurn ${turn.turnNumber}:`)
      console.log(`  User: "${String(turn.userMessage).substring(0, 50)}..."`)
      console.log(`  Assistant: "${turn.assistantText.substring(0, 50)}..."`)
      console.log(`  Tools: ${turn.toolsUsed.join(', ') || 'none'}`)
      console.log(`  Duration: ${turn.duration}ms`)
      console.log(`  Cost: $${turn.cost?.toFixed(4)}`)
      console.log(`  Tokens: ${turn.tokens?.input} → ${turn.tokens?.output}`)
    })

    console.log('\n=== Messages ===')
    console.log(`Total: ${recording.messages.length}`)
    console.log('First 5 messages:')
    recording.messages.slice(0, 5).forEach((msg, i) => {
      console.log(`${i + 1}. [${msg.direction}] ${msg.message.type} (turn ${msg.turnNumber})`)
    })

  } catch (error) {
    console.error('Error loading recording:', error)
  }
}

// ============================================================================
// Example 4: Debug with Recording
// ============================================================================

async function example4_DebugWithRecording() {
  console.log('\n\nExample 4: Debug with Recording\n')

  const session = new ClaudeSession({
    model: 'haiku',
    recordMessages: true,

    onReady: () => {
      console.log('✓ Session ready - all messages being recorded')
    },

    onText: (chunk) => {
      process.stdout.write(chunk.text)
    },

    onToolComplete: (tool) => {
      console.log(`\n[Tool: ${tool.name}]`)
      console.log(`Input: ${JSON.stringify(tool.input).substring(0, 100)}...`)

      // Mock tool execution
      session.sendToolResults([{
        toolUseId: tool.id,
        content: 'Mock result',
        isError: false
      }])
    },

    onTurnComplete: async (turn) => {
      console.log(`\n[Turn ${turn.turnNumber} complete]`)

      // Get full recording for debugging
      const recording = session.getRecording()

      if (recording) {
        console.log('\n=== Debug: Message Flow ===')
        const turnMessages = recording.messages.filter(m => m.turnNumber === turn.turnNumber)

        turnMessages.forEach((msg, i) => {
          const time = new Date(msg.timestamp).toISOString().substring(11, 23)
          console.log(`${i + 1}. [${time}] ${msg.direction} ${msg.message.type}`)
        })

        // Save for later inspection
        const dirPath = session.getRecordingPath()
        console.log(`\n✓ Debug recording directory: ${dirPath}`)
      }

      session.stop()
    },

    onError: async (error) => {
      console.error('\n❌ Error:', error.error.message)

      // Get recording directory for debugging
      const dirPath = session.getRecordingPath()
      console.log(`✓ Error recording directory: ${dirPath}`)

      session.stop()
    }
  })

  session.start()
  session.sendMessage('List files in current directory')
}

// ============================================================================
// Example 5: Session Resume (Conceptual)
// ============================================================================

async function example5_SessionResume() {
  console.log('\n\nExample 5: Session Resume (Conceptual)\n')

  // Load previous recording
  const recordingPath = process.argv[2]

  if (!recordingPath) {
    console.log('Usage: provide recording file path to resume')
    return
  }

  try {
    const recording = await ClaudeSession.loadRecording(recordingPath)

    console.log('=== Previous Session ===')
    console.log(`Session ID: ${recording.metadata.sessionId}`)
    console.log(`Turns: ${recording.metadata.totalTurns}`)
    console.log()

    // Show conversation history
    console.log('=== Conversation History ===')
    recording.turns.forEach(turn => {
      console.log(`\nTurn ${turn.turnNumber}:`)
      console.log(`  You: ${String(turn.userMessage).substring(0, 60)}`)
      console.log(`  Claude: ${turn.assistantText.substring(0, 60)}`)
    })

    console.log('\n=== Starting New Session ===')

    // Start new session with context from recording
    const session = new ClaudeSession({
      model: recording.metadata.model,
      cwd: recording.metadata.cwd,
      recordMessages: true,

      onReady: () => {
        console.log('✓ New session ready')
        console.log('  (Note: actual session resume would require sending')
        console.log('   conversation history to maintain context)')
      },

      onText: (chunk) => {
        process.stdout.write(chunk.text)
      },

      onTurnComplete: (turn) => {
        console.log(`\n[Turn ${turn.turnNumber} complete]`)

        const dirPath = session.getRecordingPath()
        console.log(`✓ Recording directory: ${dirPath}`)

        session.stop()
      }
    })

    session.start()

    // Resume conversation
    console.log('\nResuming conversation...')
    session.sendMessage('Can you summarize what we discussed?')

  } catch (error) {
    console.error('Error:', error)
  }
}

// ============================================================================
// Run Examples
// ============================================================================

const exampleNum = parseInt(process.argv[2] || '1')

switch (exampleNum) {
  case 1:
    example1_BasicRecording()
    break
  case 2:
    example2_AutoSave()
    break
  case 3:
    example3_InspectRecording()
    break
  case 4:
    example4_DebugWithRecording()
    break
  case 5:
    example5_SessionResume()
    break
  default:
    console.log('Usage: node recording-example.js [1-5] [recording-file-path]')
    console.log('Examples:')
    console.log('  1 - Basic Recording')
    console.log('  2 - Auto-Save Recording')
    console.log('  3 - Inspect Recording (requires file path)')
    console.log('  4 - Debug with Recording')
    console.log('  5 - Session Resume (requires file path)')
}
