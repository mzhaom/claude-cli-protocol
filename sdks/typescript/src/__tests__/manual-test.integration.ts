/**
 * Manual test script - run with: npm run dev or ts-node
 */

import { ClaudeSession } from '../ClaudeSession'

console.log('=== Manual ClaudeSession Test ===\n')

const session = new ClaudeSession({
  model: 'haiku',

  onReady: (info) => {
    // NOTE: This is called AFTER the first message is sent (see line 54)
    // The Claude CLI protocol requires sending a message before init is received
    console.log('✓ Session ready')
    console.log('  ID:', info.sessionId)
    console.log('  Model:', info.model)
    console.log('  Tools:', info.tools.length)
    console.log()
  },

  onText: (chunk) => {
    process.stdout.write(chunk.text)
  },

  onTurnComplete: (turn) => {
    console.log(`\n\n[Turn ${turn.turnNumber}: ${turn.durationMs}ms, $${turn.usage.costUSD.toFixed(4)}]`)

    if (turn.turnNumber === 1) {
      setTimeout(() => {
        console.log('\n>>> Turn 2: Follow-up question')
        session.sendMessage('What about 3+3?')
      }, 1000)
    } else if (turn.turnNumber === 2) {
      setTimeout(() => {
        console.log('\n\n=== Test Complete ===')
        console.log(`Total turns: ${session.getCurrentTurnNumber()}`)
        session.stop()
        process.exit(0)
      }, 1000)
    }
  },

  onError: (error) => {
    console.error('\n❌ Error:', error.error.message)
  }
})

session.start()

// IMPORTANT: The first sendMessage() triggers the init event (which calls onReady)
session.sendMessage('What is 2+2?')

setTimeout(() => {
  console.error('\n⏱ Timeout')
  session.stop()
  process.exit(1)
}, 60000)
