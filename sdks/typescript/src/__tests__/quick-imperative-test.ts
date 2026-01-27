/**
 * Quick test to verify imperative API works
 */

import { ClaudeSession } from '../ClaudeSession'

async function main() {
  console.log('=== Testing Imperative API ===\n')

  const session = new ClaudeSession({
    model: 'haiku',

    onReady: (info) => {
      console.log('✓ Session ready:', info.sessionId)
    },

    onText: (chunk) => {
      process.stdout.write(chunk.text)
    }
  })

  try {
    session.start()

    // Test 1: ask() method (no wait needed - ask() handles initialization)
    console.log('=== Test 1: ask() method ===')
    const result1 = await session.ask('What is 2+2?', 30000)
    console.log(`\n✓ Turn ${result1.turnNumber} complete`)
    console.log(`  Duration: ${result1.durationMs}ms`)
    console.log(`  Cost: $${result1.usage.costUSD.toFixed(4)}`)
    console.log(`  Success: ${result1.success}`)

    // Test 2: waitForTurn() method
    console.log('\n\n=== Test 2: waitForTurn() method ===')
    session.sendMessage('What is 3+3?')
    const result2 = await session.waitForTurn()
    console.log(`\n✓ Turn ${result2.turnNumber} complete`)
    console.log(`  Duration: ${result2.durationMs}ms`)
    console.log(`  Cost: $${result2.usage.costUSD.toFixed(4)}`)

    // Test 3: Sequential asks
    console.log('\n\n=== Test 3: Sequential asks ===')
    const result3 = await session.ask('Sum the previous two answers')
    console.log(`\n✓ Turn ${result3.turnNumber} complete`)
    console.log(`  Duration: ${result3.durationMs}ms`)
    console.log(`  Cost: $${result3.usage.costUSD.toFixed(4)}`)

    // Summary
    const totalCost = result1.usage.costUSD + result2.usage.costUSD + result3.usage.costUSD
    console.log('\n\n=== Summary ===')
    console.log(`Total turns: ${session.getCurrentTurnNumber()}`)
    console.log(`Total cost: $${totalCost.toFixed(4)}`)
    console.log('\n✓ All tests passed!')

  } catch (error) {
    console.error('\n✗ Test failed:', error)
    process.exit(1)
  } finally {
    session.stop()
  }
}

main()
