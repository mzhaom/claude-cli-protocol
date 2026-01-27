/**
 * Examples: Imperative API with waitForTurn and ask
 *
 * Demonstrates how to use the session in an imperative style
 * with async/await instead of callbacks.
 */

import { ClaudeSession } from '../ClaudeSession'

console.log('=== Imperative API Examples ===\n')

// ============================================================================
// Example 1: Simple Imperative Conversation
// ============================================================================

async function example1_SimpleImperative() {
  console.log('Example 1: Simple Imperative Conversation\n')

  const session = new ClaudeSession({
    model: 'haiku',
    onText: (chunk) => process.stdout.write(chunk.text)
  })

  session.start()

  // Wait for session to be ready
  await new Promise(resolve => {
    session.once('ready', resolve)
    setTimeout(resolve, 5000) // Safety timeout
  })

  console.log('>>> Question 1')
  const result1 = await session.ask('What is 2+2?')
  console.log(`\n[Cost: $${result1.usage.costUSD.toFixed(4)}]\n`)

  console.log('>>> Question 2')
  const result2 = await session.ask('What about 3+3?')
  console.log(`\n[Cost: $${result2.usage.costUSD.toFixed(4)}]\n`)

  console.log('>>> Question 3')
  const result3 = await session.ask('Sum the previous two answers')
  console.log(`\n[Cost: $${result3.usage.costUSD.toFixed(4)}]\n`)

  console.log('=== Summary ===')
  console.log(`Total turns: ${result3.turnNumber}`)
  console.log(`Total cost: $${(result1.usage.costUSD + result2.usage.costUSD + result3.usage.costUSD).toFixed(4)}`)

  session.stop()
}

// ============================================================================
// Example 2: Error Handling with Try-Catch
// ============================================================================

async function example2_ErrorHandling() {
  console.log('\n\nExample 2: Error Handling\n')

  const session = new ClaudeSession({
    model: 'haiku',
    onText: (chunk) => process.stdout.write(chunk.text)
  })

  session.start()


  try {
    // This might timeout if the response takes too long
    const result = await session.ask('Complex question...', 5000) // 5s timeout
    console.log(`\n[Success: $${result.usage.costUSD.toFixed(4)}]`)
  } catch (error) {
    console.error(`\n[Error: ${error instanceof Error ? error.message : error}]`)
  }

  session.stop()
}

// ============================================================================
// Example 3: Conditional Workflow
// ============================================================================

async function example3_ConditionalWorkflow() {
  console.log('\n\nExample 3: Conditional Workflow\n')

  const session = new ClaudeSession({
    model: 'haiku',
    onText: (chunk) => process.stdout.write(chunk.text),
    onToolComplete: (tool) => {
      console.log(`\n[Tool: ${tool.name}]`)
      // Mock tool execution
      session.sendToolResults([{
        toolUseId: tool.id,
        content: 'Mock result',
        isError: false
      }])
    }
  })

  session.start()

  // First question
  console.log('>>> Initial question')
  const result1 = await session.ask('What is the current directory?')
  console.log(`\n[Cost: $${result1.usage.costUSD.toFixed(4)}]`)

  // Conditional follow-up based on result
  if (result1.success && result1.usage.costUSD < 0.01) {
    console.log('\n>>> Follow-up (cheap answer, asking more)')
    const result2 = await session.ask('List all files here')
    console.log(`\n[Cost: $${result2.usage.costUSD.toFixed(4)}]`)
  } else {
    console.log('\n>>> Skipping follow-up (too expensive)')
  }

  session.stop()
}

// ============================================================================
// Example 4: Sequential Data Processing
// ============================================================================

async function example4_SequentialProcessing() {
  console.log('\n\nExample 4: Sequential Data Processing\n')

  const session = new ClaudeSession({
    model: 'haiku',
    onText: (chunk) => process.stdout.write(chunk.text)
  })

  session.start()

  const questions = [
    'What is TypeScript?',
    'What is async/await?',
    'What is a Promise?'
  ]

  const results = []

  for (const question of questions) {
    console.log(`\n>>> ${question}`)
    const result = await session.ask(question)
    console.log(`\n[${result.durationMs}ms, $${result.usage.costUSD.toFixed(4)}]`)
    results.push(result)
  }

  console.log('\n=== Summary ===')
  const totalCost = results.reduce((sum, r) => sum + r.usage.costUSD, 0)
  const totalTime = results.reduce((sum, r) => sum + r.durationMs, 0)
  console.log(`Total questions: ${results.length}`)
  console.log(`Total time: ${totalTime}ms`)
  console.log(`Total cost: $${totalCost.toFixed(4)}`)

  session.stop()
}

// ============================================================================
// Example 5: Parallel Processing (with multiple sessions)
// ============================================================================

async function example5_ParallelProcessing() {
  console.log('\n\nExample 5: Parallel Processing\n')

  // Create multiple sessions
  const createSession = (id: number) => {
    return new ClaudeSession({
      model: 'haiku',
      onText: (chunk) => {
        process.stdout.write(`[S${id}] ${chunk.text}`)
      }
    })
  }

  const session1 = createSession(1)
  const session2 = createSession(2)

  session1.start()
  session2.start()


  console.log('>>> Starting parallel questions...\n')

  // Ask questions in parallel
  const [result1, result2] = await Promise.all([
    session1.ask('What is 2+2?'),
    session2.ask('What is 3+3?')
  ])

  console.log(`\n\n[Session 1: $${result1.usage.costUSD.toFixed(4)}]`)
  console.log(`[Session 2: $${result2.usage.costUSD.toFixed(4)}]`)

  session1.stop()
  session2.stop()
}

// ============================================================================
// Example 6: waitForTurn for Manual Message Sending
// ============================================================================

async function example6_ManualWaitForTurn() {
  console.log('\n\nExample 6: Manual waitForTurn\n')

  const session = new ClaudeSession({
    model: 'haiku',
    onText: (chunk) => process.stdout.write(chunk.text)
  })

  session.start()

  // Manually send message
  console.log('>>> Sending message manually')
  session.sendMessage('What is 2+2?')

  // Wait for this turn to complete
  const result = await session.waitForTurn()
  console.log(`\n[Turn ${result.turnNumber} complete: $${result.usage.costUSD.toFixed(4)}]`)

  // Send another
  console.log('\n>>> Another manual message')
  session.sendMessage('What is 3+3?')

  // Wait for turn 2 specifically
  const result2 = await session.waitForTurn(2)
  console.log(`\n[Turn 2 complete: $${result2.usage.costUSD.toFixed(4)}]`)

  session.stop()
}

// ============================================================================
// Example 7: Timeout Handling
// ============================================================================

async function example7_TimeoutHandling() {
  console.log('\n\nExample 7: Timeout Handling\n')

  const session = new ClaudeSession({
    model: 'haiku',
    onText: (chunk) => process.stdout.write(chunk.text)
  })

  session.start()

  try {
    console.log('>>> Question with 1s timeout (likely to fail)')
    await session.ask('Explain quantum computing in detail', 1000) // Very short timeout
    console.log('\n[Completed within timeout]')
  } catch (error) {
    console.log(`\n[Timeout: ${error instanceof Error ? error.message : error}]`)
  }

  // Try again with reasonable timeout
  try {
    console.log('\n>>> Same question with 30s timeout')
    const result = await session.ask('Explain quantum computing', 30000)
    console.log(`\n[Success: $${result.usage.costUSD.toFixed(4)}]`)
  } catch (error) {
    console.log(`\n[Failed: ${error instanceof Error ? error.message : error}]`)
  }

  session.stop()
}

// ============================================================================
// Example 8: Combining Imperative and Event-Driven
// ============================================================================

async function example8_HybridApproach() {
  console.log('\n\nExample 8: Hybrid Approach\n')

  let fullText = ''

  const session = new ClaudeSession({
    model: 'haiku',

    // Event-driven for streaming
    onText: (chunk) => {
      fullText = chunk.fullText
      process.stdout.write(chunk.text)
    },

    onToolComplete: (tool) => {
      console.log(`\n[Tool: ${tool.name}]`)
      session.sendToolResults([{
        toolUseId: tool.id,
        content: 'Mock result',
        isError: false
      }])
    }
  })

  session.start()

  // Imperative for flow control
  console.log('>>> Question')
  const result = await session.ask('What files are here?')

  console.log(`\n\n=== Analysis ===`)
  console.log(`Final text length: ${fullText.length} characters`)
  console.log(`Duration: ${result.durationMs}ms`)
  console.log(`Cost: $${result.usage.costUSD.toFixed(4)}`)

  session.stop()
}

// ============================================================================
// Run Examples
// ============================================================================

const exampleNum = parseInt(process.argv[2] || '1')

async function main() {
  try {
    switch (exampleNum) {
      case 1:
        await example1_SimpleImperative()
        break
      case 2:
        await example2_ErrorHandling()
        break
      case 3:
        await example3_ConditionalWorkflow()
        break
      case 4:
        await example4_SequentialProcessing()
        break
      case 5:
        await example5_ParallelProcessing()
        break
      case 6:
        await example6_ManualWaitForTurn()
        break
      case 7:
        await example7_TimeoutHandling()
        break
      case 8:
        await example8_HybridApproach()
        break
      default:
        console.log('Usage: tsx imperative-example.ts [1-8]')
        console.log('Examples:')
        console.log('  1 - Simple Imperative Conversation')
        console.log('  2 - Error Handling')
        console.log('  3 - Conditional Workflow')
        console.log('  4 - Sequential Processing')
        console.log('  5 - Parallel Processing')
        console.log('  6 - Manual waitForTurn')
        console.log('  7 - Timeout Handling')
        console.log('  8 - Hybrid Approach')
    }
  } catch (error) {
    console.error('\nFatal error:', error)
    process.exit(1)
  }
}

main()
