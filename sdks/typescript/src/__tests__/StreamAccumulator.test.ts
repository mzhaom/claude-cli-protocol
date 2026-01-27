/**
 * Unit tests for StreamAccumulator
 */

import { describe, it, expect, beforeEach, mock, spyOn } from 'bun:test'
import { StreamAccumulator } from '../StreamAccumulator'
import { StreamEvent, AssistantMessage } from '../types'

describe('StreamAccumulator', () => {
  let accumulator: StreamAccumulator

  beforeEach(() => {
    accumulator = new StreamAccumulator()
  })

  describe('Event Registration', () => {
    it('should register event handlers', () => {
      const handler = mock(() => {})
      accumulator.on('text_chunk', handler)

      // No direct way to test, but shouldn't throw
      expect(() => accumulator.on('text_chunk', handler)).not.toThrow()
    })

    it('should unregister event handlers', () => {
      const handler = mock(() => {})
      accumulator.on('text_chunk', handler)
      accumulator.off('text_chunk', handler)

      // No direct way to test, but shouldn't throw
      expect(() => accumulator.off('text_chunk', handler)).not.toThrow()
    })
  })

  describe('Text Block Accumulation', () => {
    it('should accumulate text deltas', () => {
      const textChunks: any[] = []
      accumulator.on('text_chunk', (data) => textChunks.push(data))

      // message_start
      accumulator.handleEvent(createStreamEvent('message_start', {
        message: {
          id: 'msg_123',
          model: 'claude-3-haiku-20240307',
          type: 'message',
          role: 'assistant',
          content: [],
          stop_reason: null,
          stop_sequence: null,
          usage: {
            input_tokens: 0,
            cache_creation_input_tokens: 0,
            cache_read_input_tokens: 0,
            cache_creation: {
              ephemeral_5m_input_tokens: 0,
              ephemeral_1h_input_tokens: 0
            },
            output_tokens: 0,
            service_tier: 'standard'
          }
        }
      }))

      // content_block_start (text)
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 0,
        content_block: {
          type: 'text',
          text: ''
        }
      }))

      // content_block_delta (text)
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: {
          type: 'text_delta',
          text: 'Hello'
        }
      }))

      expect(textChunks).toHaveLength(1)
      expect(textChunks[0]).toEqual({
        index: 0,
        text: 'Hello'
      })

      // Another delta
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: {
          type: 'text_delta',
          text: ' world'
        }
      }))

      expect(textChunks).toHaveLength(2)
      expect(textChunks[1]).toEqual({
        index: 0,
        text: ' world'
      })
    })

    it('should emit block_complete with accumulated text', () => {
      const completeBlocks: any[] = []
      accumulator.on('block_complete', (data) => completeBlocks.push(data))

      // Start message
      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      // Start text block
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 0,
        content_block: {
          type: 'text',
          text: ''
        }
      }))

      // Add deltas
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'text_delta', text: 'Hello' }
      }))

      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'text_delta', text: ' world!' }
      }))

      // Complete block
      accumulator.handleEvent(createStreamEvent('content_block_stop', {
        index: 0
      }))

      expect(completeBlocks).toHaveLength(1)
      expect(completeBlocks[0]).toEqual({
        index: 0,
        block: {
          type: 'text',
          text: 'Hello world!'
        }
      })
    })
  })

  describe('Thinking Block Accumulation', () => {
    it('should accumulate thinking deltas', () => {
      // Start message
      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      // Start thinking block
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 0,
        content_block: {
          type: 'thinking',
          thinking: ''
        }
      }))

      // Add thinking deltas
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'thinking_delta', thinking: 'Let me think...' }
      }))

      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'thinking_delta', thinking: ' about this problem.' }
      }))

      // Complete block
      const completeBlocks: any[] = []
      accumulator.on('block_complete', (data) => completeBlocks.push(data))

      accumulator.handleEvent(createStreamEvent('content_block_stop', {
        index: 0
      }))

      expect(completeBlocks).toHaveLength(1)
      expect(completeBlocks[0].block).toEqual({
        type: 'thinking',
        thinking: 'Let me think... about this problem.'
      })
    })
  })

  describe('Tool Use Block Accumulation', () => {
    it('should accumulate tool input JSON', () => {
      const toolChunks: any[] = []
      accumulator.on('tool_input_chunk', (data) => toolChunks.push(data))

      // Start message
      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      // Start tool block
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 0,
        content_block: {
          type: 'tool_use',
          id: 'tool_123',
          name: 'Read'
        }
      }))

      // Add input JSON deltas
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'input_json_delta', partial_json: '{"file' }
      }))

      expect(toolChunks).toHaveLength(1)
      expect(toolChunks[0].chunk).toBe('{"file')

      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'input_json_delta', partial_json: '_path": "/foo' }
      }))

      expect(toolChunks).toHaveLength(2)

      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'input_json_delta', partial_json: '.txt"}' }
      }))

      expect(toolChunks).toHaveLength(3)
    })

    it('should emit block_complete with parsed tool input', () => {
      const completeBlocks: any[] = []
      accumulator.on('block_complete', (data) => completeBlocks.push(data))

      // Start message
      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      // Start tool block
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 0,
        content_block: {
          type: 'tool_use',
          id: 'tool_123',
          name: 'Read'
        }
      }))

      // Add complete JSON
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'input_json_delta', partial_json: '{"file_path": "/foo.txt"}' }
      }))

      // Complete block
      accumulator.handleEvent(createStreamEvent('content_block_stop', {
        index: 0
      }))

      expect(completeBlocks).toHaveLength(1)
      expect(completeBlocks[0].block).toEqual({
        type: 'tool_use',
        id: 'tool_123',
        name: 'Read',
        input: {
          file_path: '/foo.txt'
        }
      })
    })
  })

  describe('Multiple Blocks', () => {
    it('should handle multiple content blocks in order', () => {
      const completeBlocks: any[] = []
      accumulator.on('block_complete', (data) => completeBlocks.push(data))

      // Start message
      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      // Block 0: Text
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 0,
        content_block: { type: 'text', text: '' }
      }))
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'text_delta', text: 'Let me help' }
      }))
      accumulator.handleEvent(createStreamEvent('content_block_stop', { index: 0 }))

      // Block 1: Tool
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 1,
        content_block: { type: 'tool_use', id: 'tool_1', name: 'Read' }
      }))
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 1,
        delta: { type: 'input_json_delta', partial_json: '{"file_path": "/test.txt"}' }
      }))
      accumulator.handleEvent(createStreamEvent('content_block_stop', { index: 1 }))

      // Block 2: Text
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 2,
        content_block: { type: 'text', text: '' }
      }))
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 2,
        delta: { type: 'text_delta', text: 'Done!' }
      }))
      accumulator.handleEvent(createStreamEvent('content_block_stop', { index: 2 }))

      expect(completeBlocks).toHaveLength(3)
      expect(completeBlocks[0].index).toBe(0)
      expect(completeBlocks[0].block.type).toBe('text')
      expect(completeBlocks[1].index).toBe(1)
      expect(completeBlocks[1].block.type).toBe('tool_use')
      expect(completeBlocks[2].index).toBe(2)
      expect(completeBlocks[2].block.type).toBe('text')
    })
  })

  describe('Message Completion', () => {
    it('should emit message_complete with all blocks', () => {
      const messages: AssistantMessage[] = []
      accumulator.on('message_complete', (msg) => messages.push(msg))

      // Start message
      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      // Add text block
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 0,
        content_block: { type: 'text', text: '' }
      }))
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 0,
        delta: { type: 'text_delta', text: 'Hello!' }
      }))
      accumulator.handleEvent(createStreamEvent('content_block_stop', { index: 0 }))

      // Message stop
      accumulator.handleEvent(createStreamEvent('message_stop', {}))

      expect(messages).toHaveLength(1)
      expect(messages[0].type).toBe('assistant')
      expect(messages[0].message.content).toHaveLength(1)
      expect(messages[0].message.content[0]).toEqual({
        type: 'text',
        text: 'Hello!'
      })
    })

    it('should reset accumulator after message_complete', () => {
      accumulator.on('message_complete', () => {})

      // Complete first message
      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))
      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 0,
        content_block: { type: 'text', text: '' }
      }))
      accumulator.handleEvent(createStreamEvent('content_block_stop', { index: 0 }))
      accumulator.handleEvent(createStreamEvent('message_stop', {}))

      // Check that accumulator is reset
      const blocks = accumulator.getBlocks()
      expect(blocks.size).toBe(0)
    })
  })

  describe('Message Delta', () => {
    it('should emit message_meta with usage info', () => {
      const metaEvents: any[] = []
      accumulator.on('message_meta', (data) => metaEvents.push(data))

      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      accumulator.handleEvent(createStreamEvent('message_delta', {
        delta: {
          stop_reason: 'end_turn',
          stop_sequence: null
        },
        usage: {
          input_tokens: 100,
          cache_creation_input_tokens: 0,
          cache_read_input_tokens: 50,
          output_tokens: 200
        }
      }))

      expect(metaEvents).toHaveLength(1)
      expect(metaEvents[0].stopReason).toBe('end_turn')
      expect(metaEvents[0].usage.input_tokens).toBe(100)
      expect(metaEvents[0].usage.output_tokens).toBe(200)
    })
  })

  describe('Edge Cases', () => {
    it('should handle delta for unknown block gracefully', () => {
      const originalWarn = console.warn
      const warnCalls: string[] = []
      console.warn = (...args: any[]) => { warnCalls.push(args.join(' ')) }

      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      // Delta for block that doesn't exist
      accumulator.handleEvent(createStreamEvent('content_block_delta', {
        index: 99,
        delta: { type: 'text_delta', text: 'test' }
      }))

      expect(warnCalls.some(msg => msg.includes('unknown block 99'))).toBe(true)

      console.warn = originalWarn
    })

    it('should handle stop for unknown block gracefully', () => {
      const originalWarn = console.warn
      const warnCalls: string[] = []
      console.warn = (...args: any[]) => { warnCalls.push(args.join(' ')) }

      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      accumulator.handleEvent(createStreamEvent('content_block_stop', { index: 99 }))

      expect(warnCalls.some(msg => msg.includes('unknown block 99'))).toBe(true)

      console.warn = originalWarn
    })
  })

  describe('reset', () => {
    it('should clear all accumulated state', () => {
      accumulator.handleEvent(createStreamEvent('message_start', {
        message: createEmptyMessage()
      }))

      accumulator.handleEvent(createStreamEvent('content_block_start', {
        index: 0,
        content_block: { type: 'text', text: '' }
      }))

      expect(accumulator.getBlocks().size).toBe(1)

      accumulator.reset()

      expect(accumulator.getBlocks().size).toBe(0)
    })
  })
})

// ============================================================================
// Test Helpers
// ============================================================================

function createStreamEvent(type: string, data: any): StreamEvent {
  return {
    type: 'stream_event',
    event: {
      type,
      ...data
    } as any,
    session_id: 'session_123',
    uuid: 'uuid_123',
    parent_tool_use_id: null
  }
}

function createEmptyMessage(): any {
  return {
    id: 'msg_123',
    model: 'claude-3-haiku-20240307',
    type: 'message',
    role: 'assistant',
    content: [],
    stop_reason: null,
    stop_sequence: null,
    usage: {
      input_tokens: 0,
      cache_creation_input_tokens: 0,
      cache_read_input_tokens: 0,
      cache_creation: {
        ephemeral_5m_input_tokens: 0,
        ephemeral_1h_input_tokens: 0
      },
      output_tokens: 0,
      service_tier: 'standard'
    }
  }
}
