/**
 * MessageDiscriminator tests using LIVE CLI traces
 */

import { describe, it, expect, beforeAll } from 'bun:test'
import { MessageDiscriminator } from '../MessageDiscriminator'
import * as fs from 'fs'
import * as path from 'path'

describe('MessageDiscriminator - Live CLI', () => {
  let simpleTrace: any
  let streamingTrace: any
  let multiTurnTrace: any

  beforeAll(() => {
    // Use absolute path from project root
    const tracesDir = '/Users/ming/g/agent-editor/traces'
    simpleTrace = JSON.parse(fs.readFileSync(path.join(tracesDir, 'simple-text-query.json'), 'utf-8'))
    streamingTrace = JSON.parse(fs.readFileSync(path.join(tracesDir, 'streaming-partial.json'), 'utf-8'))
    multiTurnTrace = JSON.parse(fs.readFileSync(path.join(tracesDir, 'multi-turn.json'), 'utf-8'))
  })

  describe('SystemMessage discrimination', () => {
    it('should identify hook_response from simple trace', () => {
      const hookMsg = simpleTrace.messages_received[0]

      expect(MessageDiscriminator.discriminate(hookMsg)).toBe('system')
      expect(MessageDiscriminator.isSystemMessage(hookMsg)).toBe(true)
      expect(hookMsg.type).toBe('system')
      expect(hookMsg.subtype).toBe('hook_response')
    })

    it('should identify init message from simple trace', () => {
      const initMsg = simpleTrace.messages_received[1]

      expect(MessageDiscriminator.discriminate(initMsg)).toBe('system')
      expect(MessageDiscriminator.isSystemMessage(initMsg)).toBe(true)
      expect(initMsg.type).toBe('system')
      expect(initMsg.subtype).toBe('init')
      expect(initMsg.session_id).toBeDefined()
      expect(initMsg.tools).toBeDefined()
      expect(initMsg.model).toBeDefined()
    })
  })

  describe('AssistantMessage discrimination', () => {
    it('should identify assistant message from simple trace', () => {
      const assistantMsg = simpleTrace.messages_received[2]

      expect(MessageDiscriminator.discriminate(assistantMsg)).toBe('assistant')
      expect(MessageDiscriminator.isAssistantMessage(assistantMsg)).toBe(true)
      expect(assistantMsg.type).toBe('assistant')
      expect(assistantMsg.message).toBeDefined()
      expect(assistantMsg.message.model).toBeDefined()
      expect(assistantMsg.message.content).toBeDefined()
      expect(Array.isArray(assistantMsg.message.content)).toBe(true)
    })

    it('should verify assistant message structure', () => {
      const assistantMsg = simpleTrace.messages_received[2]

      // Check wrapper structure
      expect(assistantMsg.message.model).toContain('claude')
      expect(assistantMsg.message.role).toBe('assistant')
      expect(assistantMsg.message.content.length).toBeGreaterThan(0)

      // Check content block
      const firstBlock = assistantMsg.message.content[0]
      expect(firstBlock.type).toBe('text')
      expect(firstBlock.text).toBeDefined()
    })
  })

  describe('ResultMessage discrimination', () => {
    it('should identify result message from simple trace', () => {
      const resultMsg = simpleTrace.messages_received[3]

      expect(MessageDiscriminator.discriminate(resultMsg)).toBe('result')
      expect(MessageDiscriminator.isResultMessage(resultMsg)).toBe(true)
      expect(resultMsg.type).toBe('result')
      expect(resultMsg.subtype).toBe('success')
      expect(resultMsg.total_cost_usd).toBeGreaterThan(0)
      expect(resultMsg.usage).toBeDefined()
    })

    it('should verify result message fields', () => {
      const resultMsg = simpleTrace.messages_received[3]

      expect(resultMsg.duration_ms).toBeGreaterThan(0)
      expect(resultMsg.num_turns).toBe(1)
      expect(resultMsg.session_id).toBeDefined()
      expect(resultMsg.modelUsage).toBeDefined()
    })
  })

  describe('StreamEvent discrimination', () => {
    it('should identify stream events from streaming trace', () => {
      const streamEvents = streamingTrace.messages_received.filter(
        (msg: any) => msg.type === 'stream_event'
      )

      expect(streamEvents.length).toBeGreaterThan(0)

      for (const event of streamEvents) {
        expect(MessageDiscriminator.discriminate(event)).toBe('stream_event')
        expect(MessageDiscriminator.isStreamEvent(event)).toBe(true)
        expect(event.event).toBeDefined()
        expect(event.event.type).toBeDefined()
      }
    })

    it('should identify different stream event types', () => {
      const streamEvents = streamingTrace.messages_received.filter(
        (msg: any) => msg.type === 'stream_event'
      )

      const eventTypes = new Set(streamEvents.map((e: any) => e.event.type))

      expect(eventTypes.has('message_start')).toBe(true)
      expect(eventTypes.has('content_block_start')).toBe(true)
      expect(eventTypes.has('content_block_delta')).toBe(true)
      expect(eventTypes.has('content_block_stop')).toBe(true)
      expect(eventTypes.has('message_stop')).toBe(true)
    })
  })

  describe('Multi-turn conversation', () => {
    it('should identify all message types in multi-turn trace', () => {
      const messages = multiTurnTrace.messages_received
      const types = new Set(messages.map((msg: any) => MessageDiscriminator.discriminate(msg)))

      expect(types.has('system')).toBe(true)
      expect(types.has('assistant')).toBe(true)
      expect(types.has('result')).toBe(true)
    })

    it('should have multiple init messages in multi-turn', () => {
      const initMessages = multiTurnTrace.messages_received.filter(
        (msg: any) => msg.type === 'system' && msg.subtype === 'init'
      )

      // Each turn gets its own init
      expect(initMessages.length).toBeGreaterThanOrEqual(2)
    })
  })

  describe('Unknown message handling', () => {
    it('should return unknown for invalid messages', () => {
      expect(MessageDiscriminator.discriminate(null)).toBe('unknown')
      expect(MessageDiscriminator.discriminate(undefined)).toBe('unknown')
      expect(MessageDiscriminator.discriminate(42)).toBe('unknown')
      expect(MessageDiscriminator.discriminate('string')).toBe('unknown')
      expect(MessageDiscriminator.discriminate({})).toBe('unknown')
      expect(MessageDiscriminator.discriminate({ random: 'field' })).toBe('unknown')
    })
  })

  describe('All traces validation', () => {
    it('should correctly identify all messages across all traces', () => {
      const allTraces = [simpleTrace, streamingTrace, multiTurnTrace]

      for (const trace of allTraces) {
        for (const msg of trace.messages_received) {
          const type = MessageDiscriminator.discriminate(msg)

          // Should never be unknown for valid CLI output
          expect(type).not.toBe('unknown')

          // Verify it has the type field
          expect(msg.type).toBeDefined()

          // Verify discriminator matches the type field
          expect(type).toBe(msg.type)
        }
      }
    })
  })
})
