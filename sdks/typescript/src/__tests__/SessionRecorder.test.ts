/**
 * Unit tests for SessionRecorder
 */

import { describe, it, expect, beforeEach, afterEach } from 'bun:test'
import { SessionRecorder, RecordedMessage, SessionRecording } from '../SessionRecorder'
import * as fs from 'fs/promises'
import * as path from 'path'
import { tmpdir } from 'os'

describe('SessionRecorder', () => {
  let recorder: SessionRecorder
  let testDir: string

  beforeEach(async () => {
    // Create temp directory for test files
    testDir = path.join(tmpdir(), `session-recorder-test-${Date.now()}`)
    await fs.mkdir(testDir, { recursive: true })

    recorder = new SessionRecorder({
      saveDir: testDir
    })
  })

  afterEach(async () => {
    // Clean up test directory
    try {
      await fs.rm(testDir, { recursive: true, force: true })
    } catch (error) {
      // Ignore cleanup errors
    }
  })

  describe('Initialization', () => {
    it('should initialize with session info and create directory', async () => {
      await recorder.initialize({
        sessionId: 'session_123',
        model: 'haiku',
        cwd: '/test/dir',
        tools: ['Read', 'Write', 'Bash'],
        claudeCodeVersion: '1.0.0',
        permissionMode: 'default'
      })

      const recording = recorder.getRecording()

      expect(recording.metadata.sessionId).toBe('session_123')
      expect(recording.metadata.model).toBe('haiku')
      expect(recording.metadata.cwd).toBe('/test/dir')
      expect(recording.metadata.tools).toEqual(['Read', 'Write', 'Bash'])
      expect(recording.metadata.claudeCodeVersion).toBe('1.0.0')
      expect(recording.metadata.permissionMode).toBe('default')

      // Check that session directory was created
      const sessionDir = recorder.getRecordingPath()
      expect(sessionDir).toBeDefined()
      const stat = await fs.stat(sessionDir!)
      expect(stat.isDirectory()).toBe(true)
    })

    it('should write metadata file immediately on initialization', async () => {
      await recorder.initialize({
        sessionId: 'session_meta',
        model: 'haiku',
        cwd: '/test',
        tools: ['Read'],
        claudeCodeVersion: '1.0.0',
        permissionMode: 'default'
      })

      const sessionDir = recorder.getRecordingPath()
      const metaPath = path.join(sessionDir!, 'meta.json')

      // Check metadata file exists
      const stat = await fs.stat(metaPath)
      expect(stat.isFile()).toBe(true)

      // Verify metadata content
      const metaContent = await fs.readFile(metaPath, 'utf-8')
      const metadata = JSON.parse(metaContent)

      expect(metadata.sessionId).toBe('session_meta')
      expect(metadata.model).toBe('haiku')
      expect(metadata.tools).toEqual(['Read'])
      expect(metadata.claudeCodeVersion).toBe('1.0.0')
    })

    it('should only initialize once', async () => {
      await recorder.initialize({
        sessionId: 'session_first',
        model: 'haiku',
        cwd: '/first',
        tools: []
      })

      const firstDir = recorder.getRecordingPath()

      await recorder.initialize({
        sessionId: 'session_second',
        model: 'sonnet',
        cwd: '/second',
        tools: []
      })

      const secondDir = recorder.getRecordingPath()

      // Should be the same directory (no re-initialization)
      expect(secondDir).toBe(firstDir)

      const recording = recorder.getRecording()
      expect(recording.metadata.sessionId).toBe('session_first')
      expect(recording.metadata.model).toBe('haiku')
    })
  })

  describe('Message Recording - Streaming', () => {
    beforeEach(async () => {
      await recorder.initialize({
        sessionId: 'test_session',
        model: 'haiku',
        cwd: '/test',
        tools: []
      })
    })

    it('should append sent messages to to_cli.jsonl', async () => {
      const userMessage = {
        type: 'user' as const,
        message: {
          role: 'user' as const,
          content: 'Hello!'
        }
      }

      recorder.recordSent(userMessage, 1)

      // Wait a moment for async file write
      await new Promise(resolve => setTimeout(resolve, 50))

      const sessionDir = recorder.getRecordingPath()
      const toCliPath = path.join(sessionDir!, 'to_cli.jsonl')

      const content = await fs.readFile(toCliPath, 'utf-8')
      const lines = content.trim().split('\n')

      expect(lines.length).toBe(1)

      const recorded = JSON.parse(lines[0])
      expect(recorded.direction).toBe('sent')
      expect(recorded.turnNumber).toBe(1)
      expect(recorded.message).toEqual(userMessage)
    })

    it('should append received messages to from_cli.jsonl', async () => {
      const assistantMessage = {
        type: 'assistant' as const,
        message: {
          model: 'haiku',
          id: 'msg_123',
          type: 'message' as const,
          role: 'assistant' as const,
          content: [{ type: 'text' as const, text: 'Hi there!' }],
          stop_reason: null,
          stop_sequence: null,
          usage: {} as any
        },
        parent_tool_use_id: null,
        session_id: 'test_session',
        uuid: 'uuid_123'
      }

      recorder.recordReceived(assistantMessage, 1)

      // Wait a moment for async file write
      await new Promise(resolve => setTimeout(resolve, 50))

      const sessionDir = recorder.getRecordingPath()
      const fromCliPath = path.join(sessionDir!, 'from_cli.jsonl')

      const content = await fs.readFile(fromCliPath, 'utf-8')
      const lines = content.trim().split('\n')

      expect(lines.length).toBe(1)

      const recorded = JSON.parse(lines[0])
      expect(recorded.direction).toBe('received')
      expect(recorded.message).toEqual(assistantMessage)
    })

    it('should append multiple messages to separate files', async () => {
      recorder.recordSent({ type: 'user', message: { role: 'user', content: 'msg1' } }, 1)
      recorder.recordSent({ type: 'user', message: { role: 'user', content: 'msg2' } }, 1)
      recorder.recordReceived({
        type: 'assistant',
        message: {} as any,
        parent_tool_use_id: null,
        session_id: 'test',
        uuid: 'uuid1'
      }, 1)

      // Wait for async writes
      await new Promise(resolve => setTimeout(resolve, 50))

      const sessionDir = recorder.getRecordingPath()
      const toCliPath = path.join(sessionDir!, 'to_cli.jsonl')
      const fromCliPath = path.join(sessionDir!, 'from_cli.jsonl')

      const toCliContent = await fs.readFile(toCliPath, 'utf-8')
      const fromCliContent = await fs.readFile(fromCliPath, 'utf-8')

      expect(toCliContent.trim().split('\n').length).toBe(2)
      expect(fromCliContent.trim().split('\n').length).toBe(1)
    })
  })

  describe('Turn Tracking', () => {
    beforeEach(async () => {
      await recorder.initialize({
        sessionId: 'test_session',
        model: 'haiku',
        cwd: '/test',
        tools: []
      })
    })

    it('should start turn tracking', () => {
      recorder.startTurn(1, 'Hello!')

      const recording = recorder.getRecording()
      expect(recording.turns).toHaveLength(1)
      expect(recording.turns[0].turnNumber).toBe(1)
      expect(recording.turns[0].userMessage).toBe('Hello!')
      expect(recording.turns[0].assistantText).toBe('')
      expect(recording.turns[0].toolsUsed).toEqual([])
    })

    it('should update turn text incrementally', () => {
      recorder.startTurn(1, 'Hello!')

      recorder.updateTurnText(1, 'Let me')
      recorder.updateTurnText(1, 'Let me help')
      recorder.updateTurnText(1, 'Let me help you')

      const recording = recorder.getRecording()
      expect(recording.turns[0].assistantText).toBe('Let me help you')
    })

    it('should track tools used in turn', () => {
      recorder.startTurn(1, 'Read file')

      recorder.addToolToTurn(1, 'Read')
      recorder.addToolToTurn(1, 'Glob')

      const recording = recorder.getRecording()
      expect(recording.turns[0].toolsUsed).toEqual(['Read', 'Glob'])
    })

    it('should not duplicate tools in turn', () => {
      recorder.startTurn(1, 'Test')

      recorder.addToolToTurn(1, 'Read')
      recorder.addToolToTurn(1, 'Read')
      recorder.addToolToTurn(1, 'Read')

      const recording = recorder.getRecording()
      expect(recording.turns[0].toolsUsed).toEqual(['Read'])
    })

    it('should complete turn with stats and update metadata', async () => {
      recorder.startTurn(1, 'Test')

      recorder.completeTurn(1, {
        duration: 1500,
        cost: 0.0025,
        tokens: { input: 100, output: 200 }
      })

      // Wait for async metadata write
      await new Promise(resolve => setTimeout(resolve, 50))

      const recording = recorder.getRecording()
      expect(recording.turns[0].duration).toBe(1500)
      expect(recording.turns[0].cost).toBe(0.0025)
      expect(recording.turns[0].tokens).toEqual({ input: 100, output: 200 })
      expect(recording.turns[0].endTime).toBeDefined()

      // Check that metadata file was updated
      const sessionDir = recorder.getRecordingPath()
      const metaPath = path.join(sessionDir!, 'meta.json')
      const metaContent = await fs.readFile(metaPath, 'utf-8')
      const metadata = JSON.parse(metaContent)

      expect(metadata.turns).toHaveLength(1)
      expect(metadata.turns[0].duration).toBe(1500)
    })
  })

  describe('Deprecated Methods', () => {
    beforeEach(async () => {
      await recorder.initialize({
        sessionId: 'test_session',
        model: 'haiku',
        cwd: '/test',
        tools: []
      })
    })

    it('should return empty array for getMessages (deprecated)', () => {
      recorder.recordSent({ type: 'user', message: { role: 'user', content: 'test' } }, 1)

      const messages = recorder.getMessages()
      expect(messages).toEqual([])
    })

    it('should return empty array for getMessagesByTurn (deprecated)', () => {
      recorder.recordSent({ type: 'user', message: { role: 'user', content: 'test' } }, 1)

      const messages = recorder.getMessagesByTurn(1)
      expect(messages).toEqual([])
    })

    it('should return session directory for save (deprecated)', async () => {
      const result = await recorder.save()
      expect(result).toBe(recorder.getRecordingPath())
    })
  })

  describe('Summary Statistics', () => {
    beforeEach(async () => {
      await recorder.initialize({
        sessionId: 'test_session',
        model: 'haiku',
        cwd: '/test',
        tools: []
      })
    })

    it('should calculate summary statistics', () => {
      // Record 3 turns
      for (let i = 1; i <= 3; i++) {
        recorder.startTurn(i, `Message ${i}`)
        recorder.recordSent({ type: 'user', message: { role: 'user', content: `test${i}` } }, i)
        recorder.recordReceived({
          type: 'assistant',
          message: {} as any,
          parent_tool_use_id: null,
          session_id: 'test',
          uuid: `uuid${i}`
        }, i)
        recorder.completeTurn(i, {
          duration: 1000,
          cost: 0.001,
          tokens: { input: 50, output: 100 }
        })
      }

      const summary = recorder.getSummary()

      expect(summary.totalMessages).toBe(6) // 3 sent + 3 received
      expect(summary.sentMessages).toBe(3)
      expect(summary.receivedMessages).toBe(3)
      expect(summary.totalTurns).toBe(3)
      expect(summary.totalCost).toBe(0.003)
      expect(summary.totalTokens.input).toBe(150)
      expect(summary.totalTokens.output).toBe(300)
    })

    it('should calculate duration', () => {
      const summary = recorder.getSummary()
      expect(summary.duration).toBeGreaterThanOrEqual(0)
    })
  })

  describe('File Loading', () => {
    let sessionDir: string

    beforeEach(async () => {
      await recorder.initialize({
        sessionId: 'load_test',
        model: 'haiku',
        cwd: '/test',
        tools: ['Read']
      })

      recorder.startTurn(1, 'Hello')
      recorder.recordSent({ type: 'user', message: { role: 'user', content: 'test' } }, 1)
      recorder.updateTurnText(1, 'Response text')
      recorder.addToolToTurn(1, 'Read')
      recorder.completeTurn(1, {
        duration: 1500,
        cost: 0.002,
        tokens: { input: 75, output: 150 }
      })

      // Wait for async writes
      await new Promise(resolve => setTimeout(resolve, 100))

      sessionDir = recorder.getRecordingPath()!
    })

    it('should load recording from directory', async () => {
      const loaded = await SessionRecorder.load(sessionDir)

      expect(loaded.metadata.sessionId).toBe('load_test')
      expect(loaded.metadata.model).toBe('haiku')
      expect(loaded.metadata.totalTurns).toBe(1)
      expect(loaded.messages).toHaveLength(1)
      expect(loaded.turns).toHaveLength(1)
      expect(loaded.turns[0].assistantText).toBe('Response text')
      expect(loaded.turns[0].toolsUsed).toEqual(['Read'])
      expect(loaded.turns[0].cost).toBe(0.002)
    })

    it('should restore Date objects correctly', async () => {
      const loaded = await SessionRecorder.load(sessionDir)

      expect(loaded.metadata.startTime).toBeInstanceOf(Date)
      expect(loaded.metadata.endTime).toBeInstanceOf(Date)
      expect(loaded.messages[0].timestamp).toBeInstanceOf(Date)
      expect(loaded.turns[0].startTime).toBeInstanceOf(Date)
      expect(loaded.turns[0].endTime).toBeInstanceOf(Date)
    })

    it('should load messages from both to_cli and from_cli files', async () => {
      // Add another received message
      recorder.recordReceived({
        type: 'assistant',
        message: {} as any,
        parent_tool_use_id: null,
        session_id: 'test',
        uuid: 'uuid2'
      }, 1)

      // Wait for write
      await new Promise(resolve => setTimeout(resolve, 50))

      const loaded = await SessionRecorder.load(sessionDir)

      // Should have 1 sent + 1 received = 2 messages
      expect(loaded.messages).toHaveLength(2)
      expect(loaded.messages.some(m => m.direction === 'sent')).toBe(true)
      expect(loaded.messages.some(m => m.direction === 'received')).toBe(true)

      // Messages should be sorted by timestamp
      for (let i = 1; i < loaded.messages.length; i++) {
        expect(loaded.messages[i].timestamp.getTime())
          .toBeGreaterThanOrEqual(loaded.messages[i - 1].timestamp.getTime())
      }
    })
  })

  describe('Clear', () => {
    it('should clear all recorded data', async () => {
      await recorder.initialize({
        sessionId: 'clear_test',
        model: 'haiku',
        cwd: '/test',
        tools: []
      })

      recorder.startTurn(1, 'test')
      recorder.recordSent({ type: 'user', message: { role: 'user', content: 'test' } }, 1)

      recorder.clear()

      expect(recorder.getSummary().totalTurns).toBe(0)
      expect(recorder.getSummary().totalMessages).toBe(0)
    })
  })

})
