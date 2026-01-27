/**
 * StreamAccumulator - Accumulates streaming events into complete messages
 * Updated for Live CLI format
 */

import {
  StreamEvent,
  ContentBlock,
  AssistantMessage,
  TextBlock,
  ThinkingBlock,
  ToolUseBlock
} from './types'

/**
 * Internal accumulator for a single content block
 */
interface ContentBlockAccumulator {
  type: 'text' | 'thinking' | 'tool_use'
  id?: string
  name?: string
  accumulated: string
}

/**
 * Events emitted by the StreamAccumulator
 */
export interface StreamAccumulatorEvents {
  text_chunk: { index: number; text: string }
  tool_input_chunk: { index: number; chunk: string }
  block_complete: { index: number; block: ContentBlock }
  message_meta: {
    stopReason: string | null
    usage: {
      input_tokens: number
      cache_creation_input_tokens: number
      cache_read_input_tokens: number
      output_tokens: number
    }
  }
  message_complete: AssistantMessage
}

export type StreamAccumulatorEventType = keyof StreamAccumulatorEvents
export type StreamAccumulatorEventHandler<T extends StreamAccumulatorEventType> = (
  data: StreamAccumulatorEvents[T]
) => void

/**
 * Accumulates streaming events into complete messages
 */
export class StreamAccumulator {
  private blocks: Map<number, ContentBlockAccumulator> = new Map()
  private messageId = ''
  private model = ''
  private sessionId = ''
  private uuid = ''
  private listeners: Map<StreamAccumulatorEventType, Set<StreamAccumulatorEventHandler<any>>> =
    new Map()

  /**
   * Register an event listener
   */
  on<T extends StreamAccumulatorEventType>(
    event: T,
    handler: StreamAccumulatorEventHandler<T>
  ): void {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, new Set())
    }
    this.listeners.get(event)!.add(handler)
  }

  /**
   * Remove an event listener
   */
  off<T extends StreamAccumulatorEventType>(
    event: T,
    handler: StreamAccumulatorEventHandler<T>
  ): void {
    this.listeners.get(event)?.delete(handler)
  }

  /**
   * Emit an event
   */
  private emit<T extends StreamAccumulatorEventType>(
    event: T,
    data: StreamAccumulatorEvents[T]
  ): void {
    const handlers = this.listeners.get(event)
    if (handlers) {
      handlers.forEach((handler) => handler(data))
    }
  }

  /**
   * Process a streaming event
   */
  handleEvent(streamEvent: StreamEvent): void {
    const event = streamEvent.event
    this.sessionId = streamEvent.session_id
    this.uuid = streamEvent.uuid

    switch (event.type) {
      case 'message_start':
        this.handleMessageStart(event)
        break

      case 'content_block_start':
        this.handleContentBlockStart(event)
        break

      case 'content_block_delta':
        this.handleContentBlockDelta(event)
        break

      case 'content_block_stop':
        this.handleContentBlockStop(event)
        break

      case 'message_delta':
        this.handleMessageDelta(event)
        break

      case 'message_stop':
        this.handleMessageStop()
        break
    }
  }

  /**
   * Handle message_start event
   */
  private handleMessageStart(event: any): void {
    this.messageId = event.message.id
    this.model = event.message.model
  }

  /**
   * Handle content_block_start event
   */
  private handleContentBlockStart(event: any): void {
    const { index, content_block } = event

    this.blocks.set(index, {
      type: content_block.type as 'text' | 'thinking' | 'tool_use',
      id: content_block.type === 'tool_use' ? content_block.id : undefined,
      name: content_block.type === 'tool_use' ? content_block.name : undefined,
      accumulated: ''
    })
  }

  /**
   * Handle content_block_delta event
   */
  private handleContentBlockDelta(event: any): void {
    const { index, delta } = event
    const block = this.blocks.get(index)

    if (!block) {
      console.warn(`[StreamAccumulator] Received delta for unknown block ${index}`)
      return
    }

    if (delta.type === 'text_delta') {
      block.accumulated += delta.text
      this.emit('text_chunk', { index, text: delta.text })
    } else if (delta.type === 'thinking_delta') {
      block.accumulated += delta.thinking
    } else if (delta.type === 'input_json_delta') {
      block.accumulated += delta.partial_json
      this.emit('tool_input_chunk', { index, chunk: delta.partial_json })
    }
  }

  /**
   * Handle content_block_stop event
   */
  private handleContentBlockStop(event: any): void {
    const { index } = event
    const accumulated = this.blocks.get(index)

    if (!accumulated) {
      console.warn(`[StreamAccumulator] Received stop for unknown block ${index}`)
      return
    }

    const block = this.toContentBlock(accumulated)
    this.emit('block_complete', { index, block })
  }

  /**
   * Handle message_delta event
   */
  private handleMessageDelta(event: any): void {
    this.emit('message_meta', {
      stopReason: event.delta.stop_reason,
      usage: event.usage
    })
  }

  /**
   * Handle message_stop event
   */
  private handleMessageStop(): void {
    const message = this.buildMessage()
    this.emit('message_complete', message)
    this.reset()
  }

  /**
   * Convert accumulated block to ContentBlock
   */
  private toContentBlock(accumulated: ContentBlockAccumulator): ContentBlock {
    switch (accumulated.type) {
      case 'text':
        return {
          type: 'text',
          text: accumulated.accumulated
        } as TextBlock

      case 'thinking':
        return {
          type: 'thinking',
          thinking: accumulated.accumulated
        } as ThinkingBlock

      case 'tool_use':
        return {
          type: 'tool_use',
          id: accumulated.id!,
          name: accumulated.name!,
          input: JSON.parse(accumulated.accumulated)
        } as ToolUseBlock

      default:
        throw new Error(`Unknown block type: ${accumulated.type}`)
    }
  }

  /**
   * Build complete AssistantMessage from accumulated blocks
   * Note: Returns in LIVE CLI format with wrapped message
   */
  private buildMessage(): AssistantMessage {
    const content: ContentBlock[] = []

    // Sort blocks by index and convert
    const sortedIndices = Array.from(this.blocks.keys()).sort((a, b) => a - b)
    for (const index of sortedIndices) {
      const accumulated = this.blocks.get(index)!
      content.push(this.toContentBlock(accumulated))
    }

    // Return in live CLI format
    return {
      type: 'assistant',
      message: {
        model: this.model,
        id: this.messageId,
        type: 'message',
        role: 'assistant',
        content,
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
        },
        context_management: null
      },
      parent_tool_use_id: null,
      session_id: this.sessionId,
      uuid: this.uuid
    }
  }

  /**
   * Reset accumulator state
   */
  reset(): void {
    this.blocks.clear()
    this.messageId = ''
    this.model = ''
    this.sessionId = ''
    this.uuid = ''
  }

  /**
   * Get current accumulated blocks (for debugging)
   */
  getBlocks(): Map<number, ContentBlockAccumulator> {
    return new Map(this.blocks)
  }
}
