/**
 * MessageDiscriminator - Live CLI Format
 * All messages from live CLI have a "type" field at top level
 */

import { MessageType } from './types-live'

export class MessageDiscriminator {
  /**
   * Discriminate message type based on "type" field
   * Live CLI always includes this field
   */
  static discriminate(msg: unknown): MessageType {
    if (!msg || typeof msg !== 'object') {
      return 'unknown'
    }

    const obj = msg as Record<string, unknown>

    // All live CLI messages have explicit "type" field
    switch (obj.type) {
      case 'system':
        return 'system'

      case 'assistant':
        return 'assistant'

      case 'user':
        return 'user'

      case 'result':
        return 'result'

      case 'stream_event':
        return 'stream_event'

      case 'control_request':
        return 'control_request'

      case 'control_response':
        return 'control_response'

      default:
        return 'unknown'
    }
  }

  // Type guards
  static isSystemMessage(msg: unknown): boolean {
    return this.discriminate(msg) === 'system'
  }

  static isAssistantMessage(msg: unknown): boolean {
    return this.discriminate(msg) === 'assistant'
  }

  static isUserMessage(msg: unknown): boolean {
    return this.discriminate(msg) === 'user'
  }

  static isResultMessage(msg: unknown): boolean {
    return this.discriminate(msg) === 'result'
  }

  static isStreamEvent(msg: unknown): boolean {
    return this.discriminate(msg) === 'stream_event'
  }

  static isControlRequest(msg: unknown): boolean {
    return this.discriminate(msg) === 'control_request'
  }

  static isControlResponse(msg: unknown): boolean {
    return this.discriminate(msg) === 'control_response'
  }
}
