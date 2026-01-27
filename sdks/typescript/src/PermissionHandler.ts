/**
 * PermissionHandler - Handles permission requests from CLI
 * Based on SESSION_API_DESIGN.md Section 7.3
 */

import { ControlRequest, ControlResponse, CanUseToolRequest, PermissionUpdate } from './types'

/**
 * User's permission decision
 */
export type PermissionDecision =
  | { action: 'allow' }
  | { action: 'allow_always'; rule: PermissionUpdate }
  | { action: 'allow_modified'; updatedInput: Record<string, unknown> }
  | { action: 'deny'; message?: string }
  | { action: 'deny_stop' }

/**
 * Permission request data for UI
 */
export interface PermissionRequestData {
  requestId: string
  toolName: string
  input: Record<string, unknown>
  suggestions: unknown[]
  blockedPath: string | null
}

/**
 * Handles permission requests and responses
 */
export class PermissionHandler {
  /**
   * Extract permission request data from control request
   */
  static extractPermissionRequest(request: ControlRequest): PermissionRequestData | null {
    if (request.request.subtype !== 'can_use_tool') {
      return null
    }

    const canUseToolRequest = request.request as CanUseToolRequest

    return {
      requestId: request.request_id,
      toolName: canUseToolRequest.tool_name,
      input: canUseToolRequest.input,
      suggestions: canUseToolRequest.permission_suggestions,
      blockedPath: canUseToolRequest.blocked_path
    }
  }

  /**
   * Build control response from user decision
   */
  static buildResponse(requestId: string, decision: PermissionDecision): ControlResponse {
    const base: ControlResponse = {
      type: 'control_response',
      response: {
        subtype: 'success',
        request_id: requestId,
        response: undefined
      }
    }

    switch (decision.action) {
      case 'allow':
        base.response.response = { behavior: 'allow' }
        break

      case 'allow_always':
        base.response.response = {
          behavior: 'allow',
          updated_permissions: [decision.rule]
        }
        break

      case 'allow_modified':
        base.response.response = {
          behavior: 'allow',
          updated_input: decision.updatedInput
        }
        break

      case 'deny':
        base.response.response = {
          behavior: 'deny',
          message: decision.message || 'User denied'
        }
        break

      case 'deny_stop':
        base.response.response = {
          behavior: 'deny',
          message: 'User stopped execution',
          interrupt: true
        }
        break
    }

    return base
  }

  /**
   * Create a permission rule for "always allow this"
   */
  static createAlwaysAllowRule(
    toolName: string,
    ruleContent: string,
    destination: 'session' | 'global' = 'session'
  ): PermissionUpdate {
    return {
      type: 'addRules',
      rules: [
        {
          tool_name: toolName,
          rule_content: ruleContent
        }
      ],
      behavior: 'allow',
      destination
    }
  }
}
