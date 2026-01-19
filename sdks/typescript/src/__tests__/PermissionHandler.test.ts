/**
 * Unit tests for PermissionHandler
 */

import { describe, it, expect } from 'bun:test'
import { PermissionHandler, PermissionDecision, PermissionRequestData } from '../PermissionHandler'
import { ControlRequest, ControlResponse } from '../types'

describe('PermissionHandler', () => {
  describe('extractPermissionRequest', () => {
    it('should extract permission request from can_use_tool control request', () => {
      const controlRequest: ControlRequest = {
        type: 'control_request',
        request_id: 'req_123',
        request: {
          subtype: 'can_use_tool',
          tool_name: 'Bash',
          input: {
            command: 'rm -rf /'
          },
          permission_suggestions: [],
          blocked_path: null
        }
      }

      const result = PermissionHandler.extractPermissionRequest(controlRequest)

      expect(result).toBeDefined()
      expect(result!.requestId).toBe('req_123')
      expect(result!.toolName).toBe('Bash')
      expect(result!.input).toEqual({ command: 'rm -rf /' })
      expect(result!.suggestions).toEqual([])
      expect(result!.blockedPath).toBeNull()
    })

    it('should return null for non-permission control requests', () => {
      const controlRequest: ControlRequest = {
        type: 'control_request',
        request_id: 'req_123',
        request: {
          subtype: 'set_permission_mode',
          mode: 'plan'
        }
      }

      const result = PermissionHandler.extractPermissionRequest(controlRequest)
      expect(result).toBeNull()
    })

    it('should extract permission request with suggestions', () => {
      const controlRequest: ControlRequest = {
        type: 'control_request',
        request_id: 'req_456',
        request: {
          subtype: 'can_use_tool',
          tool_name: 'Write',
          input: {
            file_path: '/sensitive/data.txt',
            content: 'secret'
          },
          permission_suggestions: [
            { type: 'always_allow', rule: 'Write:/sensitive/*' }
          ],
          blocked_path: '/sensitive/data.txt'
        }
      }

      const result = PermissionHandler.extractPermissionRequest(controlRequest)

      expect(result).toBeDefined()
      expect(result!.requestId).toBe('req_456')
      expect(result!.toolName).toBe('Write')
      expect(result!.suggestions).toHaveLength(1)
      expect(result!.blockedPath).toBe('/sensitive/data.txt')
    })
  })

  describe('buildResponse', () => {
    describe('allow decision', () => {
      it('should build response for simple allow', () => {
        const decision: PermissionDecision = { action: 'allow' }

        const response = PermissionHandler.buildResponse('req_123', decision)

        expect(response).toEqual({
          type: 'control_response',
          response: {
            subtype: 'success',
            request_id: 'req_123',
            response: {
              behavior: 'allow'
            }
          }
        })
      })
    })

    describe('allow_always decision', () => {
      it('should build response with permission rule', () => {
        const decision: PermissionDecision = {
          action: 'allow_always',
          rule: {
            type: 'addRules',
            rules: [{
              tool_name: 'Bash',
              rule_content: 'Bash:command="ls *"'
            }],
            behavior: 'allow',
            destination: 'session'
          }
        }

        const response = PermissionHandler.buildResponse('req_123', decision)

        expect(response.response.subtype).toBe('success')
        expect(response.response.request_id).toBe('req_123')
        expect(response.response.response).toEqual({
          behavior: 'allow',
          updated_permissions: [decision.rule]
        })
      })
    })

    describe('allow_modified decision', () => {
      it('should build response with modified input', () => {
        const decision: PermissionDecision = {
          action: 'allow_modified',
          updatedInput: {
            command: 'ls'  // Instead of 'rm -rf /'
          }
        }

        const response = PermissionHandler.buildResponse('req_123', decision)

        expect(response.response.response).toEqual({
          behavior: 'allow',
          updated_input: {
            command: 'ls'
          }
        })
      })
    })

    describe('deny decision', () => {
      it('should build response for simple deny', () => {
        const decision: PermissionDecision = { action: 'deny' }

        const response = PermissionHandler.buildResponse('req_123', decision)

        expect(response.response.response).toEqual({
          behavior: 'deny',
          message: 'User denied'
        })
      })

      it('should build response with custom message', () => {
        const decision: PermissionDecision = {
          action: 'deny',
          message: 'This operation is too dangerous'
        }

        const response = PermissionHandler.buildResponse('req_123', decision)

        expect(response.response.response).toEqual({
          behavior: 'deny',
          message: 'This operation is too dangerous'
        })
      })
    })

    describe('deny_stop decision', () => {
      it('should build response with interrupt flag', () => {
        const decision: PermissionDecision = { action: 'deny_stop' }

        const response = PermissionHandler.buildResponse('req_123', decision)

        expect(response.response.response).toEqual({
          behavior: 'deny',
          message: 'User stopped execution',
          interrupt: true
        })
      })
    })

    describe('all decision types', () => {
      it('should include correct request_id for all decisions', () => {
        const decisions: PermissionDecision[] = [
          { action: 'allow' },
          { action: 'deny' },
          { action: 'deny_stop' },
          { action: 'allow_modified', updatedInput: {} },
          {
            action: 'allow_always',
            rule: {
              type: 'addRules',
              rules: [{ tool_name: 'Test', rule_content: 'test' }],
              behavior: 'allow',
              destination: 'session'
            }
          }
        ]

        for (const decision of decisions) {
          const response = PermissionHandler.buildResponse('req_unique', decision)
          expect(response.type).toBe('control_response')
          expect(response.response.subtype).toBe('success')
          expect(response.response.request_id).toBe('req_unique')
        }
      })
    })
  })

  describe('createAlwaysAllowRule', () => {
    it('should create session-scoped rule by default', () => {
      const rule = PermissionHandler.createAlwaysAllowRule(
        'Bash',
        'Bash:command="ls *"'
      )

      expect(rule).toEqual({
        type: 'addRules',
        rules: [{
          tool_name: 'Bash',
          rule_content: 'Bash:command="ls *"'
        }],
        behavior: 'allow',
        destination: 'session'
      })
    })

    it('should create global-scoped rule when specified', () => {
      const rule = PermissionHandler.createAlwaysAllowRule(
        'Write',
        'Write:file_path="/tmp/*"',
        'global'
      )

      expect(rule.destination).toBe('global')
      expect(rule.rules[0].tool_name).toBe('Write')
    })

    it('should handle complex rule content', () => {
      const rule = PermissionHandler.createAlwaysAllowRule(
        'Bash',
        'Bash:command="git *" && cwd="/project/*"'
      )

      expect(rule.rules[0].rule_content).toBe('Bash:command="git *" && cwd="/project/*"')
    })
  })

  describe('Integration: Extract and Build Flow', () => {
    it('should handle full permission request/response cycle', () => {
      // 1. Extract request
      const controlRequest: ControlRequest = {
        type: 'control_request',
        request_id: 'req_integration',
        request: {
          subtype: 'can_use_tool',
          tool_name: 'Bash',
          input: {
            command: 'git status'
          },
          permission_suggestions: [],
          blocked_path: null
        }
      }

      const permissionData = PermissionHandler.extractPermissionRequest(controlRequest)
      expect(permissionData).toBeDefined()
      expect(permissionData!.toolName).toBe('Bash')

      // 2. User makes decision
      const decision: PermissionDecision = {
        action: 'allow_always',
        rule: PermissionHandler.createAlwaysAllowRule(
          'Bash',
          'Bash:command="git *"',
          'session'
        )
      }

      // 3. Build response
      const response = PermissionHandler.buildResponse(
        permissionData!.requestId,
        decision
      )

      expect(response.type).toBe('control_response')
      expect(response.response.request_id).toBe('req_integration')
      expect(response.response.response).toHaveProperty('behavior', 'allow')
      expect(response.response.response).toHaveProperty('updated_permissions')
    })

    it('should handle deny cycle', () => {
      const controlRequest: ControlRequest = {
        type: 'control_request',
        request_id: 'req_deny',
        request: {
          subtype: 'can_use_tool',
          tool_name: 'Bash',
          input: {
            command: 'rm -rf /'
          },
          permission_suggestions: [],
          blocked_path: null
        }
      }

      const permissionData = PermissionHandler.extractPermissionRequest(controlRequest)!
      const decision: PermissionDecision = {
        action: 'deny',
        message: 'Dangerous command blocked'
      }

      const response = PermissionHandler.buildResponse(permissionData.requestId, decision)

      expect(response.response.response).toEqual({
        behavior: 'deny',
        message: 'Dangerous command blocked'
      })
    })

    it('should handle modified input cycle', () => {
      const controlRequest: ControlRequest = {
        type: 'control_request',
        request_id: 'req_modify',
        request: {
          subtype: 'can_use_tool',
          tool_name: 'Write',
          input: {
            file_path: '/root/sensitive.txt',
            content: 'data'
          },
          permission_suggestions: [],
          blocked_path: '/root/sensitive.txt'
        }
      }

      const permissionData = PermissionHandler.extractPermissionRequest(controlRequest)!

      // User modifies to safer path
      const decision: PermissionDecision = {
        action: 'allow_modified',
        updatedInput: {
          file_path: '/tmp/safe.txt',
          content: 'data'
        }
      }

      const response = PermissionHandler.buildResponse(permissionData.requestId, decision)

      expect(response.response.response).toEqual({
        behavior: 'allow',
        updated_input: {
          file_path: '/tmp/safe.txt',
          content: 'data'
        }
      })
    })
  })

  describe('Type Safety', () => {
    it('should enforce PermissionDecision type', () => {
      // These should all compile (type check)
      const allow: PermissionDecision = { action: 'allow' }
      const deny: PermissionDecision = { action: 'deny', message: 'test' }
      const denyStop: PermissionDecision = { action: 'deny_stop' }
      const modified: PermissionDecision = {
        action: 'allow_modified',
        updatedInput: {}
      }
      const always: PermissionDecision = {
        action: 'allow_always',
        rule: {
          type: 'addRules',
          rules: [],
          behavior: 'allow',
          destination: 'session'
        }
      }

      // All decisions should build responses successfully
      expect(() => PermissionHandler.buildResponse('test', allow)).not.toThrow()
      expect(() => PermissionHandler.buildResponse('test', deny)).not.toThrow()
      expect(() => PermissionHandler.buildResponse('test', denyStop)).not.toThrow()
      expect(() => PermissionHandler.buildResponse('test', modified)).not.toThrow()
      expect(() => PermissionHandler.buildResponse('test', always)).not.toThrow()
    })
  })
})
