package claude

import (
	"context"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/protocol"
)

// PermissionRequest contains the data for a permission request.
type PermissionRequest struct {
	RequestID   string
	ToolName    string
	Input       map[string]interface{}
	BlockedPath *string
}

// PermissionResponse contains the response to a permission request.
type PermissionResponse struct {
	Behavior           PermissionBehavior
	Message            string
	Interrupt          bool
	UpdatedInput       map[string]interface{}
	UpdatedPermissions []protocol.PermissionUpdate
}

// PermissionBehavior is the behavior for a permission response.
type PermissionBehavior string

const (
	// PermissionAllow allows tool execution.
	PermissionAllow PermissionBehavior = "allow"
	// PermissionDeny denies tool execution.
	PermissionDeny PermissionBehavior = "deny"
)

// PermissionHandler handles permission requests.
type PermissionHandler interface {
	HandlePermission(ctx context.Context, req *PermissionRequest) (*PermissionResponse, error)
}

// PermissionHandlerFunc is a function that implements PermissionHandler.
type PermissionHandlerFunc func(ctx context.Context, req *PermissionRequest) (*PermissionResponse, error)

// HandlePermission implements PermissionHandler.
func (f PermissionHandlerFunc) HandlePermission(ctx context.Context, req *PermissionRequest) (*PermissionResponse, error) {
	return f(ctx, req)
}

// permissionManager handles permission request/response flow.
type permissionManager struct {
	handler PermissionHandler
}

// newPermissionManager creates a new permission manager.
func newPermissionManager(handler PermissionHandler) *permissionManager {
	return &permissionManager{
		handler: handler,
	}
}

// ExtractPermissionRequest extracts a permission request from a control request.
// Returns nil if the control request is not a permission request.
func (pm *permissionManager) ExtractPermissionRequest(msg protocol.ControlRequest) *PermissionRequest {
	reqData, err := protocol.ParseControlRequest(msg.Request)
	if err != nil {
		return nil
	}

	canUseTool, ok := reqData.(protocol.CanUseToolRequest)
	if !ok {
		return nil
	}

	return &PermissionRequest{
		RequestID:   msg.RequestID,
		ToolName:    canUseTool.ToolName,
		Input:       canUseTool.Input,
		BlockedPath: canUseTool.BlockedPath,
	}
}

// HandleRequest handles a permission request and returns a control response.
func (pm *permissionManager) HandleRequest(ctx context.Context, msg protocol.ControlRequest) (*protocol.ControlResponse, error) {
	req := pm.ExtractPermissionRequest(msg)
	if req == nil {
		// Not a permission request
		return nil, nil
	}

	if pm.handler == nil {
		// No handler configured, auto-deny
		return pm.buildDenyResponse(msg.RequestID, "No permission handler configured", false), nil
	}

	resp, err := pm.handler.HandlePermission(ctx, req)
	if err != nil {
		return pm.buildDenyResponse(msg.RequestID, "Permission handler error", false), err
	}

	return pm.buildResponse(msg.RequestID, resp), nil
}

// buildResponse builds a control response from a permission response.
func (pm *permissionManager) buildResponse(requestID string, resp *PermissionResponse) *protocol.ControlResponse {
	if resp.Behavior == PermissionAllow {
		return &protocol.ControlResponse{
			Type: protocol.MessageTypeControlResponse,
			Response: protocol.ControlResponsePayload{
				Subtype:   "success",
				RequestID: requestID,
				Response: protocol.PermissionResultAllow{
					Behavior:           protocol.PermissionBehaviorAllow,
					UpdatedInput:       resp.UpdatedInput,
					UpdatedPermissions: resp.UpdatedPermissions,
				},
			},
		}
	}

	return pm.buildDenyResponse(requestID, resp.Message, resp.Interrupt)
}

// buildDenyResponse builds a deny control response.
func (pm *permissionManager) buildDenyResponse(requestID, message string, interrupt bool) *protocol.ControlResponse {
	return &protocol.ControlResponse{
		Type: protocol.MessageTypeControlResponse,
		Response: protocol.ControlResponsePayload{
			Subtype:   "success",
			RequestID: requestID,
			Response: protocol.PermissionResultDeny{
				Behavior:  protocol.PermissionBehaviorDeny,
				Message:   message,
				Interrupt: interrupt,
			},
		},
	}
}

// DefaultPermissionHandler returns a handler that denies all permissions.
func DefaultPermissionHandler() PermissionHandler {
	return PermissionHandlerFunc(func(ctx context.Context, req *PermissionRequest) (*PermissionResponse, error) {
		return &PermissionResponse{
			Behavior: PermissionDeny,
			Message:  "No permission handler configured",
		}, nil
	})
}

// AllowAllPermissionHandler returns a handler that allows all permissions.
func AllowAllPermissionHandler() PermissionHandler {
	return PermissionHandlerFunc(func(ctx context.Context, req *PermissionRequest) (*PermissionResponse, error) {
		return &PermissionResponse{
			Behavior: PermissionAllow,
		}, nil
	})
}
