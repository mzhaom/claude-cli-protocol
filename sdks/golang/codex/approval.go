package codex

import "context"

// ApprovalPolicy controls tool execution approval.
type ApprovalPolicy string

const (
	// ApprovalPolicySuggest prompts for approval on tool executions.
	ApprovalPolicySuggest ApprovalPolicy = "suggest"

	// ApprovalPolicyAutoEdit auto-approves file edits but prompts for other tools.
	ApprovalPolicyAutoEdit ApprovalPolicy = "auto-edit"

	// ApprovalPolicyFullAuto auto-approves all tool executions.
	ApprovalPolicyFullAuto ApprovalPolicy = "full-auto"
)

// ApprovalRequest contains data for an approval request.
type ApprovalRequest struct {
	ThreadID string
	TurnID   string
	ToolName string
	Input    map[string]interface{}
}

// ApprovalResponse contains the response to an approval request.
type ApprovalResponse struct {
	Approved     bool
	Message      string
	UpdatedInput map[string]interface{}
}

// ApprovalHandler handles tool execution approval requests.
type ApprovalHandler interface {
	HandleApproval(ctx context.Context, req *ApprovalRequest) (*ApprovalResponse, error)
}

// ApprovalHandlerFunc is a function adapter for ApprovalHandler.
type ApprovalHandlerFunc func(ctx context.Context, req *ApprovalRequest) (*ApprovalResponse, error)

// HandleApproval implements ApprovalHandler.
func (f ApprovalHandlerFunc) HandleApproval(ctx context.Context, req *ApprovalRequest) (*ApprovalResponse, error) {
	return f(ctx, req)
}

// AutoApproveHandler returns a handler that auto-approves all tools.
func AutoApproveHandler() ApprovalHandler {
	return ApprovalHandlerFunc(func(ctx context.Context, req *ApprovalRequest) (*ApprovalResponse, error) {
		return &ApprovalResponse{Approved: true}, nil
	})
}

// DenyAllHandler returns a handler that denies all tools.
func DenyAllHandler() ApprovalHandler {
	return ApprovalHandlerFunc(func(ctx context.Context, req *ApprovalRequest) (*ApprovalResponse, error) {
		return &ApprovalResponse{
			Approved: false,
			Message:  "tool execution denied by policy",
		}, nil
	})
}
