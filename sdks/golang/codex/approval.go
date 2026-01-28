package codex

import (
	"context"

	"github.com/mzhaom/claude-cli-protocol/sdks/golang/codexprotocol"
)

// ExecApprovalRequest contains the data for a command execution approval request.
type ExecApprovalRequest struct {
	CallID                      string
	TurnID                      string
	Command                     []string
	CWD                         string
	Reason                      string
	ProposedExecpolicyAmendment *codexprotocol.ExecPolicyAmendment
	ParsedCommand               []codexprotocol.ParsedCommand
}

// PatchApprovalRequest contains the data for a patch approval request.
type PatchApprovalRequest struct {
	CallID    string
	TurnID    string
	Changes   map[string]codexprotocol.FileChange
	Reason    string
	GrantRoot *string
}

// ApprovalResponse contains the response to an approval request.
type ApprovalResponse struct {
	Decision  codexprotocol.ReviewDecision
	Amendment *codexprotocol.ExecPolicyAmendment
}

// ApprovalHandler handles approval requests.
type ApprovalHandler interface {
	HandleExecApproval(ctx context.Context, req *ExecApprovalRequest) (*ApprovalResponse, error)
	HandlePatchApproval(ctx context.Context, req *PatchApprovalRequest) (*ApprovalResponse, error)
}

// ApprovalHandlerFuncs allows using functions as ApprovalHandler.
type ApprovalHandlerFuncs struct {
	ExecFunc  func(ctx context.Context, req *ExecApprovalRequest) (*ApprovalResponse, error)
	PatchFunc func(ctx context.Context, req *PatchApprovalRequest) (*ApprovalResponse, error)
}

// HandleExecApproval implements ApprovalHandler.
func (f ApprovalHandlerFuncs) HandleExecApproval(ctx context.Context, req *ExecApprovalRequest) (*ApprovalResponse, error) {
	if f.ExecFunc != nil {
		return f.ExecFunc(ctx, req)
	}
	return &ApprovalResponse{Decision: codexprotocol.ReviewDecisionDenied}, nil
}

// HandlePatchApproval implements ApprovalHandler.
func (f ApprovalHandlerFuncs) HandlePatchApproval(ctx context.Context, req *PatchApprovalRequest) (*ApprovalResponse, error) {
	if f.PatchFunc != nil {
		return f.PatchFunc(ctx, req)
	}
	return &ApprovalResponse{Decision: codexprotocol.ReviewDecisionDenied}, nil
}

// approvalManager handles approval request/response flow.
type approvalManager struct {
	handler ApprovalHandler
}

// newApprovalManager creates a new approval manager.
func newApprovalManager(handler ApprovalHandler) *approvalManager {
	return &approvalManager{
		handler: handler,
	}
}

// HandleExecApproval handles an exec approval request and returns an ExecApprovalOp.
func (am *approvalManager) HandleExecApproval(ctx context.Context, req *ExecApprovalRequest) (*codexprotocol.ExecApprovalOp, error) {
	if am.handler == nil {
		// No handler configured, auto-deny
		return &codexprotocol.ExecApprovalOp{
			ID:       req.CallID,
			Decision: codexprotocol.ReviewDecisionDenied,
		}, nil
	}

	resp, err := am.handler.HandleExecApproval(ctx, req)
	if err != nil {
		return &codexprotocol.ExecApprovalOp{
			ID:       req.CallID,
			Decision: codexprotocol.ReviewDecisionDenied,
		}, err
	}

	op := &codexprotocol.ExecApprovalOp{
		ID:       req.CallID,
		Decision: resp.Decision,
	}
	return op, nil
}

// HandlePatchApproval handles a patch approval request and returns a PatchApprovalOp.
func (am *approvalManager) HandlePatchApproval(ctx context.Context, req *PatchApprovalRequest) (*codexprotocol.PatchApprovalOp, error) {
	if am.handler == nil {
		// No handler configured, auto-deny
		return &codexprotocol.PatchApprovalOp{
			ID:       req.CallID,
			Decision: codexprotocol.ReviewDecisionDenied,
		}, nil
	}

	resp, err := am.handler.HandlePatchApproval(ctx, req)
	if err != nil {
		return &codexprotocol.PatchApprovalOp{
			ID:       req.CallID,
			Decision: codexprotocol.ReviewDecisionDenied,
		}, err
	}

	return &codexprotocol.PatchApprovalOp{
		ID:       req.CallID,
		Decision: resp.Decision,
	}, nil
}

// AutoApproveHandler returns a handler that approves all requests.
func AutoApproveHandler() ApprovalHandler {
	return ApprovalHandlerFuncs{
		ExecFunc: func(ctx context.Context, req *ExecApprovalRequest) (*ApprovalResponse, error) {
			return &ApprovalResponse{Decision: codexprotocol.ReviewDecisionApproved}, nil
		},
		PatchFunc: func(ctx context.Context, req *PatchApprovalRequest) (*ApprovalResponse, error) {
			return &ApprovalResponse{Decision: codexprotocol.ReviewDecisionApproved}, nil
		},
	}
}

// DenyAllHandler returns a handler that denies all requests.
func DenyAllHandler() ApprovalHandler {
	return ApprovalHandlerFuncs{
		ExecFunc: func(ctx context.Context, req *ExecApprovalRequest) (*ApprovalResponse, error) {
			return &ApprovalResponse{Decision: codexprotocol.ReviewDecisionDenied}, nil
		},
		PatchFunc: func(ctx context.Context, req *PatchApprovalRequest) (*ApprovalResponse, error) {
			return &ApprovalResponse{Decision: codexprotocol.ReviewDecisionDenied}, nil
		},
	}
}

// ApproveForSessionHandler returns a handler that approves all requests for the session duration.
func ApproveForSessionHandler() ApprovalHandler {
	return ApprovalHandlerFuncs{
		ExecFunc: func(ctx context.Context, req *ExecApprovalRequest) (*ApprovalResponse, error) {
			return &ApprovalResponse{Decision: codexprotocol.ReviewDecisionApprovedForSession}, nil
		},
		PatchFunc: func(ctx context.Context, req *PatchApprovalRequest) (*ApprovalResponse, error) {
			return &ApprovalResponse{Decision: codexprotocol.ReviewDecisionApprovedForSession}, nil
		},
	}
}
