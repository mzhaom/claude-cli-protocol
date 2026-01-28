// Package codex provides a Go SDK for interacting with the Codex CLI.
//
// Basic usage:
//
//	session := codex.NewSession(
//	    codex.WithModel("o3"),
//	    codex.WithApprovalPolicy(codexprotocol.ApprovalPolicyOnRequest),
//	    codex.WithSandboxPolicy(codexprotocol.SandboxPolicyWorkspaceWrite),
//	    codex.WithApprovalHandler(codex.AutoApproveHandler()),
//	)
//
//	if err := session.Start(ctx); err != nil {
//	    log.Fatal(err)
//	}
//	defer session.Stop()
//
//	result, err := session.Ask(ctx, "What is 2+2?")
//	if err != nil {
//	    log.Fatal(err)
//	}
//	fmt.Printf("Success: %v\n", result.Success)
package codex
