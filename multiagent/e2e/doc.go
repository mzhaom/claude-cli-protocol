// Package e2e provides end-to-end tests for the multi-agent system.
// Mock tests run without real Claude API calls and are suitable for CI.
// Live tests require Claude CLI to be installed and configured.
//
// Run mock tests:
//
//	go test ./e2e/...
//
// Run live tests (requires API key):
//
//	go test -tags=integration ./e2e/...
package e2e
