// Package codexprotocol defines the wire protocol types for communication with the Codex CLI.
//
// This package implements the Codex protocol as defined in:
//   - codex-rs/protocol/src/protocol.rs (Submission/Op, Event/EventMsg)
//   - codex-rs/protocol/src/approvals.rs (approval types)
//   - codex-rs/protocol/src/user_input.rs (user input types)
//
// The protocol uses NDJSON (newline-delimited JSON) with a SQ/EQ (Submission Queue / Event Queue)
// pattern for bidirectional async communication.
//
// See PROTOCOL.md for the complete protocol specification.
package codexprotocol
