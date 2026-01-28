// Package execprotocol defines types for the `codex exec --json` JSONL output format.
//
// This is a simplified event format designed for external consumers.
// It differs from the internal protocol used between Codex core and UIs.
//
// Source of truth: ~/g/codex/codex-rs/exec/src/exec_events.rs
//
// Event types:
//   - thread.started: Thread initialized with thread_id
//   - turn.started: Turn processing started
//   - turn.completed: Turn completed successfully with usage
//   - turn.failed: Turn failed with error
//   - item.started: Item began processing
//   - item.updated: Item state changed
//   - item.completed: Item finished processing
//   - error: Unrecoverable error
package execprotocol
