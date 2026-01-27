# Claude CLI Protocol Generator

This tool generates comprehensive documentation of the protocol between the Claude Agent SDK for Python and the Claude Code CLI.

## Installation

**Requirement**: This package depends on `claude-agent-sdk`. Install it first:

```bash
pip install claude-agent-sdk
```

## Usage

### Generate Documentation

```bash
# Capture protocol traces and generate all docs
generate-protocol-docs

# Use a specific model for trace capture (default: haiku)
generate-protocol-docs --model opus
```

## Output Artifacts

The generator produces the following files in the `docs/` directory:

| File | Description |
|------|-------------|
| [`docs/PROTOCOL_SPECIFICATION.md`](docs/PROTOCOL_SPECIFICATION.md) | Human-readable protocol documentation |
| [`docs/schemas/message.schema.json`](docs/schemas/message.schema.json) | JSON Schema for message types |
| [`docs/schemas/control-protocol.schema.json`](docs/schemas/control-protocol.schema.json) | JSON Schema for control protocol |
| [`docs/schemas/hooks.schema.json`](docs/schemas/hooks.schema.json) | JSON Schema for hook types |
| [`docs/traces/protocol_traces.json`](docs/traces/protocol_traces.json) | Captured protocol traces |

## Discovery Mechanisms

The generator avoids hardcoding by using three discovery mechanisms:

### 1. CLI Invocation Capture (No CLI execution required)

Instantiates `SubprocessCLITransport` with various `ClaudeAgentOptions` configurations and calls `_build_command()` to capture the exact CLI arguments. This discovers:

- All CLI flags and their values
- Environment variables set by the SDK
- Mapping from `ClaudeAgentOptions` fields to CLI flags

### 2. Protocol Trace Capture (Requires CLI execution)

Runs actual CLI interactions with different options to capture real JSON messages exchanged over stdio. This discovers:

- Available tools (from `SystemInitData`)
- Permission modes
- Model names
- Slash commands
- Built-in agents
- CLI version
- Message structure and examples

### 3. Type Introspection

Uses Python's `typing` module to extract type information from dataclasses and TypedDicts in `types.py`. This generates:

- Field tables for each message type
- JSON Schema definitions
- Type unions and variants
