# Multi-Agent SWE System

A hierarchical multi-agent system for large-scope software engineering work, built on the Claude CLI Go SDK.

## Overview

This system uses multiple specialized AI agents working together to handle complex software engineering tasks:

- **Orchestrator**: User-facing agent that triages requests
- **Planner**: Task decomposition and sub-agent coordination
- **Designer**: Technical design creation
- **Builder**: Code implementation
- **Reviewer**: Code review and feedback

## Quick Start

```bash
# Build the CLI
cd multiagent
go build -o swarm ./cmd/swarm

# Run with a mission
./swarm --mission "Build a hello world CLI in Go"

# Run in interactive mode
./swarm --interactive

# Customize models and budget
./swarm \
  --mission "Add user authentication" \
  --orchestrator-model sonnet \
  --planner-model opus \
  --builder-model sonnet \
  --budget 2.00
```

## CLI Flags

| Flag | Default | Description |
|------|---------|-------------|
| `--mission` | - | Task description to execute |
| `--interactive` | false | Run in interactive mode |
| `--work-dir` | `.` | Working directory for file operations |
| `--session-dir` | `.claude-swarm/sessions` | Directory for session logs |
| `--orchestrator-model` | `sonnet` | Model for Orchestrator agent |
| `--planner-model` | `sonnet` | Model for Planner agent |
| `--designer-model` | `haiku` | Model for Designer agent |
| `--builder-model` | `haiku` | Model for Builder agent |
| `--reviewer-model` | `haiku` | Model for Reviewer agent |
| `--budget` | `1.00` | Maximum budget in USD |
| `--max-iterations` | `50` | Maximum agent iterations |

## Architecture

```
User Input
    │
    ▼
┌───────────────────┐
│   Orchestrator    │  ← User-facing, triages requests
└───────────────────┘
    │
    │ [delegates complex tasks]
    ▼
┌───────────────────┐
│     Planner       │  ← Task decomposition, controls sub-agents
└───────────────────┘
    │
    ├──[Designer]──▶ Returns technical design
    ├──[Builder]───▶ Returns implementation results
    └──[Reviewer]──▶ Returns feedback (no verdicts)
```

### Agent Lifecycle

- **Long-running agents** (Orchestrator, Planner): Persist across the session, maintain conversation context
- **Ephemeral agents** (Designer, Builder, Reviewer): Created fresh for each task, destroyed after completion

## Session Logging

All agent I/O is captured to `.claude-swarm/sessions/` for analysis:

```
.claude-swarm/sessions/<session-id>/
├── orchestrator/
│   ├── to_cli.jsonl
│   ├── from_cli.jsonl
│   └── metadata.json
├── planner/
│   └── ...
├── designer/
│   ├── task-001/
│   └── task-002/
├── builder/
│   └── ...
├── reviewer/
│   └── ...
└── summary.json
```

## Testing

```bash
# Run all tests
go test -v ./...

# Test individual packages
go test -v ./agent/
go test -v ./orchestrator/
go test -v ./planner/
go test -v ./subagent/
go test -v ./subagents/designer/
go test -v ./subagents/builder/
go test -v ./subagents/reviewer/
go test -v ./protocol/
go test -v ./integration/
```

## Project Structure

```
multiagent/
├── cmd/swarm/main.go       # CLI entry point
├── agent/                  # Core agent infrastructure
│   ├── agent.go            # Agent interface
│   ├── config.go           # Configuration types
│   └── session.go          # Session management
├── orchestrator/           # User-facing agent
├── planner/                # Task coordination agent
│   ├── planner.go          # Core planner logic
│   └── streaming_integration.go  # StreamingPlanner with real-time events
├── subagent/               # Streaming sub-agent protocol
│   ├── protocol.go         # Protocol message types (Progress, FileEvent, etc.)
│   └── streaming.go        # StreamingSubAgent implementation
├── subagents/
│   ├── designer/           # Design agent (prompts, response parsing)
│   ├── builder/            # Implementation agent
│   └── reviewer/           # Review agent
├── protocol/               # Inter-agent communication types
├── testutil/               # Mock sessions and fixtures
├── integration/            # Integration tests including streaming tests
└── go.mod
```

## Documentation

- [ARCHITECTURE.md](./ARCHITECTURE.md) - Detailed system design
- [IMPLEMENTATION.md](./IMPLEMENTATION.md) - Implementation status and gaps

## Dependencies

This module depends on the Claude CLI Go SDK:

```go
require github.com/mzhaom/claude-cli-protocol/sdks/golang v0.0.0
```
