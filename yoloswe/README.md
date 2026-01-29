# yoloswe - Builder-Reviewer Loop for Software Engineering

A robust tool that orchestrates an iterative builder-reviewer loop for software engineering tasks. The builder (Claude) implements changes while the reviewer (Codex) provides feedback, creating a self-improving development cycle.

## Features

### Core Functionality
- **Dual-Session Architecture**: Separate builder (Claude SDK) and reviewer (Codex SDK) sessions
- **Iterative Refinement**: Continuous improvement loop until reviewer accepts or limits are reached
- **Auto-Approval Mode**: Builder automatically approves all tool executions for autonomous operation
- **Budget Control**: Hard limits on spending to prevent runaway costs
- **Time Constraints**: Wall-clock timeout protection
- **Iteration Limits**: Safety limit on number of builder-reviewer cycles

### Robustness & Error Handling
- **Comprehensive Input Validation**: Validates prompts, configuration, and all user inputs
- **Graceful Degradation**: Handles session failures, network issues, and partial errors
- **Context Cancellation**: Proper Ctrl+C handling with cleanup
- **Null Safety**: All responses validated before use
- **Enhanced Verdict Parsing**: 14+ acceptance patterns for robust reviewer response interpretation

### Advanced Features
- **Intelligent Auto-Answering**: Smart default selection for AskUserQuestion prompts
- **Multi-Select Support**: Handles both single and multi-select questions
- **Session Recording**: Optional recording of all interactions for debugging
- **Verbose Mode**: Detailed progress information and diagnostics
- **Custom System Prompts**: Override default builder behavior

## Installation

```bash
cd yoloswe
make build
```

This creates the `yoloswe` binary in the current directory.

## Usage

### Basic Usage

```bash
./yoloswe "Add unit tests for the user service"
```

### Advanced Usage

```bash
# Custom models and budget
./yoloswe --builder-model opus --reviewer-model o4 --budget 10.0 \
  "Refactor the authentication system"

# Longer timeout for complex tasks
./yoloswe --timeout 1800 --max-iterations 20 \
  "Implement a complete REST API with tests"

# Flags can appear anywhere (using Cobra)
./yoloswe "Implement feature X" --timeout 7200 --budget 10

# Specify working directory
./yoloswe --dir /path/to/project \
  "Fix all TODO comments in the codebase"

# Require approval for each tool execution
./yoloswe --require-approval "Make breaking changes to the API"

# Custom system prompt
./yoloswe --system "You are a senior Go developer focused on performance" \
  "Optimize the database queries"

# Verbose output with custom recording directory
./yoloswe --verbose --record /tmp/swe-logs \
  "Add comprehensive error handling"
```

## Configuration Flags

Flags can appear before or after the prompt (using [Cobra](https://github.com/spf13/cobra)).

| Flag | Type | Default | Description |
|------|------|---------|-------------|
| `--builder-model` | string | `sonnet` | Builder model: `haiku`, `sonnet`, `opus` |
| `--reviewer-model` | string | `gpt-5.2-codex` | Reviewer model: `gpt-5.2-codex`, `o4-mini`, `o4` |
| `--dir` | string | current dir | Working directory for the task |
| `--budget` | float | `5.0` | Maximum USD to spend on builder session |
| `--timeout` | int | `600` | Maximum seconds (wall-clock time) |
| `--max-iterations` | int | `10` | Maximum builder-reviewer iterations |
| `--record` | string | `.swe-sessions` | Session recordings directory |
| `--verbose` | bool | `false` | Show detailed output |
| `--system` | string | (none) | Custom system prompt for builder |
| `--require-approval` | bool | `false` | Require user approval for tool executions |

## Exit Conditions

The loop terminates when any of these conditions are met:

1. **✅ Accepted** - Reviewer approves the changes with "patch is correct" or similar
2. **💰 Budget Exceeded** - Cumulative builder cost >= max budget
3. **⏰ Timeout** - Elapsed time >= max timeout
4. **🔁 Max Iterations** - Iteration count >= max iterations
5. **❌ Error** - Unrecoverable error in builder or reviewer
6. **⛔ Interrupt** - User cancellation (Ctrl+C)

## Output

### Session Summary

After completion, yoloswe prints a detailed summary:

```
============================================================
YOLOSWE SESSION SUMMARY
------------------------------------------------------------
Exit reason:        accepted
Iterations:         3
Duration:           127.5s
------------------------------------------------------------
Builder:
  Cost:             $1.2340
  Input tokens:     12500
  Output tokens:    8750
------------------------------------------------------------
Reviewer:
  Input tokens:     5200
  Output tokens:    3100
============================================================
```

### Exit Codes

- `0` - Success (reviewer accepted)
- `1` - Failed or exited before acceptance

## Architecture

### Builder Session (builder.go)
- Wraps Claude SDK
- Auto-approves tool executions (unless `-require-approval`)
- Intelligent AskUserQuestion handling with smart defaults
- Tracks token usage and costs
- Enhanced error reporting

### Reviewer Session (reviewer.go)
- Wraps Codex SDK
- Persistent thread for follow-up reviews
- Structured review prompts focusing on:
  - Correctness
  - Test coverage
  - Maintainability
  - Developer experience
  - Performance
  - Security
- Returns full response text for verdict parsing

### Main Loop (swe.go)
- Coordinates builder and reviewer sessions
- Manages iteration lifecycle
- Enforces all limits (budget, time, iterations)
- Robust verdict parsing with 14+ acceptance patterns
- Comprehensive error handling and recovery

### Validation (validation.go)
- Input validation (prompts, configuration)
- Path validation (working directory, recording directory)
- Range validation (budget, timeout, iterations)
- Smart defaults and sanitization
- Warning system for unusual but valid configs

## Verdict Parsing

The reviewer response is parsed for acceptance using multiple patterns:

**Acceptance Patterns:**
- "patch is correct"
- "the patch is correct"
- "changes are correct"
- "implementation is correct"
- "verdict: accepted"
- "status: approved"
- "LGTM"
- "looks good to merge"
- And more...

**Rejection Patterns:**
- "patch is incorrect"
- "needs changes"
- "requires changes"
- "verdict: rejected"
- And more...

If none of the acceptance patterns match, the verdict defaults to **rejected** with the full reviewer response as feedback.

## Testing

### Unit Tests

```bash
# Run all unit tests
make test

# Run with verbose output
make test-v

# Run specific test
go test -v -run TestParseVerdict
```

### Test Coverage

The implementation includes comprehensive test coverage:

- **builder_test.go**: Builder session, auto-answering, configuration
- **swe_test.go**: Main loop, verdict parsing, prompts, stats
- **validation_test.go**: Input validation, sanitization, edge cases
- **integration_test.go**: Full end-to-end scenarios (skipped by default)

### Integration Tests

Integration tests require real SDK sessions and are skipped by default:

```bash
# Run integration tests (requires API access)
go test -v -run TestIntegration
```

## Error Handling

### Input Validation
- Empty prompts rejected
- Prompts < 3 characters rejected
- Prompts > 50,000 characters rejected
- Invalid models reported
- Nonexistent directories caught early

### Runtime Protection
- Nil session checks before operations
- Nil usage/result validation after operations
- Context cancellation at every step
- Graceful cleanup with deferred error logging
- Partial results preserved on failure

### Recovery Strategies
- Builder failures: Log error, stop cleanly
- Reviewer failures: Log error, stop cleanly
- Context cancelled: Exit with interrupt reason
- Tool failures: Logged but not fatal (builder continues)

## Examples

### Example 1: Simple Task

```bash
./yoloswe "Create a hello world function in Go"
```

Expected outcome: Completes in 1-2 iterations, reviewer accepts.

### Example 2: Complex Task with Budget Limit

```bash
./yoloswe -budget 2.0 -timeout 900 \
  "Add comprehensive test coverage for the entire API"
```

Expected outcome: May hit budget or iteration limit, partial progress saved.

### Example 3: High Iteration Task

```bash
./yoloswe -max-iterations 20 -budget 15.0 \
  "Refactor the codebase to use dependency injection"
```

Expected outcome: Multiple iterations of refinement until reviewer accepts.

### Example 4: Debugging with Verbose Mode

```bash
./yoloswe -verbose -record /tmp/debug \
  "Fix the race condition in the cache"
```

Expected outcome: Detailed logs and session recording for analysis.

## Best Practices

1. **Start Small**: Test with simple tasks first to understand the behavior
2. **Set Conservative Budgets**: Start with default $5 and increase as needed
3. **Use Timeouts**: Prevent infinite loops with reasonable time limits
4. **Review Recordings**: Use `--record` to save sessions for later analysis
5. **Verbose for Debugging**: Enable `--verbose` when troubleshooting
6. **Specific Goals**: Provide clear, specific prompts for better results
7. **Require Approval for Critical Changes**: Use `--require-approval` for important codebases

## Limitations

- Requires both Claude SDK and Codex SDK access
- Builder costs tracked but reviewer costs are not (Codex limitation)
- No support for pausing/resuming sessions
- Auto-approval mode requires trust in the builder
- Verdict parsing may miss unusual reviewer response formats

## Troubleshooting

### "failed to start builder/reviewer"
- Check API credentials are configured
- Verify network connectivity
- Ensure SDK versions are compatible

### "budget limit reached" too quickly
- Increase `--budget` flag
- Use cheaper model like `--builder-model haiku`
- Break task into smaller subtasks

### "max iterations reached"
- Task may be too complex for reviewer to accept
- Increase `--max-iterations` flag
- Simplify the prompt
- Check recording to see what's failing

### Builder keeps failing on same issue
- Reviewer feedback may be unclear
- Try `--builder-model opus` for better understanding
- Add more context in the prompt
- Use `--system` to guide builder behavior

## Contributing

When adding features:

1. Add unit tests for all new code
2. Update validation logic for new config options
3. Add error handling for new failure modes
4. Update this README with new flags/features
5. Maintain backward compatibility where possible

## License

See repository root for license information.
