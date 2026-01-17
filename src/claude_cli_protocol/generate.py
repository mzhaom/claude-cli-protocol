#!/usr/bin/env python3
"""Generate protocol documentation for the Claude Agent SDK.

This script generates comprehensive documentation of the exact protocol between
the Claude Agent SDK for Python and the Claude Code CLI. It uses multiple
discovery mechanisms to ensure accuracy and avoid hardcoding:

DISCOVERY MECHANISMS
====================

1. CLI Invocation Capture (No CLI execution required)
   -------------------------------------------------
   Instantiates SubprocessCLITransport with various ClaudeAgentOptions
   configurations and calls _build_command() to capture the exact CLI
   arguments that would be generated. This discovers:
   - All CLI flags and their values
   - Environment variables set by the SDK
   - Mapping from ClaudeAgentOptions fields to CLI flags

2. Protocol Trace Capture (Requires CLI execution)
   ------------------------------------------------
   Runs actual CLI interactions with different options to capture real
   JSON messages exchanged over stdio. This discovers:
   - Available tools (from SystemInitData)
   - Permission modes
   - Model names
   - Slash commands
   - Built-in agents
   - CLI version
   - Message structure and examples

3. Type Introspection
   ------------------
   Uses Python's typing module to extract type information from
   dataclasses and TypedDicts in types.py. This generates:
   - Field tables for each message type
   - JSON Schema definitions
   - Type unions and variants

OUTPUT ARTIFACTS
================

- docs/PROTOCOL_SPECIFICATION.md  - Human-readable protocol documentation
- docs/protocol.proto             - Protocol Buffers definition file
- docs/schemas/*.json             - JSON Schema files for validation
- docs/traces/protocol_traces.json - Captured protocol traces for regression

Usage:
    generate-protocol-docs                    # Capture traces and generate docs
    generate-protocol-docs --check            # Check if docs are up to date
    generate-protocol-docs --no-capture       # Use existing traces only
    generate-protocol-docs --no-json-schema   # Skip JSON Schema generation
    generate-protocol-docs --no-protobuf      # Skip Protocol Buffers generation
    generate-protocol-docs --model opus       # Use specific model for traces
"""

from __future__ import annotations

import argparse
import asyncio
import json
import tempfile
from dataclasses import MISSING, dataclass, field, fields, is_dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Literal, Union, get_args, get_origin, get_type_hints

from typing_extensions import NotRequired, is_typeddict

from claude_agent_sdk import ClaudeSDKClient, create_sdk_mcp_server, query, tool, types
from claude_agent_sdk._cli_version import __cli_version__
from claude_agent_sdk._version import __version__
from claude_agent_sdk.types import ClaudeAgentOptions

# Path patterns to redact for privacy (absolute paths that could identify the runner)
import re


def sanitize_path(value: str) -> str:
    """Sanitize a string by replacing absolute paths with placeholders.

    Replaces patterns like /Users/username/... or /home/username/... with
    generic placeholders to avoid leaking identifying information.
    """
    if not isinstance(value, str):
        return value

    # Replace home directory paths
    # Pattern: /Users/<username>/... or /home/<username>/...
    value = re.sub(r"/Users/[^/]+/", "", value)
    value = re.sub(r"/home/[^/]+/", "", value)

    # Replace Windows-style paths
    value = re.sub(r"C:\\Users\\[^\\]+\\", "", value)

    # Replace /tmp paths with random suffixes (e.g., /tmp/tmpXXXXXX)
    value = re.sub(r"/tmp/tmp[a-zA-Z0-9_]+", "/tmp/<tmpdir>", value)
    value = re.sub(r"/var/folders/[^/]+/[^/]+/T/tmp[a-zA-Z0-9_]+", "/tmp/<tmpdir>", value)
    # macOS /private/tmp symlink
    value = re.sub(r"/private/tmp/tmp[a-zA-Z0-9_]+", "/tmp/<tmpdir>", value)

    # Replace .venv paths (the CLI binary path inside virtual environments)
    # This replaces paths like /path/to/project/.venv/... with <venv>/...
    value = re.sub(
        r"/Users/<user>/[^/]+/[^/]+/[^/]+/\.venv/",
        "<venv>/",
        value,
    )
    value = re.sub(
        r"/home/<user>/[^/]+/\.venv/",
        "<venv>/",
        value,
    )

    # Replace session IDs (UUIDs) with all zeros
    # Pattern: 8-4-4-4-12 hex characters like "7310810f-76e3-46f6-8a2a-1f052d876589"
    value = re.sub(
        r"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}",
        "00000000-0000-0000-0000-000000000000",
        value,
    )

    return value


def sanitize_data(data: Any) -> Any:
    """Recursively sanitize all strings in a data structure."""
    if isinstance(data, str):
        return sanitize_path(data)
    elif isinstance(data, dict):
        return {k: sanitize_data(v) for k, v in data.items()}
    elif isinstance(data, list):
        return [sanitize_data(item) for item in data]
    else:
        return data


@dataclass
class ProtocolTrace:
    """A captured protocol trace from a CLI interaction."""

    name: str
    description: str
    mode: str  # "string" or "streaming"
    options: dict[str, Any]
    prompt: str | list[dict[str, Any]]  # Original prompt (for reference)
    messages_sent: list[dict[str, Any]] = field(default_factory=list)  # Messages sent to CLI
    messages_received: list[dict[str, Any]] = field(default_factory=list)  # Messages from CLI
    control_requests_sent: list[dict[str, Any]] = field(default_factory=list)
    control_requests_received: list[dict[str, Any]] = field(default_factory=list)
    error: str | None = None


class ProtocolTracer:
    """Captures protocol traces from CLI interactions."""

    def __init__(self, output_dir: Path):
        self.output_dir = output_dir
        self.traces: list[ProtocolTrace] = []

    async def capture_string_mode(
        self,
        name: str,
        description: str,
        prompt: str,
        options: ClaudeAgentOptions | None = None,
    ) -> ProtocolTrace:
        """Capture a trace from a string-mode (one-shot) query."""
        opts = options or ClaudeAgentOptions()

        # In string mode, the prompt is passed as a CLI argument (after --print --)
        # The SDK internally converts it to a user message
        messages_sent = [
            {
                "type": "user",
                "message": {
                    "role": "user",
                    "content": prompt,
                },
            }
        ]

        trace = ProtocolTrace(
            name=name,
            description=description,
            mode="string",
            options=self._options_to_dict(opts),
            prompt=prompt,
            messages_sent=messages_sent,
        )

        try:
            async for msg in query(prompt=prompt, options=opts):
                trace.messages_received.append(self._message_to_dict(msg))
        except Exception as e:
            trace.error = str(e)

        self.traces.append(trace)
        return trace

    async def capture_streaming_mode(
        self,
        name: str,
        description: str,
        messages: list[dict[str, Any]],
        options: ClaudeAgentOptions | None = None,
        permission_callback: bool = False,
        hooks: dict[str, Any] | None = None,
    ) -> ProtocolTrace:
        """Capture a trace from a streaming-mode (interactive) query."""
        opts = options or ClaudeAgentOptions()

        # Track control messages
        control_sent: list[dict[str, Any]] = []
        control_received: list[dict[str, Any]] = []

        # Setup permission callback if requested
        if permission_callback:

            async def can_use_tool(
                tool_name: str, tool_input: dict[str, Any], context: Any
            ) -> types.PermissionResult:
                control_received.append(
                    {
                        "type": "can_use_tool",
                        "tool_name": tool_name,
                        "input": tool_input,
                    }
                )
                # Always allow for tracing purposes
                return types.PermissionResultAllow()

            # Use dataclass replace to preserve all fields including mcp_servers
            from dataclasses import replace
            opts = replace(opts, can_use_tool=can_use_tool)

        # Wrap hooks to capture hook callbacks if hooks are configured
        if opts.hooks:
            def make_hook_wrapper(
                original_fn: Any, event_name: str, received_list: list[dict[str, Any]]
            ) -> Any:
                """Create a wrapper that captures hook callbacks."""
                async def wrapped_hook(
                    hook_input: Any,
                    session_id: str | None,
                    context: types.HookContext,
                ) -> Any:
                    # Record the hook callback
                    received_list.append({
                        "type": "hook_callback",
                        "hook_event": event_name,
                        "tool_name": hook_input.get("tool_name") if isinstance(hook_input, dict) else None,
                    })
                    # Call the original hook
                    return await original_fn(hook_input, session_id, context)
                return wrapped_hook

            wrapped_hooks: dict[str, list[types.HookMatcher]] = {}
            for event_name, matchers in opts.hooks.items():
                wrapped_matchers: list[types.HookMatcher] = []
                for matcher in matchers:
                    original_hooks = matcher.hooks or []
                    wrapped_hook_fns = [
                        make_hook_wrapper(fn, event_name, control_received)
                        for fn in original_hooks
                    ]
                    wrapped_matchers.append(types.HookMatcher(
                        matcher=matcher.matcher,
                        hooks=wrapped_hook_fns,
                    ))
                wrapped_hooks[event_name] = wrapped_matchers
            opts = ClaudeAgentOptions(
                **{**self._options_to_dict(opts), "hooks": wrapped_hooks}
            )

        # In streaming mode, messages are sent via stdin as NDJSON
        messages_sent: list[dict[str, Any]] = []

        trace = ProtocolTrace(
            name=name,
            description=description,
            mode="streaming",
            options=self._options_to_dict(opts),
            prompt=messages,
            messages_sent=messages_sent,
            control_requests_sent=control_sent,
            control_requests_received=control_received,
        )

        try:
            client = ClaudeSDKClient(opts)

            async def message_stream():
                for msg in messages:
                    # Record each message as it's sent
                    messages_sent.append(msg)
                    yield msg

            await client.connect(message_stream())
            try:
                async for msg in client.receive_response():
                    trace.messages_received.append(self._message_to_dict(msg))
            finally:
                await client.disconnect()
        except Exception as e:
            trace.error = str(e)

        self.traces.append(trace)
        return trace

    def _options_to_dict(self, opts: ClaudeAgentOptions) -> dict[str, Any]:
        """Convert options to a serializable dict."""
        result = {}
        for f in fields(opts):
            value = getattr(opts, f.name)
            if value is not None and value != f.default:
                # Skip non-serializable fields
                if f.name in ("can_use_tool", "hooks", "stderr", "debug_stderr"):
                    result[f.name] = f"<{f.name}>"
                elif isinstance(value, Path):
                    result[f.name] = str(value)
                else:
                    try:
                        json.dumps(value)
                        result[f.name] = value
                    except (TypeError, ValueError):
                        result[f.name] = str(value)
        return result

    def _message_to_dict(self, msg: Any) -> dict[str, Any]:
        """Convert a message to a serializable dict."""
        if hasattr(msg, "__dataclass_fields__"):
            result = {"_type": type(msg).__name__}
            for f in fields(msg):
                value = getattr(msg, f.name)
                if is_dataclass(value):
                    result[f.name] = self._message_to_dict(value)
                elif isinstance(value, list):
                    result[f.name] = [
                        self._message_to_dict(v) if is_dataclass(v) else v
                        for v in value
                    ]
                else:
                    result[f.name] = value
            return result
        return {"_raw": str(msg)}

    def save_traces(
        self,
        filename: str = "protocol_traces.json",
        cli_invocations: list[dict[str, Any]] | None = None,
    ) -> Path:
        """Save all captured traces to a file.

        Args:
            filename: Output filename
            cli_invocations: Optional list of CLI invocation captures to include
        """
        output_path = self.output_dir / filename
        self.output_dir.mkdir(parents=True, exist_ok=True)

        data: dict[str, Any] = {
            "generated_at": datetime.now(timezone.utc).isoformat(),
            "sdk_version": __version__,
            "cli_version": __cli_version__,
            "traces": [self._trace_to_dict(t) for t in self.traces],
        }

        # Include CLI invocation captures if provided
        if cli_invocations:
            data["cli_invocations"] = cli_invocations

        # Sanitize all paths to avoid leaking identifying information
        data = sanitize_data(data)

        output_path.write_text(json.dumps(data, indent=2, default=str) + "\n")
        return output_path

    def _trace_to_dict(self, trace: ProtocolTrace) -> dict[str, Any]:
        """Convert a trace to a serializable dict."""
        return {
            "name": trace.name,
            "description": trace.description,
            "mode": trace.mode,
            "options": trace.options,
            "prompt": trace.prompt,
            "messages_sent": trace.messages_sent,
            "messages_received": trace.messages_received,
            "control_requests_sent": trace.control_requests_sent,
            "control_requests_received": trace.control_requests_received,
            "error": trace.error,
        }

    async def capture_streaming_mode_with_control(
        self,
        name: str,
        description: str,
        messages: list[dict[str, Any]],
        options: ClaudeAgentOptions | None = None,
        control_actions: list[tuple[str, dict[str, Any]]] | None = None,
        interrupt_after_messages: int | None = None,
        rewind_after_messages: int | None = None,
    ) -> ProtocolTrace:
        """Capture a trace from streaming mode with control protocol actions.

        This method demonstrates control protocol features like set_model,
        set_permission_mode, interrupt, and rewind_files.

        Args:
            name: Trace name for identification
            description: Human-readable description
            messages: Initial messages to send
            options: ClaudeAgentOptions
            control_actions: List of (action_type, params) tuples to execute.
                             action_type can be "set_model", "set_permission_mode",
                             "interrupt", "rewind_files"
            interrupt_after_messages: If set, send interrupt after receiving N messages
            rewind_after_messages: If set, send rewind_files after receiving N messages
                                   using the first captured user_message_id
        """
        opts = options or ClaudeAgentOptions()

        control_sent: list[dict[str, Any]] = []
        control_received: list[dict[str, Any]] = []
        messages_sent: list[dict[str, Any]] = []

        trace = ProtocolTrace(
            name=name,
            description=description,
            mode="streaming_with_control",
            options=self._options_to_dict(opts),
            prompt=messages,
            messages_sent=messages_sent,
            control_requests_sent=control_sent,
            control_requests_received=control_received,
        )

        try:
            client = ClaudeSDKClient(opts)

            async def message_stream():
                for msg in messages:
                    messages_sent.append(msg)
                    yield msg

            await client.connect(message_stream())

            try:
                # Execute control actions before receiving messages
                if control_actions:
                    for action_type, params in control_actions:
                        if action_type == "set_model":
                            control_sent.append({"type": action_type, "params": params})
                            await client.set_model(params.get("model"))
                        elif action_type == "set_permission_mode":
                            control_sent.append({"type": action_type, "params": params})
                            await client.set_permission_mode(
                                params.get("mode", "default")
                            )
                        elif action_type == "rewind_files":
                            # rewind_files with explicit user_message_id
                            user_message_id = params.get("user_message_id")
                            if user_message_id:
                                control_sent.append({"type": action_type, "params": params})
                                await client.rewind_files(user_message_id)
                            # Otherwise, handled below with rewind_after_messages
                        elif action_type == "interrupt":
                            # For interrupt, we need to receive some messages first
                            pass  # Will be handled below

                # Receive messages, potentially interrupting or rewinding
                msg_count = 0
                captured_user_message_id: str | None = None
                rewind_done = False
                async for msg in client.receive_response():
                    msg_dict = self._message_to_dict(msg)
                    trace.messages_received.append(msg_dict)
                    msg_count += 1

                    # Capture user_message_id from UserMessage for rewind
                    if msg_dict.get("_type") == "UserMessage" and msg_dict.get("uuid"):
                        captured_user_message_id = msg_dict["uuid"]

                    # Check if we should rewind
                    should_rewind = (
                        rewind_after_messages
                        and msg_count >= rewind_after_messages
                        and captured_user_message_id
                        and not rewind_done
                        and control_actions
                        and any(a[0] == "rewind_files" for a in control_actions)
                    )
                    if should_rewind:
                        control_sent.append({
                            "type": "rewind_files",
                            "params": {"user_message_id": captured_user_message_id}
                        })
                        await client.rewind_files(captured_user_message_id)
                        rewind_done = True
                        # Continue receiving messages after rewind

                    # Check if we should interrupt
                    should_interrupt = (
                        interrupt_after_messages
                        and msg_count >= interrupt_after_messages
                        and control_actions
                        and any(a[0] == "interrupt" for a in control_actions)
                    )
                    if should_interrupt:
                        control_sent.append({"type": "interrupt", "params": {}})
                        await client.interrupt()
                        break
            finally:
                await client.disconnect()

        except Exception as e:
            trace.error = str(e)

        self.traces.append(trace)
        return trace

    def load_traces(self, filename: str = "protocol_traces.json") -> bool:
        """Load traces from a file."""
        input_path = self.output_dir / filename
        if not input_path.exists():
            return False

        data = json.loads(input_path.read_text())
        self.traces = []
        for t in data.get("traces", []):
            self.traces.append(
                ProtocolTrace(
                    name=t["name"],
                    description=t["description"],
                    mode=t["mode"],
                    options=t["options"],
                    prompt=t["prompt"],
                    messages_sent=t.get("messages_sent", []),
                    messages_received=t.get("messages_received", []),
                    control_requests_sent=t.get("control_requests_sent", []),
                    control_requests_received=t.get("control_requests_received", []),
                    error=t.get("error"),
                )
            )
        return True


async def capture_protocol_traces(tracer: ProtocolTracer, model: str = "haiku") -> None:
    """Run various CLI interactions to capture protocol traces.

    Args:
        tracer: The ProtocolTracer instance to capture traces with.
        model: The model to use for traces. Defaults to "haiku" (cheapest).
    """
    print(f"Capturing protocol traces using model: {model}...")
    test_num = 0
    total_tests = 20  # Updated count for rewind_files scenario

    def next_test(desc: str) -> None:
        nonlocal test_num
        test_num += 1
        print(f"  [{test_num}/{total_tests}] {desc}...")

    # Create a temp directory for test files
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = Path(tmpdir) / "test.txt"
        test_file.write_text("Hello from test file!")

        # =================================================================
        # BASIC MODES
        # =================================================================

        next_test("String mode - simple query")
        await tracer.capture_string_mode(
            name="string_mode_simple",
            description="Simple one-shot query in string mode",
            prompt="What is 2+2? Reply with just the number.",
            options=ClaudeAgentOptions(max_turns=1, model=model, cwd=tmpdir),
        )

        next_test("String mode - with system prompt")
        await tracer.capture_string_mode(
            name="string_mode_system_prompt",
            description="Query with custom system prompt",
            prompt="Hello",
            options=ClaudeAgentOptions(
                system_prompt="You are a helpful assistant. Always respond with exactly one word.",
                max_turns=1,
                model=model,
                cwd=tmpdir,
            ),
        )

        next_test("Streaming mode - simple query")
        await tracer.capture_streaming_mode(
            name="streaming_mode_simple",
            description="Simple interactive query in streaming mode",
            messages=[
                {
                    "type": "user",
                    "message": {
                        "role": "user",
                        "content": "What is 3+3? Reply with just the number.",
                    },
                }
            ],
            options=ClaudeAgentOptions(max_turns=1, model=model, cwd=tmpdir),
        )

        # =================================================================
        # PERMISSION MODES
        # =================================================================

        next_test("Permission mode - default (prompts for dangerous tools)")
        await tracer.capture_string_mode(
            name="permission_mode_default",
            description="Default permission mode - CLI prompts for dangerous tools",
            prompt="What is 5+5? Reply with just the number.",
            options=ClaudeAgentOptions(
                max_turns=1,
                model=model,
                permission_mode="default",
                cwd=tmpdir,
            ),
        )

        next_test("Permission mode - bypassPermissions")
        await tracer.capture_string_mode(
            name="permission_mode_bypass",
            description="Bypass permissions - allows all tools without prompting",
            prompt=f"Read the file at {test_file} and tell me its contents.",
            options=ClaudeAgentOptions(
                max_turns=2,
                model=model,
                permission_mode="bypassPermissions",
                cwd=tmpdir,
            ),
        )

        next_test("Permission mode - acceptEdits")
        await tracer.capture_string_mode(
            name="permission_mode_accept_edits",
            description="Accept edits mode - auto-approves file modifications",
            prompt="What is 6+6? Reply with just the number.",
            options=ClaudeAgentOptions(
                max_turns=1,
                model=model,
                permission_mode="acceptEdits",
                cwd=tmpdir,
            ),
        )

        # =================================================================
        # TOOL USE AND APPROVAL FLOW
        # =================================================================

        next_test("Tool use with permission callback (approval flow)")
        # Use an SDK MCP tool which triggers the can_use_tool callback
        # Built-in tools like Write/Bash may have different permission handling

        # Define a custom tool that will require permission
        @tool("test_action", "A test action for permission callback demo", {"message": str})
        async def test_action_tool(args: dict[str, Any]) -> dict[str, Any]:
            return {"content": [{"type": "text", "text": f"Executed: {args['message']}"}]}

        # Create SDK MCP server with the test tool
        test_mcp_server = create_sdk_mcp_server("permission_test", tools=[test_action_tool])

        await tracer.capture_streaming_mode(
            name="tool_use_permission_callback",
            description="Tool use with SDK permission callback - demonstrates can_use_tool control request",
            messages=[
                {
                    "type": "user",
                    "message": {
                        "role": "user",
                        "content": "Use the test_action tool with message 'hello world'",
                    },
                }
            ],
            options=ClaudeAgentOptions(
                max_turns=2,
                cwd=tmpdir,
                model=model,
                mcp_servers={"permission_test": test_mcp_server},
                # No permission_mode set - this triggers permission callbacks
            ),
            permission_callback=True,
        )

        # =================================================================
        # CONTROL PROTOCOL - MODEL SWITCHING
        # =================================================================

        next_test("Model switching via control protocol")
        await tracer.capture_streaming_mode_with_control(
            name="control_set_model",
            description="Demonstrates set_model control request to switch models mid-session",
            messages=[
                {
                    "type": "user",
                    "message": {
                        "role": "user",
                        "content": "What is 7+7? Reply with just the number.",
                    },
                }
            ],
            options=ClaudeAgentOptions(max_turns=1, model=model, cwd=tmpdir),
            control_actions=[
                ("set_model", {"model": "sonnet"}),
            ],
        )

        # =================================================================
        # CONTROL PROTOCOL - PERMISSION MODE SWITCHING
        # =================================================================

        next_test("Permission mode switching via control protocol")
        await tracer.capture_streaming_mode_with_control(
            name="control_set_permission_mode",
            description="Demonstrates set_permission_mode control request",
            messages=[
                {
                    "type": "user",
                    "message": {
                        "role": "user",
                        "content": "What is 8+8? Reply with just the number.",
                    },
                }
            ],
            options=ClaudeAgentOptions(max_turns=1, model=model, cwd=tmpdir),
            control_actions=[
                ("set_permission_mode", {"mode": "acceptEdits"}),
            ],
        )

        # =================================================================
        # CONTROL PROTOCOL - INTERRUPT
        # =================================================================

        next_test("Interrupt control request")
        await tracer.capture_streaming_mode_with_control(
            name="control_interrupt",
            description="Demonstrates interrupt control request to stop execution",
            messages=[
                {
                    "type": "user",
                    "message": {
                        "role": "user",
                        "content": "Count from 1 to 100, one number at a time.",
                    },
                }
            ],
            options=ClaudeAgentOptions(max_turns=1, model=model, cwd=tmpdir),
            control_actions=[
                ("interrupt", {}),
            ],
            interrupt_after_messages=2,
        )

        # =================================================================
        # CONTROL PROTOCOL - REWIND FILES
        # =================================================================

        next_test("Rewind files control request")
        # Create a file that could be modified, demonstrating rewind capability
        rewind_test_file = Path(tmpdir) / "rewind_test.txt"
        rewind_test_file.write_text("Original content for rewind test")

        await tracer.capture_streaming_mode_with_control(
            name="control_rewind_files",
            description="Demonstrates rewind_files control request to restore file state",
            messages=[
                {
                    "type": "user",
                    "message": {
                        "role": "user",
                        "content": f"Read the file at {rewind_test_file} and tell me its contents.",
                    },
                }
            ],
            options=ClaudeAgentOptions(
                max_turns=2,
                model=model,
                cwd=tmpdir,
                enable_file_checkpointing=True,
                permission_mode="bypassPermissions",
            ),
            control_actions=[
                ("rewind_files", {}),  # Will use captured user_message_id
            ],
            rewind_after_messages=3,  # Rewind after receiving some messages
        )

        # =================================================================
        # PARTIAL MESSAGES / STREAMING
        # =================================================================

        next_test("Partial message streaming")
        await tracer.capture_streaming_mode(
            name="streaming_partial_messages",
            description="Query with partial message streaming enabled",
            messages=[
                {
                    "type": "user",
                    "message": {"role": "user", "content": "Count from 1 to 5."},
                }
            ],
            options=ClaudeAgentOptions(
                max_turns=1,
                include_partial_messages=True,
                model=model,
                cwd=tmpdir,
            ),
        )

        # =================================================================
        # FILE CHECKPOINTING
        # =================================================================

        next_test("File checkpointing enabled")
        await tracer.capture_string_mode(
            name="file_checkpointing",
            description="Query with file checkpointing enabled for rewind support",
            prompt="What is 9+9? Reply with just the number.",
            options=ClaudeAgentOptions(
                max_turns=1,
                model=model,
                enable_file_checkpointing=True,
                cwd=tmpdir,
            ),
        )

        # =================================================================
        # SUBAGENTS (Custom Agent Definitions)
        # =================================================================

        next_test("Subagents - custom agent definition")
        await tracer.capture_string_mode(
            name="subagents_custom",
            description="Query with custom subagent definitions",
            prompt="What is 10+10? Reply with just the number.",
            options=ClaudeAgentOptions(
                max_turns=1,
                model=model,
                cwd=tmpdir,
                agents={
                    "research-agent": types.AgentDefinition(
                        description="An agent for research tasks",
                        prompt="You are a research assistant. Be concise.",
                        model="haiku",
                        tools=["Read", "Grep", "Glob"],
                    ),
                    "code-agent": types.AgentDefinition(
                        description="An agent for coding tasks",
                        prompt="You are a coding assistant.",
                        model="sonnet",
                        tools=["Read", "Write", "Edit", "Bash"],
                    ),
                },
            ),
        )

        # =================================================================
        # MCP SERVERS
        # =================================================================

        next_test("MCP servers configuration")
        await tracer.capture_string_mode(
            name="mcp_servers",
            description="Query with MCP server configuration",
            prompt="What is 11+11? Reply with just the number.",
            options=ClaudeAgentOptions(
                max_turns=1,
                model=model,
                cwd=tmpdir,
                mcp_servers={
                    "filesystem": {
                        "type": "stdio",
                        "command": "npx",
                        "args": ["-y", "@anthropic/mcp-server-filesystem", "/tmp"],
                    },
                    "memory": {
                        "type": "stdio",
                        "command": "npx",
                        "args": ["-y", "@anthropic/mcp-server-memory"],
                    },
                },
            ),
        )

        # =================================================================
        # HOOKS (SDK-based hooks use Python callables)
        # =================================================================

        # Define a simple hook callback for demonstration that captures the callback
        hook_callbacks_received: list[dict[str, Any]] = []

        async def pre_tool_hook(
            hook_input: types.PreToolUseHookInput,
            _session_id: str | None,
            _context: types.HookContext,
        ) -> types.SyncHookJSONOutput:
            """Example pre-tool hook that allows all tools and captures the callback."""
            hook_callbacks_received.append({
                "hook_event": "PreToolUse",
                "tool_name": hook_input.get("tool_name"),
                "tool_input": hook_input.get("tool_input"),
            })
            return types.SyncHookJSONOutput(decision="approve")

        next_test("Hooks - PreToolUse callback with tool use")
        # Use streaming mode so hooks can actually be called via control protocol
        await tracer.capture_streaming_mode(
            name="hooks_pre_tool_use",
            description="Hook callback triggered by tool use - demonstrates hook_callback control request",
            messages=[
                {
                    "type": "user",
                    "message": {
                        "role": "user",
                        "content": f"Read the file at {test_file}. Use the Read tool.",
                    },
                }
            ],
            options=ClaudeAgentOptions(
                max_turns=2,
                model=model,
                cwd=tmpdir,
                permission_mode="bypassPermissions",  # Allow tool use without permission prompts
                hooks={
                    "PreToolUse": [
                        types.HookMatcher(
                            matcher="Read",  # Match Read tool
                            hooks=[pre_tool_hook],
                        )
                    ],
                },
            ),
        )

        # =================================================================
        # SETTING SOURCES
        # =================================================================

        next_test("Setting sources - project only")
        await tracer.capture_string_mode(
            name="setting_sources_project",
            description="Query with setting_sources=['project'] to use only project settings",
            prompt="What is 13+13? Reply with just the number.",
            options=ClaudeAgentOptions(
                max_turns=1,
                model=model,
                cwd=tmpdir,
                setting_sources=["project"],
            ),
        )

        next_test("Setting sources - user and local")
        await tracer.capture_string_mode(
            name="setting_sources_user_local",
            description="Query with setting_sources=['user', 'local']",
            prompt="What is 14+14? Reply with just the number.",
            options=ClaudeAgentOptions(
                max_turns=1,
                model=model,
                cwd=tmpdir,
                setting_sources=["user", "local"],
            ),
        )

        # =================================================================
        # SESSION RESUME
        # =================================================================

        next_test("Session resume")
        await tracer.capture_string_mode(
            name="session_resume",
            description="Query demonstrating session resume capability",
            prompt="What is 15+15? Reply with just the number.",
            options=ClaudeAgentOptions(
                max_turns=1,
                model=model,
                cwd=tmpdir,
                # Note: resume requires a valid session ID; this demonstrates the flag
                # In practice, you would use a session ID from a previous conversation
                # resume="session-id-from-previous-conversation",
            ),
        )

        # =================================================================
        # PLUGINS
        # =================================================================

        next_test("Plugins configuration")
        await tracer.capture_string_mode(
            name="plugins_config",
            description="Query with plugins configuration",
            prompt="What is 16+16? Reply with just the number.",
            options=ClaudeAgentOptions(
                max_turns=1,
                model=model,
                cwd=tmpdir,
                # Note: plugins require actual plugin implementations
                # This demonstrates the configuration structure
                plugins=[],
            ),
        )

    print(f"Captured {len(tracer.traces)} traces")


# ============================================================================
# Type introspection utilities (for combining with trace data)
# ============================================================================


def get_type_name(typ: Any) -> str:
    """Get a human-readable name for a type."""
    if typ is type(None):
        return "null"
    if typ is str:
        return "string"
    if typ is int:
        return "integer"
    if typ is float:
        return "number"
    if typ is bool:
        return "boolean"
    if typ is Any:
        return "any"

    origin = get_origin(typ)

    if origin is Literal:
        values = get_args(typ)
        if len(values) == 1:
            return json.dumps(values[0])
        return " | ".join(json.dumps(v) for v in values)

    if origin is Union:
        args = get_args(typ)
        non_none = [a for a in args if a is not type(None)]
        if len(non_none) < len(args):
            if len(non_none) == 1:
                return f"{get_type_name(non_none[0])} | null"
            inner = " | ".join(get_type_name(a) for a in non_none)
            return f"({inner}) | null"
        return " | ".join(get_type_name(a) for a in args)

    if origin is list:
        args = get_args(typ)
        if args:
            return f"Array<{get_type_name(args[0])}>"
        return "Array"

    if origin is dict:
        args = get_args(typ)
        if args and len(args) == 2:
            return f"Record<{get_type_name(args[0])}, {get_type_name(args[1])}>"
        return "object"

    if hasattr(typ, "__name__"):
        return typ.__name__

    return str(typ)


def is_not_required(typ: Any) -> bool:
    """Check if a type is wrapped in NotRequired."""
    return get_origin(typ) is NotRequired


def unwrap_not_required(typ: Any) -> Any:
    """Unwrap NotRequired to get the inner type."""
    if is_not_required(typ):
        args = get_args(typ)
        return args[0] if args else typ
    return typ


def extract_typeddict_info(cls: type) -> dict[str, Any]:
    """Extract field information from a TypedDict."""
    try:
        hints = get_type_hints(cls, include_extras=True)
    except Exception:
        hints = {}

    fields_info = []
    for name, typ in hints.items():
        is_optional = is_not_required(typ)
        actual_type = unwrap_not_required(typ)
        fields_info.append(
            {
                "name": name,
                "type": get_type_name(actual_type),
                "required": not is_optional,
                "raw_type": actual_type,
            }
        )

    return {
        "kind": "typeddict",
        "name": cls.__name__,
        "doc": cls.__doc__,
        "fields": fields_info,
    }


def extract_dataclass_info(cls: type) -> dict[str, Any]:
    """Extract field information from a dataclass."""
    fields_info = []
    for f in fields(cls):
        has_default = f.default is not MISSING or f.default_factory is not MISSING
        fields_info.append(
            {
                "name": f.name,
                "type": get_type_name(f.type),
                "required": not has_default,
                "raw_type": f.type,
            }
        )

    return {
        "kind": "dataclass",
        "name": cls.__name__,
        "doc": cls.__doc__,
        "fields": fields_info,
    }


def extract_type_info(typ: Any) -> dict[str, Any]:
    """Extract information from any type."""
    if is_dataclass(typ):
        return extract_dataclass_info(typ)
    if is_typeddict(typ):
        return extract_typeddict_info(typ)

    origin = get_origin(typ)
    if origin is Literal:
        return {"kind": "literal", "values": list(get_args(typ))}
    if origin is Union:
        return {"kind": "union", "variants": [get_type_name(a) for a in get_args(typ)]}

    return {"kind": "primitive", "type": get_type_name(typ)}


# ============================================================================
# Type definitions to document
# ============================================================================

MESSAGE_TYPES = [
    ("UserMessage", types.UserMessage),
    ("AssistantMessage", types.AssistantMessage),
    ("SystemMessage", types.SystemMessage),
    ("ResultMessage", types.ResultMessage),
    ("StreamEvent", types.StreamEvent),
]

CONTENT_BLOCK_TYPES = [
    ("TextBlock", types.TextBlock),
    ("ThinkingBlock", types.ThinkingBlock),
    ("ToolUseBlock", types.ToolUseBlock),
    ("ToolResultBlock", types.ToolResultBlock),
]

CONTROL_REQUEST_TYPES = [
    ("SDKControlInterruptRequest", types.SDKControlInterruptRequest),
    ("SDKControlPermissionRequest", types.SDKControlPermissionRequest),
    ("SDKControlInitializeRequest", types.SDKControlInitializeRequest),
    ("SDKControlSetPermissionModeRequest", types.SDKControlSetPermissionModeRequest),
    ("SDKHookCallbackRequest", types.SDKHookCallbackRequest),
    ("SDKControlMcpMessageRequest", types.SDKControlMcpMessageRequest),
    ("SDKControlRewindFilesRequest", types.SDKControlRewindFilesRequest),
]

CONTROL_RESPONSE_TYPES = [
    ("ControlResponse", types.ControlResponse),
    ("ControlErrorResponse", types.ControlErrorResponse),
]

HOOK_INPUT_TYPES = [
    ("PreToolUseHookInput", types.PreToolUseHookInput),
    ("PostToolUseHookInput", types.PostToolUseHookInput),
    ("UserPromptSubmitHookInput", types.UserPromptSubmitHookInput),
    ("StopHookInput", types.StopHookInput),
    ("SubagentStopHookInput", types.SubagentStopHookInput),
    ("PreCompactHookInput", types.PreCompactHookInput),
]

HOOK_OUTPUT_TYPES = [
    ("AsyncHookJSONOutput", types.AsyncHookJSONOutput),
    ("SyncHookJSONOutput", types.SyncHookJSONOutput),
]

PERMISSION_TYPES = [
    ("PermissionResultAllow", types.PermissionResultAllow),
    ("PermissionResultDeny", types.PermissionResultDeny),
    ("PermissionUpdate", types.PermissionUpdate),
    ("PermissionRuleValue", types.PermissionRuleValue),
    ("ToolPermissionContext", types.ToolPermissionContext),
]

MCP_SERVER_TYPES = [
    ("McpStdioServerConfig", types.McpStdioServerConfig),
    ("McpSSEServerConfig", types.McpSSEServerConfig),
    ("McpHttpServerConfig", types.McpHttpServerConfig),
    ("McpSdkServerConfig", types.McpSdkServerConfig),
]

CONFIG_TYPES = [
    ("ClaudeAgentOptions", types.ClaudeAgentOptions),
    ("SandboxSettings", types.SandboxSettings),
    ("SandboxNetworkConfig", types.SandboxNetworkConfig),
    ("AgentDefinition", types.AgentDefinition),
]

# ============================================================================
# Dynamic extraction of CLI information by capturing actual SDK execution
# ============================================================================


@dataclass
class CLIInvocationCapture:
    """Captured CLI invocation details from actual SDK execution."""

    options_name: str  # Human-readable name for the options configuration
    options: dict[str, Any]  # The ClaudeAgentOptions as dict
    command: list[str]  # The actual CLI command built
    env_vars: dict[str, str]  # SDK-specific environment variables set
    mode: str  # "string" or "streaming"


def capture_cli_invocation(
    options: ClaudeAgentOptions,
    mode: str,
    options_name: str = "default",
) -> CLIInvocationCapture:
    """Capture the actual CLI command and environment for given options.

    This instantiates SubprocessCLITransport and extracts the command it would
    build, without actually executing the CLI.
    """
    from claude_agent_sdk._internal.transport.subprocess_cli import (
        SubprocessCLITransport,
    )
    from claude_agent_sdk._version import __version__ as sdk_version

    # Create a transport instance to access _build_command()
    # Use a dummy prompt - we won't actually connect
    if mode == "string":
        transport = SubprocessCLITransport(prompt="dummy", options=options)
    else:
        # For streaming mode, use an async generator placeholder

        async def dummy_stream():
            yield {}

        transport = SubprocessCLITransport(prompt=dummy_stream(), options=options)

    # Get the command that would be built
    cmd = transport._build_command()

    # Compute the SDK-specific environment variables that would be set
    # (not including inherited system env vars)
    sdk_env_vars: dict[str, str] = {
        "CLAUDE_CODE_ENTRYPOINT": "sdk-py",
        "CLAUDE_AGENT_SDK_VERSION": sdk_version,
    }

    if options.enable_file_checkpointing:
        sdk_env_vars["CLAUDE_CODE_ENABLE_SDK_FILE_CHECKPOINTING"] = "true"

    if options.cwd:
        sdk_env_vars["PWD"] = str(options.cwd)

    # Convert options to dict for serialization
    options_dict = {}
    for f in fields(options):
        value = getattr(options, f.name)
        if value is not None and value != f.default:
            # Skip non-serializable fields
            if f.name in ("can_use_tool", "hooks", "stderr", "debug_stderr"):
                options_dict[f.name] = f"<{f.name}>"
            elif isinstance(value, Path):
                options_dict[f.name] = str(value)
            else:
                try:
                    json.dumps(value)
                    options_dict[f.name] = value
                except (TypeError, ValueError):
                    options_dict[f.name] = str(value)

    return CLIInvocationCapture(
        options_name=options_name,
        options=options_dict,
        command=cmd,
        env_vars=sdk_env_vars,
        mode=mode,
    )


def capture_cli_invocations_for_various_options() -> list[CLIInvocationCapture]:
    """Capture CLI invocations for various ClaudeAgentOptions configurations.

    This explores different option combinations to discover all CLI flags.
    """
    captures: list[CLIInvocationCapture] = []

    # 1. Minimal options - string mode
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(),
            mode="string",
            options_name="minimal_string_mode",
        )
    )

    # 2. Minimal options - streaming mode
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(),
            mode="streaming",
            options_name="minimal_streaming_mode",
        )
    )

    # 3. With system prompt (string)
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(system_prompt="You are a helpful assistant."),
            mode="string",
            options_name="with_system_prompt",
        )
    )

    # 4. With system prompt (preset with append)
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(
                system_prompt={"type": "preset", "append": "Additional instructions."}
            ),
            mode="string",
            options_name="with_append_system_prompt",
        )
    )

    # 5. With model and fallback model
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(model="haiku", fallback_model="sonnet"),
            mode="string",
            options_name="with_model_options",
        )
    )

    # 6. With tools configuration
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(
                tools=["Read", "Write"],
                allowed_tools=["Bash"],
                disallowed_tools=["WebSearch"],
            ),
            mode="string",
            options_name="with_tools_config",
        )
    )

    # 7. With limits
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(max_turns=5, max_budget_usd=1.0),
            mode="string",
            options_name="with_limits",
        )
    )

    # 8. With permission mode
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(permission_mode="bypassPermissions"),
            mode="string",
            options_name="with_permission_mode",
        )
    )

    # 9. With session options
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(continue_conversation=True),
            mode="string",
            options_name="with_continue",
        )
    )

    # 10. With resume
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(resume="session-123"),
            mode="string",
            options_name="with_resume",
        )
    )

    # 11. With partial messages
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(include_partial_messages=True),
            mode="streaming",
            options_name="with_partial_messages",
        )
    )

    # 12. With fork session
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(fork_session=True),
            mode="string",
            options_name="with_fork_session",
        )
    )

    # 13. With betas
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(betas=["beta-feature-1"]),
            mode="string",
            options_name="with_betas",
        )
    )

    # 14. With MCP servers
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(
                mcp_servers={
                    "test-server": {
                        "type": "stdio",
                        "command": "node",
                        "args": ["server.js"],
                    }
                }
            ),
            mode="string",
            options_name="with_mcp_servers",
        )
    )

    # 15. With max thinking tokens
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(max_thinking_tokens=1000),
            mode="string",
            options_name="with_max_thinking_tokens",
        )
    )

    # 16. With output format (JSON schema)
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(
                output_format={
                    "type": "json_schema",
                    "schema": {
                        "type": "object",
                        "properties": {"result": {"type": "string"}},
                    },
                }
            ),
            mode="string",
            options_name="with_json_schema",
        )
    )

    # 17. With file checkpointing
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(enable_file_checkpointing=True),
            mode="string",
            options_name="with_file_checkpointing",
        )
    )

    # 18. With cwd
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(cwd="/tmp/test"),
            mode="string",
            options_name="with_cwd",
        )
    )

    # 19. With add_dirs
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(add_dirs=["/extra/dir1", "/extra/dir2"]),
            mode="string",
            options_name="with_add_dirs",
        )
    )

    # 20. With settings
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(settings='{"key": "value"}'),
            mode="string",
            options_name="with_settings",
        )
    )

    # 21. With agents
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(
                agents={
                    "custom-agent": types.AgentDefinition(
                        description="A custom agent for testing",
                        prompt="You are a test agent.",
                        model="haiku",
                        tools=["Read"],
                    )
                }
            ),
            mode="string",
            options_name="with_agents",
        )
    )

    # 22. With permission prompt tool
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(permission_prompt_tool_name="stdio"),
            mode="streaming",
            options_name="with_permission_prompt_tool",
        )
    )

    # 23. With setting_sources (project only)
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(setting_sources=["project"]),
            mode="string",
            options_name="with_setting_sources_project",
        )
    )

    # 24. With setting_sources (multiple)
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(setting_sources=["user", "project", "local"]),
            mode="string",
            options_name="with_setting_sources_multiple",
        )
    )

    # 25. With hooks (SDK hooks are Python callables, so we show empty hooks config)
    # Note: hooks in the SDK are Python callback functions, not CLI commands
    # The CLI invocation won't show hooks as they're handled in-process by the SDK
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(
                hooks={
                    "PreToolUse": [],  # Empty list - hooks are callables added at runtime
                }
            ),
            mode="string",
            options_name="with_hooks",
        )
    )

    # 26. With plugins
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(plugins=[]),
            mode="string",
            options_name="with_plugins_empty",
        )
    )

    # 27. With sandbox settings
    captures.append(
        capture_cli_invocation(
            ClaudeAgentOptions(
                sandbox=types.SandboxSettings(
                    enabled=True,
                    network=types.SandboxNetworkConfig(
                        allow_hosts=["api.example.com"],
                        block_hosts=["evil.com"],
                    ),
                )
            ),
            mode="string",
            options_name="with_sandbox",
        )
    )

    return captures


def analyze_cli_invocations(
    captures: list[CLIInvocationCapture],
) -> dict[str, Any]:
    """Analyze captured CLI invocations to extract flag mappings.

    Returns a dict with:
    - flags: dict mapping flag name to info about when it's used
    - env_vars: dict mapping env var name to info about when it's set
    """
    # Track which flags appear in which captures
    flag_occurrences: dict[str, list[str]] = {}  # flag -> list of options_names
    flag_values: dict[str, set[str]] = {}  # flag -> set of values seen

    # Track environment variables
    env_occurrences: dict[str, list[str]] = {}  # var -> list of options_names
    env_values: dict[str, set[str]] = {}  # var -> set of values seen

    for capture in captures:
        cmd = capture.command

        # Parse command into flags and values
        i = 0
        while i < len(cmd):
            arg = cmd[i]
            if arg.startswith("--"):
                flag = arg
                if flag not in flag_occurrences:
                    flag_occurrences[flag] = []
                    flag_values[flag] = set()

                flag_occurrences[flag].append(capture.options_name)

                # Check if next arg is a value (not a flag)
                if i + 1 < len(cmd) and not cmd[i + 1].startswith("--"):
                    value = cmd[i + 1]
                    # Truncate long values for readability
                    if len(value) > 50:
                        value = value[:50] + "..."
                    flag_values[flag].add(value)
                    i += 1
            i += 1

        # Track environment variables
        for var, value in capture.env_vars.items():
            if var not in env_occurrences:
                env_occurrences[var] = []
                env_values[var] = set()
            env_occurrences[var].append(capture.options_name)
            env_values[var].add(value)

    result = {
        "flags": {
            flag: {
                "occurrences": occurrences,
                "values": list(flag_values[flag]),
            }
            for flag, occurrences in flag_occurrences.items()
        },
        "env_vars": {
            var: {
                "occurrences": occurrences,
                "values": list(env_values[var]),
            }
            for var, occurrences in env_occurrences.items()
        },
        "captures": [
            {
                "name": c.options_name,
                "mode": c.mode,
                "options": c.options,
                "command": c.command,
                "env_vars": c.env_vars,
            }
            for c in captures
        ],
    }

    # Sanitize all paths to avoid leaking identifying information
    return sanitize_data(result)


def extract_cli_capabilities_from_traces(
    traces: list[ProtocolTrace],
) -> dict[str, Any]:
    """Extract CLI capabilities from SystemInitData in captured traces.

    Returns a dict with discovered capabilities like:
    - tools: list of available tool names
    - permission_modes: set of permission modes seen
    - models: set of model names seen
    - slash_commands: list of slash commands
    - agents: list of agent names
    - skills: list of skill names
    - plugins: list of plugin names
    - cli_version: CLI version string
    - output_styles: set of output styles seen
    """
    capabilities: dict[str, Any] = {
        "tools": set(),
        "permission_modes": set(),
        "models": set(),
        "slash_commands": set(),
        "agents": set(),
        "skills": set(),
        "plugins": set(),
        "cli_version": None,
        "output_styles": set(),
        "api_key_sources": set(),
    }

    for trace in traces:
        for msg in trace.messages_received:
            # Look for SystemMessage with subtype "init"
            if msg.get("_type") == "SystemMessage" and msg.get("subtype") == "init":
                data = msg.get("data", {})

                # Extract tools
                if "tools" in data:
                    capabilities["tools"].update(data["tools"])

                # Extract permission mode
                if "permissionMode" in data:
                    capabilities["permission_modes"].add(data["permissionMode"])

                # Extract model
                if "model" in data:
                    capabilities["models"].add(data["model"])

                # Extract slash commands
                if "slash_commands" in data:
                    capabilities["slash_commands"].update(data["slash_commands"])

                # Extract agents
                if "agents" in data:
                    capabilities["agents"].update(data["agents"])

                # Extract skills
                if "skills" in data:
                    capabilities["skills"].update(data["skills"])

                # Extract plugins
                if "plugins" in data:
                    capabilities["plugins"].update(data["plugins"])

                # Extract CLI version
                if "claude_code_version" in data:
                    capabilities["cli_version"] = data["claude_code_version"]

                # Extract output style
                if "output_style" in data:
                    capabilities["output_styles"].add(data["output_style"])

                # Extract API key source
                if "apiKeySource" in data:
                    capabilities["api_key_sources"].add(data["apiKeySource"])

    # Convert sets to sorted lists for consistent output
    for key in [
        "tools",
        "permission_modes",
        "models",
        "slash_commands",
        "agents",
        "skills",
        "plugins",
        "output_styles",
        "api_key_sources",
    ]:
        if isinstance(capabilities[key], set):
            capabilities[key] = sorted(capabilities[key])

    return capabilities


# ============================================================================
# Documentation generation
# ============================================================================


def generate_fields_table(info: dict[str, Any]) -> str:
    """Generate a markdown table for type fields."""
    if "fields" not in info:
        return ""

    lines = [
        "| Field | Type | Required | Description |",
        "|-------|------|----------|-------------|",
    ]

    for fld in info["fields"]:
        required = "Yes" if fld["required"] else "No"
        field_type = f"`{fld['type']}`"
        lines.append(f"| `{fld['name']}` | {field_type} | {required} | |")

    return "\n".join(lines)


def format_json_example(data: Any, indent: int = 2) -> str:
    """Format data as a JSON example with paths sanitized."""
    # Sanitize paths before formatting to avoid leaking identifying information
    sanitized = sanitize_data(data)
    return json.dumps(sanitized, indent=indent, default=str)


def generate_markdown(
    tracer: ProtocolTracer,
    cli_captures: list[CLIInvocationCapture],
) -> str:
    """Generate the complete protocol specification markdown.

    Args:
        tracer: ProtocolTracer with captured traces
        cli_captures: List of CLI invocation captures
    """
    timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC")

    sections = []

    # Header
    sections.append(f"""# Claude CLI Protocol Specification

> **Generated**: {timestamp}
> **SDK Version**: {__version__}
> **CLI Version**: {__cli_version__}

This document specifies the exact wire protocol between the Claude Agent SDK for Python
and the Claude Code CLI. It is auto-generated from type definitions and captured protocol traces.

**See also:** [`protocol.proto`](protocol.proto) - Protocol Buffers definition with detailed comments

## Table of Contents

1. [CLI Invocation](#1-cli-invocation)
2. [Operating Modes](#2-operating-modes)
3. [Message Types](#3-message-types)
4. [Content Blocks](#4-content-blocks)
5. [Control Protocol](#5-control-protocol)
6. [Hook Protocol](#6-hook-protocol)
7. [Permission Protocol](#7-permission-protocol)
8. [MCP Server Configuration](#8-mcp-server-configuration)
9. [SDK Configuration](#9-sdk-configuration)

---
""")

    # Analyze CLI invocations
    cli_analysis = analyze_cli_invocations(cli_captures)

    # Extract CLI capabilities from traces
    cli_capabilities = extract_cli_capabilities_from_traces(tracer.traces)

    # Section 1: CLI Invocation
    sections.append("""## 1. CLI Invocation

### Command Line Arguments

The SDK invokes the Claude Code CLI with the following arguments.
These were discovered by capturing actual CLI invocations from the SDK:

| Flag | Example Values | Triggered By |
|------|----------------|--------------|""")

    # Sort flags for consistent output
    for flag in sorted(cli_analysis["flags"].keys()):
        info = cli_analysis["flags"][flag]
        values = info["values"]
        # Show up to 3 example values
        if values:
            values_str = ", ".join(f"`{v}`" for v in values[:3])
            if len(values) > 3:
                values_str += f" (+{len(values) - 3} more)"
        else:
            values_str = "(no value)"

        # Show which option configurations triggered this flag
        occurrences = info["occurrences"]
        if len(occurrences) == len(cli_captures):
            trigger_str = "Always"
        else:
            # Show first 2 triggers
            trigger_str = ", ".join(occurrences[:2])
            if len(occurrences) > 2:
                trigger_str += f" (+{len(occurrences) - 2} more)"

        sections.append(f"| `{flag}` | {values_str} | {trigger_str} |")

    sections.append("""
### Environment Variables

The SDK sets these environment variables when invoking the CLI:

| Variable | Example Values | Triggered By |
|----------|----------------|--------------|""")

    for var in sorted(cli_analysis["env_vars"].keys()):
        info = cli_analysis["env_vars"][var]
        values = info["values"]
        values_str = ", ".join(f"`{v}`" for v in values[:3])

        occurrences = info["occurrences"]
        if len(occurrences) == len(cli_captures):
            trigger_str = "Always"
        else:
            trigger_str = ", ".join(occurrences[:2])
            if len(occurrences) > 2:
                trigger_str += f" (+{len(occurrences) - 2} more)"

        sections.append(f"| `{var}` | {values_str} | {trigger_str} |")

    # Add CLI capabilities discovered from traces
    sections.append("""
### CLI Capabilities (Discovered from Traces)

The following capabilities were discovered from captured protocol traces via `SystemInitData`:
""")

    if cli_capabilities["tools"]:
        sections.append(f"\n**Available Tools** ({len(cli_capabilities['tools'])}):\n")
        sections.append("```")
        sections.append(", ".join(cli_capabilities["tools"]))
        sections.append("```\n")

    if cli_capabilities["permission_modes"]:
        sections.append(
            f"\n**Permission Modes Observed**: `{', '.join(cli_capabilities['permission_modes'])}`\n"
        )

    if cli_capabilities["models"]:
        sections.append(
            f"\n**Models Observed**: `{', '.join(cli_capabilities['models'])}`\n"
        )

    if cli_capabilities["slash_commands"]:
        sections.append(
            f"\n**Slash Commands** ({len(cli_capabilities['slash_commands'])}):\n"
        )
        sections.append("```")
        sections.append(", ".join(cli_capabilities["slash_commands"]))
        sections.append("```\n")

    # Add example CLI invocations section
    sections.append("""
### Example CLI Invocations

The following shows actual CLI commands built for various `ClaudeAgentOptions` configurations:
""")

    # Show a few representative examples
    example_captures = [
        c
        for c in cli_analysis["captures"]
        if c["name"]
        in [
            "minimal_string_mode",
            "minimal_streaming_mode",
            "with_model_options",
            "with_permission_mode",
        ]
    ]

    for capture in example_captures:
        sections.append(f"\n**{capture['name']}** ({capture['mode']} mode):\n")
        if capture["options"]:
            sections.append("Options:\n```json")
            sections.append(format_json_example(capture["options"]))
            sections.append("```\n")
        sections.append("Command:\n```bash")
        # Format command nicely
        cmd_str = " \\\n    ".join(capture["command"])
        sections.append(cmd_str)
        sections.append("```\n")

    if cli_capabilities["agents"]:
        sections.append(
            f"\n**Built-in Agents**: `{', '.join(cli_capabilities['agents'])}`\n"
        )

    if cli_capabilities["cli_version"]:
        sections.append(
            f"\n**CLI Version (from traces)**: `{cli_capabilities['cli_version']}`\n"
        )

    sections.append("\n---\n")

    # Section 2: Operating Modes
    sections.append("""## 2. Operating Modes

### String Mode (One-shot)

For single queries without bidirectional communication:

```bash
claude --output-format stream-json --verbose \\
       --system-prompt "..." \\
       --print -- "Your prompt here"
```

- Prompt passed as CLI argument after `--print --`
- stdin is closed immediately after process starts
- Unidirectional: responses only flow from CLI to SDK

### Streaming Mode (Interactive)

For interactive sessions with bidirectional communication:

```bash
claude --output-format stream-json --verbose \\
       --system-prompt "..." \\
       --input-format stream-json
```

- Uses `--input-format stream-json` flag
- stdin remains open for sending messages and control requests
- Bidirectional: supports control protocol, hooks, and permission callbacks

### Context Window Usage Tracking

The `ResultMessage.usage` field provides detailed token usage information:

```json
{
  "usage": {
    "input_tokens": 100,
    "cache_creation_input_tokens": 2000,
    "cache_read_input_tokens": 15000,
    "output_tokens": 50,
    "server_tool_use": {
      "web_search_requests": 0,
      "web_fetch_requests": 0
    },
    "service_tier": "standard",
    "cache_creation": {
      "ephemeral_1h_input_tokens": 0,
      "ephemeral_5m_input_tokens": 333
    }
  }
}
```

Key fields:
- `input_tokens`: New tokens sent to the model
- `cache_creation_input_tokens`: Tokens used to create cache
- `cache_read_input_tokens`: Tokens read from cache
- `output_tokens`: Tokens generated by the model

Total context used = `input_tokens` + `cache_read_input_tokens` (cache creation is separate)

### Planner Mode

To enter planner mode, set `permission_mode` to `"plan"`:

```python
options = ClaudeAgentOptions(permission_mode="plan")
```

In plan mode:
- Claude reviews the plan before executing
- Use `set_permission_mode` control request to switch modes dynamically

---
""")

    # Section 3: Message Types
    sections.append("""## 3. Message Types

All messages are JSON objects sent as newline-delimited JSON (NDJSON) over stdout.
The `type` field discriminates between message types.

### Message Union

```
Message = UserMessage | AssistantMessage | SystemMessage | ResultMessage | StreamEvent
```
""")

    for name, typ in MESSAGE_TYPES:
        info = extract_type_info(typ)
        sections.append(f"\n### {name}\n")
        if info.get("doc"):
            sections.append(f"{info['doc']}\n")
        sections.append(generate_fields_table(info))

    sections.append("\n---\n")

    # Section 4: Content Blocks
    sections.append("""## 4. Content Blocks

Content blocks appear in `UserMessage.content` and `AssistantMessage.content`.
The `type` field (on the wire, not the Python class) discriminates block types.

### ContentBlock Union

```
ContentBlock = TextBlock | ThinkingBlock | ToolUseBlock | ToolResultBlock
```
""")

    for name, typ in CONTENT_BLOCK_TYPES:
        info = extract_type_info(typ)
        sections.append(f"\n### {name}\n")
        if info.get("doc"):
            sections.append(f"{info['doc']}\n")
        sections.append(generate_fields_table(info))

    sections.append("\n---\n")

    # Section 5: Control Protocol
    sections.append("""## 5. Control Protocol

The control protocol enables bidirectional communication between SDK and CLI.
Control messages are sent via stdin (SDK to CLI) or stdout (CLI to SDK).

### Control Request Wrapper

All control requests are wrapped in `SDKControlRequest`:

```json
{
  "type": "control_request",
  "request_id": "req_1_abc123",
  "request": {
    "subtype": "<request_type>",
    ...
  }
}
```

### Control Response Wrapper

All control responses are wrapped in `SDKControlResponse`:

```json
{
  "type": "control_response",
  "response": {
    "subtype": "success",
    "request_id": "req_1_abc123",
    "response": { ... }
  }
}
```

### Request ID Format

Request IDs follow the pattern: `req_{counter}_{random_hex}`
- `counter`: Incrementing integer per session
- `random_hex`: 4 bytes of random hex for uniqueness

### Control Request Types
""")

    for name, typ in CONTROL_REQUEST_TYPES:
        info = extract_type_info(typ)
        sections.append(f"\n#### {name}\n")
        if info.get("doc"):
            sections.append(f"{info['doc']}\n")
        sections.append(generate_fields_table(info))

    sections.append("\n### Control Response Types\n")

    for name, typ in CONTROL_RESPONSE_TYPES:
        info = extract_type_info(typ)
        sections.append(f"\n#### {name}\n")
        sections.append(generate_fields_table(info))

    sections.append("\n---\n")

    # Section 6: Hook Protocol
    sections.append("""## 6. Hook Protocol

Hooks allow the SDK to intercept and modify CLI behavior at specific points.

### Hook Events

```python
HookEvent = "PreToolUse" | "PostToolUse" | "UserPromptSubmit" | "Stop" | "SubagentStop" | "PreCompact"
```

### Hook Input Types

The `hook_event_name` field discriminates between input types.
""")

    for name, typ in HOOK_INPUT_TYPES:
        info = extract_type_info(typ)
        sections.append(f"\n#### {name}\n")
        if info.get("doc"):
            sections.append(f"{info['doc']}\n")
        sections.append(generate_fields_table(info))

    sections.append("""
### Hook Output Types

**Important**: Python uses `async_` and `continue_` (with underscores) to avoid
keyword conflicts. These are automatically converted to `async` and `continue`
when sent to the CLI.
""")

    for name, typ in HOOK_OUTPUT_TYPES:
        info = extract_type_info(typ)
        sections.append(f"\n#### {name}\n")
        if info.get("doc"):
            sections.append(f"{info['doc']}\n")
        sections.append(generate_fields_table(info))

    sections.append("\n---\n")

    # Section 7: Permission Protocol
    sections.append("""## 7. Permission Protocol

The permission system controls tool execution approval.

### Permission Result Types
""")

    for name, typ in PERMISSION_TYPES:
        info = extract_type_info(typ)
        sections.append(f"\n#### {name}\n")
        if info.get("doc"):
            sections.append(f"{info['doc']}\n")
        sections.append(generate_fields_table(info))

    sections.append("""
### Permission Modes

```python
PermissionMode = "default" | "acceptEdits" | "plan" | "bypassPermissions"
```

| Mode | Description |
|------|-------------|
| `default` | CLI prompts user for dangerous tool operations |
| `acceptEdits` | Auto-accept file modifications |
| `plan` | Review plan before execution |
| `bypassPermissions` | Allow all tools (dangerous!) |
""")

    sections.append("\n---\n")

    # Section 8: MCP Server Configuration
    sections.append("""## 8. MCP Server Configuration

The SDK supports multiple MCP server transport types.

### Server Config Union

```
McpServerConfig = McpStdioServerConfig | McpSSEServerConfig | McpHttpServerConfig | McpSdkServerConfig
```
""")

    for name, typ in MCP_SERVER_TYPES:
        info = extract_type_info(typ)
        sections.append(f"\n#### {name}\n")
        if info.get("doc"):
            sections.append(f"{info['doc']}\n")
        sections.append(generate_fields_table(info))

    sections.append("\n---\n")

    # Section 9: SDK Configuration
    sections.append("""## 9. SDK Configuration

### ClaudeAgentOptions

The main configuration object for SDK queries.
""")

    for name, typ in CONFIG_TYPES:
        info = extract_type_info(typ)
        sections.append(f"\n#### {name}\n")
        if info.get("doc"):
            sections.append(f"{info['doc']}\n")
        sections.append(generate_fields_table(info))

    return "\n".join(sections)


def type_to_json_schema(typ: Any, definitions: dict[str, Any]) -> dict[str, Any]:
    """Convert a Python type to JSON Schema."""
    if typ is type(None):
        return {"type": "null"}
    if typ is str:
        return {"type": "string"}
    if typ is int:
        return {"type": "integer"}
    if typ is float:
        return {"type": "number"}
    if typ is bool:
        return {"type": "boolean"}
    if typ is Any:
        return {}

    origin = get_origin(typ)

    if origin is Literal:
        values = list(get_args(typ))
        if len(values) == 1:
            return {"const": values[0]}
        return {"enum": values}

    if origin is Union:
        args = get_args(typ)
        schemas = [type_to_json_schema(a, definitions) for a in args]
        return {"oneOf": schemas}

    if origin is list:
        args = get_args(typ)
        if args:
            return {"type": "array", "items": type_to_json_schema(args[0], definitions)}
        return {"type": "array"}

    if origin is dict:
        args = get_args(typ)
        if args and len(args) == 2:
            return {
                "type": "object",
                "additionalProperties": type_to_json_schema(args[1], definitions),
            }
        return {"type": "object"}

    if is_dataclass(typ) or is_typeddict(typ):
        type_name = typ.__name__
        if type_name not in definitions:
            definitions[type_name] = {}
            info = extract_type_info(typ)
            schema: dict[str, Any] = {
                "type": "object",
                "properties": {},
                "required": [],
            }
            for fld in info.get("fields", []):
                raw_type = fld.get("raw_type", str)
                if is_not_required(raw_type):
                    raw_type = unwrap_not_required(raw_type)
                schema["properties"][fld["name"]] = type_to_json_schema(
                    raw_type, definitions
                )
                if fld["required"]:
                    schema["required"].append(fld["name"])
            if not schema["required"]:
                del schema["required"]
            definitions[type_name] = schema
        return {"$ref": f"#/$defs/{type_name}"}

    return {"type": "string", "description": f"Unknown type: {typ}"}


def generate_json_schema_file(
    title: str, types_list: list[tuple[str, type]], filename: str
) -> dict[str, Any]:
    """Generate a JSON Schema file for a list of types."""
    definitions: dict[str, Any] = {}

    for _name, typ in types_list:
        type_to_json_schema(typ, definitions)

    schema = {
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "$id": f"https://anthropic.com/claude-agent-sdk/{filename}",
        "title": title,
        "description": f"Generated from claude-agent-sdk v{__version__}",
        "$defs": definitions,
    }

    if len(types_list) > 1:
        schema["oneOf"] = [{"$ref": f"#/$defs/{name}"} for name, _ in types_list]

    return schema


def generate_all_schemas() -> dict[str, dict[str, Any]]:
    """Generate all JSON Schema files."""
    schemas = {}

    schemas["message.schema.json"] = generate_json_schema_file(
        "Claude SDK Message Types", MESSAGE_TYPES, "message.schema.json"
    )

    all_control_types = [
        ("SDKControlRequest", types.SDKControlRequest),
        ("SDKControlResponse", types.SDKControlResponse),
        *CONTROL_REQUEST_TYPES,
        *CONTROL_RESPONSE_TYPES,
    ]
    schemas["control-protocol.schema.json"] = generate_json_schema_file(
        "Claude SDK Control Protocol", all_control_types, "control-protocol.schema.json"
    )

    all_hook_types = [*HOOK_INPUT_TYPES, *HOOK_OUTPUT_TYPES]
    schemas["hooks.schema.json"] = generate_json_schema_file(
        "Claude SDK Hook Types", all_hook_types, "hooks.schema.json"
    )

    return schemas


# ============================================================================
# Protocol Buffers generation
# ============================================================================


def type_to_proto_type(typ: Any) -> tuple[str, bool]:
    """Convert a Python type to a Protocol Buffers type.

    Returns (proto_type, is_optional).
    """
    if typ is type(None):
        return "google.protobuf.NullValue", False
    if typ is str:
        return "string", False
    if typ is int:
        return "int64", False
    if typ is float:
        return "double", False
    if typ is bool:
        return "bool", False
    if typ is Any:
        return "google.protobuf.Value", False

    origin = get_origin(typ)

    if origin is Literal:
        # Literals become enums or strings
        values = get_args(typ)
        if all(isinstance(v, str) for v in values):
            return "string", False
        return "string", False

    if origin is Union:
        args = get_args(typ)
        non_none = [a for a in args if a is not type(None)]
        if len(non_none) < len(args):
            # Optional type
            if len(non_none) == 1:
                inner_type, _ = type_to_proto_type(non_none[0])
                return inner_type, True  # Mark as optional
            return "google.protobuf.Value", True
        # Union of multiple types - use oneof or Value
        return "google.protobuf.Value", False

    if origin is list:
        args = get_args(typ)
        if args:
            inner_type, _ = type_to_proto_type(args[0])
            return f"repeated {inner_type}", False
        return "repeated google.protobuf.Value", False

    if origin is dict:
        args = get_args(typ)
        if args and len(args) == 2:
            key_type, _ = type_to_proto_type(args[0])
            value_type, _ = type_to_proto_type(args[1])
            return f"map<{key_type}, {value_type}>", False
        return "map<string, google.protobuf.Value>", False

    if hasattr(typ, "__name__"):
        return typ.__name__, False

    return "google.protobuf.Value", False


def generate_proto_message(name: str, info: dict[str, Any], indent: int = 0) -> list[str]:
    """Generate a protobuf message definition from type info."""
    lines = []
    prefix = "  " * indent

    if info.get("doc"):
        # Add documentation as comments
        doc_lines = info["doc"].strip().split("\n")
        for doc_line in doc_lines:
            lines.append(f"{prefix}// {doc_line.strip()}")

    lines.append(f"{prefix}message {name} {{")

    field_num = 1
    for fld in info.get("fields", []):
        raw_type = fld.get("raw_type", str)
        if is_not_required(raw_type):
            raw_type = unwrap_not_required(raw_type)

        proto_type, is_optional = type_to_proto_type(raw_type)

        # Handle repeated types
        if proto_type.startswith("repeated "):
            lines.append(f"{prefix}  {proto_type} {fld['name']} = {field_num};")
        elif proto_type.startswith("map<"):
            lines.append(f"{prefix}  {proto_type} {fld['name']} = {field_num};")
        elif is_optional or not fld["required"]:
            lines.append(f"{prefix}  optional {proto_type} {fld['name']} = {field_num};")
        else:
            lines.append(f"{prefix}  {proto_type} {fld['name']} = {field_num};")

        field_num += 1

    lines.append(f"{prefix}}}")
    return lines


def generate_protobuf_file() -> str:
    """Generate the complete protocol.proto file."""
    lines = [
        '// Claude Agent SDK Protocol Specification',
        '// Auto-generated from Python type definitions',
        f'// SDK Version: {__version__}',
        f'// CLI Version: {__cli_version__}',
        '',
        'syntax = "proto3";',
        '',
        'package claude.agent.sdk;',
        '',
        'import "google/protobuf/struct.proto";',
        '',
        '// ============================================================================',
        '// Message Types',
        '// ============================================================================',
        '',
    ]

    # Generate message types
    for name, typ in MESSAGE_TYPES:
        info = extract_type_info(typ)
        lines.extend(generate_proto_message(name, info))
        lines.append('')

    lines.extend([
        '// ============================================================================',
        '// Content Block Types',
        '// ============================================================================',
        '',
    ])

    for name, typ in CONTENT_BLOCK_TYPES:
        info = extract_type_info(typ)
        lines.extend(generate_proto_message(name, info))
        lines.append('')

    lines.extend([
        '// ============================================================================',
        '// Control Protocol Types',
        '// ============================================================================',
        '',
    ])

    for name, typ in CONTROL_REQUEST_TYPES:
        info = extract_type_info(typ)
        lines.extend(generate_proto_message(name, info))
        lines.append('')

    for name, typ in CONTROL_RESPONSE_TYPES:
        info = extract_type_info(typ)
        lines.extend(generate_proto_message(name, info))
        lines.append('')

    lines.extend([
        '// ============================================================================',
        '// Hook Types',
        '// ============================================================================',
        '',
    ])

    for name, typ in HOOK_INPUT_TYPES:
        info = extract_type_info(typ)
        lines.extend(generate_proto_message(name, info))
        lines.append('')

    for name, typ in HOOK_OUTPUT_TYPES:
        info = extract_type_info(typ)
        lines.extend(generate_proto_message(name, info))
        lines.append('')

    lines.extend([
        '// ============================================================================',
        '// Permission Types',
        '// ============================================================================',
        '',
    ])

    for name, typ in PERMISSION_TYPES:
        info = extract_type_info(typ)
        lines.extend(generate_proto_message(name, info))
        lines.append('')

    lines.extend([
        '// ============================================================================',
        '// MCP Server Configuration Types',
        '// ============================================================================',
        '',
    ])

    for name, typ in MCP_SERVER_TYPES:
        info = extract_type_info(typ)
        lines.extend(generate_proto_message(name, info))
        lines.append('')

    lines.extend([
        '// ============================================================================',
        '// SDK Configuration Types',
        '// ============================================================================',
        '',
    ])

    for name, typ in CONFIG_TYPES:
        info = extract_type_info(typ)
        lines.extend(generate_proto_message(name, info))
        lines.append('')

    return '\n'.join(lines)


def get_output_dir() -> Path:
    """Get the output directory for generated docs.

    Uses the current working directory's docs/ subdirectory.
    """
    return Path.cwd() / "docs"


async def main_async(args: argparse.Namespace) -> int:
    """Async main function."""
    docs_dir = get_output_dir()
    traces_dir = docs_dir / "traces"
    schemas_dir = docs_dir / "schemas"
    md_path = docs_dir / "PROTOCOL_SPECIFICATION.md"
    proto_path = docs_dir / "protocol.proto"

    tracer = ProtocolTracer(traces_dir)

    # Capture CLI invocations (doesn't require running CLI)
    cli_captures = capture_cli_invocations_for_various_options()
    cli_invocations = [
        {
            "name": c.options_name,
            "mode": c.mode,
            "options": c.options,
            "command": c.command,
            "env_vars": c.env_vars,
        }
        for c in cli_captures
    ]

    # Capture or load traces
    # In check mode, always use existing traces (don't recapture)
    use_existing_traces = args.no_capture or args.check

    if not use_existing_traces:
        await capture_protocol_traces(tracer, model=args.model)
        trace_path = tracer.save_traces(cli_invocations=cli_invocations)
        print(f"Saved traces to: {trace_path}")
    else:
        if not tracer.load_traces():
            print(
                "No existing traces found. Run without --no-capture to generate them."
            )
            return 1
        print(f"Loaded {len(tracer.traces)} existing traces")

    # Generate content
    markdown_content = generate_markdown(tracer, cli_captures)
    schemas = {} if args.no_json_schema else generate_all_schemas()
    proto_content = "" if args.no_protobuf else generate_protobuf_file()

    if args.check:
        all_match = True

        if md_path.exists():
            existing_md = md_path.read_text()
            # Skip timestamp comparison (first few lines)
            existing_lines = existing_md.split("\n")[6:]
            new_lines = markdown_content.split("\n")[6:]
            if existing_lines != new_lines:
                print(f"MISMATCH: {md_path}")
                all_match = False
        else:
            print(f"MISSING: {md_path}")
            all_match = False

        for filename, schema in schemas.items():
            schema_path = schemas_dir / filename
            if schema_path.exists():
                existing_schema = json.loads(schema_path.read_text())
                if existing_schema != schema:
                    print(f"MISMATCH: {schema_path}")
                    all_match = False
            else:
                print(f"MISSING: {schema_path}")
                all_match = False

        if proto_content:
            if proto_path.exists():
                existing_proto = proto_path.read_text()
                # Skip version comments (first 4 lines) for comparison
                existing_lines = existing_proto.split("\n")[4:]
                new_lines = proto_content.split("\n")[4:]
                if existing_lines != new_lines:
                    print(f"MISMATCH: {proto_path}")
                    all_match = False
            else:
                print(f"MISSING: {proto_path}")
                all_match = False

        if all_match:
            print("All protocol docs are up to date.")
            return 0
        else:
            print("\nProtocol docs need regeneration. Run:")
            print("  generate-protocol-docs")
            return 1

    # Write mode
    md_path.parent.mkdir(parents=True, exist_ok=True)
    md_path.write_text(markdown_content)
    print(f"Generated: {md_path}")

    if schemas:
        schemas_dir.mkdir(parents=True, exist_ok=True)
        for filename, schema in schemas.items():
            schema_path = schemas_dir / filename
            schema_path.write_text(json.dumps(schema, indent=2) + "\n")
            print(f"Generated: {schema_path}")

    if proto_content:
        proto_path.write_text(proto_content)
        print(f"Generated: {proto_path}")

    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Generate protocol documentation by running CLI and capturing traces"
    )
    parser.add_argument(
        "--check", action="store_true", help="Check if docs are up to date"
    )
    parser.add_argument(
        "--no-capture",
        action="store_true",
        help="Skip trace capture, use existing traces",
    )
    parser.add_argument(
        "--no-json-schema", action="store_true", help="Skip JSON Schema generation"
    )
    parser.add_argument(
        "--no-protobuf", action="store_true", help="Skip Protocol Buffers generation"
    )
    parser.add_argument(
        "--model",
        default="haiku",
        help="Model to use for trace capture (default: haiku for cost efficiency)",
    )
    args = parser.parse_args()

    return asyncio.run(main_async(args))


if __name__ == "__main__":
    import sys
    sys.exit(main())
