#!/usr/bin/env python3
"""Capture the full protocol trace for custom Python tool calls.

This script captures both the message-level trace AND the hidden MCP
control protocol messages (initialize, tools/list, tools/call) that
flow between the CLI and SDK during custom tool execution.

The existing ProtocolTracer in protocol/src/claude_cli_protocol/generate.py
only captures the message layer. This script captures the control protocol
layer too, by monkey-patching Query._handle_control_request.

Usage:
    pip install claude-agent-sdk
    python capture_trace.py
"""

from __future__ import annotations

import asyncio
import json
import re
import sys
import tempfile
from copy import deepcopy
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from claude_agent_sdk import (
    ClaudeSDKClient,
    create_sdk_mcp_server,
    tool,
    types,
)
from claude_agent_sdk._version import __version__


# ---------------------------------------------------------------------------
# Path sanitization (reused from protocol/src/claude_cli_protocol/generate.py)
# ---------------------------------------------------------------------------

def sanitize_path(value: str) -> str:
    """Replace absolute paths with placeholders to avoid leaking identifying info."""
    if not isinstance(value, str):
        return value
    value = re.sub(r"/Users/[^/]+/", "/Users/<user>/", value)
    value = re.sub(r"/home/[^/]+/", "/home/<user>/", value)
    value = re.sub(r"C:\\\\Users\\\\[^\\\\]+\\\\", "C:\\\\Users\\\\<user>\\\\", value)
    value = re.sub(r"/tmp/tmp[a-zA-Z0-9_]+", "/tmp/<tmpdir>", value)
    value = re.sub(r"/var/folders/[^/]+/[^/]+/T/tmp[a-zA-Z0-9_]+", "/tmp/<tmpdir>", value)
    value = re.sub(r"/private/tmp/tmp[a-zA-Z0-9_]+", "/tmp/<tmpdir>", value)
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


# ---------------------------------------------------------------------------
# Message serialization
# ---------------------------------------------------------------------------

def message_to_dict(msg: Any) -> dict[str, Any]:
    """Convert a message object to a serializable dict."""
    from dataclasses import fields, is_dataclass

    if is_dataclass(msg) and not isinstance(msg, type):
        result: dict[str, Any] = {"_type": type(msg).__name__}
        for f in fields(msg):
            value = getattr(msg, f.name)
            if is_dataclass(value) and not isinstance(value, type):
                result[f.name] = message_to_dict(value)
            elif isinstance(value, list):
                result[f.name] = [
                    message_to_dict(v) if (is_dataclass(v) and not isinstance(v, type)) else v
                    for v in value
                ]
            else:
                result[f.name] = value
        return result
    return {"_raw": str(msg)}


# ---------------------------------------------------------------------------
# Define a custom tool
# ---------------------------------------------------------------------------

@tool("greet", "Greet someone by name", {"name": str})
async def greet_tool(args: dict[str, Any]) -> dict[str, Any]:
    """A simple greeting tool for protocol analysis."""
    return {
        "content": [{"type": "text", "text": f"Hello, {args['name']}! Welcome."}]
    }


# ---------------------------------------------------------------------------
# Control request interceptor
# ---------------------------------------------------------------------------

class ControlMessageCapture:
    """Captures control protocol messages by monkey-patching Query._handle_control_request."""

    def __init__(self) -> None:
        self.captured: list[dict[str, Any]] = []
        self._original_handler: Any = None
        self._patched = False

    def install(self) -> None:
        """Install the monkey-patch on Query._handle_control_request."""
        from claude_agent_sdk._internal.query import Query

        self._original_handler = Query._handle_control_request

        capture = self  # closure reference

        async def patched_handler(query_self: Any, request: dict[str, Any]) -> None:
            """Wrapper that captures control requests before/after processing."""
            request_id = request.get("request_id", "")
            request_data = request.get("request", {})
            subtype = request_data.get("subtype", "")

            # Capture the incoming request (CLI -> SDK)
            entry: dict[str, Any] = {
                "direction": "cli_to_sdk",
                "request_id": request_id,
                "subtype": subtype,
                "request": deepcopy(request_data),
                "timestamp": datetime.now(timezone.utc).isoformat(),
            }

            # For mcp_message, extract the JSONRPC details
            if subtype == "mcp_message":
                entry["server_name"] = request_data.get("server_name")
                entry["jsonrpc_method"] = request_data.get("message", {}).get("method")

            capture.captured.append(entry)

            # Call original handler (which sends the response)
            await capture._original_handler(query_self, request)

            # Note: The response is written directly to transport by the original
            # handler, so we can't easily capture it here without also patching
            # transport.write. Instead, we reconstruct what we know about the
            # response from the request type.
            capture.captured.append({
                "direction": "sdk_to_cli",
                "request_id": request_id,
                "subtype": subtype,
                "note": "response sent (see _handle_control_request source for format)",
                "timestamp": datetime.now(timezone.utc).isoformat(),
            })

        Query._handle_control_request = patched_handler  # type: ignore[assignment]
        self._patched = True

    def install_transport_capture(self) -> None:
        """Also patch transport.write to capture actual response payloads."""
        from claude_agent_sdk._internal.query import Query

        original_handler = self._original_handler
        capture = self

        async def patched_handler_v2(query_self: Any, request: dict[str, Any]) -> None:
            """Wrapper that captures both request and response payloads."""
            request_id = request.get("request_id", "")
            request_data = request.get("request", {})
            subtype = request_data.get("subtype", "")

            # Capture the incoming request (CLI -> SDK)
            entry: dict[str, Any] = {
                "direction": "cli_to_sdk",
                "request_id": request_id,
                "subtype": subtype,
                "request": deepcopy(request_data),
                "timestamp": datetime.now(timezone.utc).isoformat(),
            }
            if subtype == "mcp_message":
                entry["server_name"] = request_data.get("server_name")
                entry["jsonrpc_method"] = request_data.get("message", {}).get("method")
            capture.captured.append(entry)

            # Temporarily wrap transport.write to capture the response
            original_write = query_self.transport.write
            response_payload: dict[str, Any] | None = None

            async def capturing_write(data: str) -> None:
                nonlocal response_payload
                try:
                    parsed = json.loads(data.strip())
                    if (
                        parsed.get("type") == "control_response"
                        and parsed.get("response", {}).get("request_id") == request_id
                    ):
                        response_payload = deepcopy(parsed)
                except (json.JSONDecodeError, AttributeError):
                    pass
                await original_write(data)

            query_self.transport.write = capturing_write
            try:
                await original_handler(query_self, request)
            finally:
                query_self.transport.write = original_write

            # Capture the response (SDK -> CLI)
            resp_entry: dict[str, Any] = {
                "direction": "sdk_to_cli",
                "request_id": request_id,
                "subtype": subtype,
                "timestamp": datetime.now(timezone.utc).isoformat(),
            }
            if response_payload:
                resp_entry["response"] = response_payload
            capture.captured.append(resp_entry)

        Query._handle_control_request = patched_handler_v2  # type: ignore[assignment]

    def uninstall(self) -> None:
        """Restore original handler."""
        if self._patched and self._original_handler:
            from claude_agent_sdk._internal.query import Query
            Query._handle_control_request = self._original_handler
            self._patched = False


# ---------------------------------------------------------------------------
# Main capture logic
# ---------------------------------------------------------------------------

async def capture_custom_tool_trace() -> dict[str, Any]:
    """Run a custom tool query and capture the full protocol trace."""

    # Create SDK MCP server
    demo_server = create_sdk_mcp_server("demo_tools", tools=[greet_tool])

    # Permission callback captures
    permission_events: list[dict[str, Any]] = []

    async def can_use_tool(
        tool_name: str, tool_input: dict[str, Any], context: Any
    ) -> types.PermissionResult:
        """Permission callback that captures the request and allows everything."""
        permission_events.append({
            "tool_name": tool_name,
            "input": deepcopy(tool_input),
            "timestamp": datetime.now(timezone.utc).isoformat(),
        })
        return types.PermissionResultAllow()

    # Install control message capture
    control_capture = ControlMessageCapture()
    control_capture.install()
    control_capture.install_transport_capture()

    # Prepare options
    with tempfile.TemporaryDirectory() as tmpdir:
        options = types.ClaudeAgentOptions(
            max_turns=2,
            model="haiku",
            cwd=tmpdir,
            mcp_servers={"demo_tools": demo_server},
            can_use_tool=can_use_tool,
        )

        # Collect messages
        messages_sent: list[dict[str, Any]] = []
        messages_received: list[dict[str, Any]] = []

        user_message = {
            "type": "user",
            "message": {
                "role": "user",
                "content": "Use the greet tool with name 'Alice'",
            },
        }

        print("Starting custom tool trace capture...")
        print(f"  SDK version: {__version__}")
        print(f"  Model: haiku")
        print(f"  Tool: greet (via demo_tools MCP server)")
        print()

        try:
            client = ClaudeSDKClient(options)

            async def message_stream():
                messages_sent.append(deepcopy(user_message))
                yield user_message

            await client.connect(message_stream())

            try:
                async for msg in client.receive_messages():
                    messages_received.append(message_to_dict(msg))
            finally:
                await client.disconnect()

        except Exception as e:
            print(f"Error during capture: {e}", file=sys.stderr)
            messages_received.append({"_type": "Error", "error": str(e)})

        finally:
            control_capture.uninstall()

    # Build the trace
    trace = {
        "description": "Full protocol trace for custom Python tool call via claude-agent-sdk",
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "sdk_version": __version__,
        "tool_definition": {
            "name": "greet",
            "description": "Greet someone by name",
            "input_schema": {"name": "str"},
            "server_name": "demo_tools",
            "tool_wire_name": "mcp__demo_tools__greet",
        },
        "messages_sent": messages_sent,
        "messages_received": messages_received,
        "control_messages": control_capture.captured,
        "permission_events": permission_events,
    }

    return trace


def print_trace_summary(trace: dict[str, Any]) -> None:
    """Print a human-readable summary of the captured trace."""
    print("=" * 70)
    print("TRACE SUMMARY")
    print("=" * 70)
    print(f"SDK Version: {trace['sdk_version']}")
    print(f"Generated: {trace['generated_at']}")
    print()

    # Messages
    print(f"Messages sent: {len(trace['messages_sent'])}")
    print(f"Messages received: {len(trace['messages_received'])}")
    for msg in trace["messages_received"]:
        msg_type = msg.get("_type", "unknown")
        if msg_type == "SystemMessage":
            tools = msg.get("data", {}).get("tools", [])
            mcp_servers = msg.get("data", {}).get("mcp_servers", [])
            print(f"  - {msg_type}/init: {len(tools)} tools, {len(mcp_servers)} MCP servers")
            custom_tools = [t for t in tools if t.startswith("mcp__")]
            if custom_tools:
                print(f"    Custom tools: {custom_tools}")
        elif msg_type == "AssistantMessage":
            content = msg.get("content", [])
            for block in content:
                block_type = block.get("_type", "unknown")
                if block_type == "ToolUseBlock":
                    print(f"  - {msg_type}/ToolUse: {block.get('name')} -> {json.dumps(block.get('input', {}))}")
                elif block_type == "TextBlock":
                    text = block.get("text", "")
                    print(f"  - {msg_type}/Text: {text[:80]}{'...' if len(text) > 80 else ''}")
        elif msg_type == "UserMessage":
            content = msg.get("content", [])
            for block in content:
                block_type = block.get("_type", "unknown")
                if block_type == "ToolResultBlock":
                    result_content = block.get("content", [])
                    result_text = result_content[0].get("text", "") if result_content else ""
                    print(f"  - {msg_type}/ToolResult: {result_text}")
        elif msg_type == "ResultMessage":
            print(f"  - {msg_type}: cost=${msg.get('total_cost_usd', 0):.6f}, turns={msg.get('num_turns', 0)}")
        else:
            print(f"  - {msg_type}")

    # Control messages
    print()
    print(f"Control messages captured: {len(trace['control_messages'])}")
    for cm in trace["control_messages"]:
        direction = cm.get("direction", "?")
        subtype = cm.get("subtype", "?")
        arrow = "->" if direction == "cli_to_sdk" else "<-"
        if subtype == "mcp_message":
            method = cm.get("jsonrpc_method", "?")
            server = cm.get("server_name", "?")
            if direction == "cli_to_sdk":
                print(f"  CLI {arrow} SDK: mcp_message [{server}] method={method}")
            else:
                # Show response payload if available
                resp = cm.get("response", {})
                resp_data = resp.get("response", {}).get("response", {})
                mcp_resp = resp_data.get("mcp_response", {})
                if mcp_resp:
                    result_keys = list(mcp_resp.get("result", {}).keys()) if isinstance(mcp_resp.get("result"), dict) else []
                    print(f"  SDK {arrow} CLI: mcp_message response (result keys: {result_keys})")
                else:
                    print(f"  SDK {arrow} CLI: mcp_message response")
        elif subtype == "can_use_tool":
            if direction == "cli_to_sdk":
                tool_name = cm.get("request", {}).get("tool_name", "?")
                print(f"  CLI {arrow} SDK: can_use_tool [{tool_name}]")
            else:
                print(f"  SDK {arrow} CLI: can_use_tool response (allow)")
        else:
            print(f"  {'CLI' if direction == 'cli_to_sdk' else 'SDK'} {arrow} {'SDK' if direction == 'cli_to_sdk' else 'CLI'}: {subtype}")

    # Permission events
    print()
    print(f"Permission events: {len(trace['permission_events'])}")
    for pe in trace["permission_events"]:
        print(f"  - can_use_tool: {pe['tool_name']} with input {json.dumps(pe['input'])}")


def main() -> None:
    """Main entry point."""
    trace = asyncio.run(capture_custom_tool_trace())

    # Sanitize paths
    trace = sanitize_data(trace)

    # Save trace
    output_dir = Path(__file__).parent / "traces"
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / "custom_tool_trace.json"
    output_path.write_text(json.dumps(trace, indent=2, default=str) + "\n")
    print(f"Trace saved to: {output_path}")
    print()

    # Print summary
    print_trace_summary(trace)


if __name__ == "__main__":
    main()
