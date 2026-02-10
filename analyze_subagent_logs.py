#!/usr/bin/env python3
"""
Analyze Claude Code multi-agent (team/swarm) JSONL logs.

Reads the subagents-logs/ directory and produces a report covering:
  - Agent inventory and roles
  - Full chronological timeline
  - Tool usage statistics
  - Task lifecycle tracking
  - Communication graph
  - Parent orchestration pattern analysis
"""

import json
import glob
import re
import os
import sys
from collections import Counter, defaultdict
from datetime import datetime

LOG_DIR = os.path.join(os.path.dirname(__file__), "subagents-logs")


# ── helpers ──────────────────────────────────────────────────────────────────

def parse_timestamp(ts: str) -> datetime | None:
    if not ts:
        return None
    try:
        return datetime.fromisoformat(ts.replace("Z", "+00:00"))
    except Exception:
        return None


def extract_tool_calls(entry: dict) -> list[dict]:
    """Return list of {name, input} from an assistant message."""
    tools = []
    msg = entry.get("message", {})
    content = msg.get("content", [])
    if not isinstance(content, list):
        return tools
    for block in content:
        if isinstance(block, dict) and block.get("type") == "tool_use":
            tools.append({
                "name": block.get("name"),
                "input": block.get("input", {}),
                "id": block.get("id", ""),
            })
    return tools


def extract_text(entry: dict) -> str:
    """Return concatenated text blocks from an assistant message."""
    msg = entry.get("message", {})
    content = msg.get("content", [])
    if isinstance(content, str):
        return content
    parts = []
    if isinstance(content, list):
        for block in content:
            if isinstance(block, dict) and block.get("type") == "text":
                parts.append(block.get("text", ""))
    return "\n".join(parts)


def infer_agent_role(entries: list[dict], agent_id: str) -> str:
    """Try to determine the role/name of an agent from its first message."""
    for e in entries:
        if e.get("type") != "user":
            continue
        msg = e.get("message", {})
        content = msg.get("content", "")
        if isinstance(content, str):
            text = content
        elif isinstance(content, list):
            text = " ".join(
                b.get("text", "") for b in content if isinstance(b, dict) and b.get("type") == "text"
            )
        else:
            continue

        # Check for teammate-message with named role
        m = re.search(r'You are "([^"]+)"', text)
        if m:
            return m.group(1)

        # Shutdown request
        if "shutdown_request" in text:
            m2 = re.search(r'@([\w-]+)', text)
            return f"shutdown-handler({m2.group(1)})" if m2 else "shutdown-handler"

        # Task re-assignment
        if "task_assignment" in text:
            m3 = re.search(r'"subject"\s*:\s*"([^"]+)"', text)
            return f"task-reassign({m3.group(1)[:40]})" if m3 else "task-reassign"

    return f"agent-{agent_id}"


# ── loading ──────────────────────────────────────────────────────────────────

def load_jsonl(path: str) -> list[dict]:
    entries = []
    with open(path) as f:
        for line in f:
            line = line.strip()
            if line:
                try:
                    entries.append(json.loads(line))
                except json.JSONDecodeError:
                    pass
    return entries


def discover_logs(log_dir: str) -> dict:
    """Return {label: [entries]} for the main session and all subagents."""
    logs = {}

    # Find main session log (*.jsonl at top level, not inside subagents/)
    for path in sorted(glob.glob(os.path.join(log_dir, "*.jsonl"))):
        session_id = os.path.basename(path).replace(".jsonl", "")
        logs["main"] = {
            "path": path,
            "session_id": session_id,
            "entries": load_jsonl(path),
        }

        # Find subagent logs
        subdir = os.path.join(log_dir, session_id, "subagents")
        if os.path.isdir(subdir):
            for sp in sorted(glob.glob(os.path.join(subdir, "*.jsonl"))):
                agent_file = os.path.basename(sp)
                agent_id = agent_file.replace("agent-", "").replace(".jsonl", "")
                entries = load_jsonl(sp)
                role = infer_agent_role(entries, agent_id)
                logs[f"agent-{agent_id}"] = {
                    "path": sp,
                    "agent_id": agent_id,
                    "role": role,
                    "entries": entries,
                }
    return logs


# ── analyses ─────────────────────────────────────────────────────────────────

def print_section(title: str):
    print(f"\n{'=' * 72}")
    print(f"  {title}")
    print(f"{'=' * 72}\n")


def analyze_agents(logs: dict):
    print_section("1. AGENT INVENTORY")

    print(f"{'Label':<25} {'Role':<35} {'AgentId':<10} {'Entries':>7}")
    print("-" * 80)
    for label, info in sorted(logs.items()):
        role = info.get("role", "team-lead" if label == "main" else "?")
        aid = info.get("agent_id", info.get("session_id", "-"))[:9]
        print(f"{label:<25} {role:<35} {aid:<10} {len(info['entries']):>7}")

    # Show how logical names map to multiple agent processes
    print_section("1b. LOGICAL AGENT → PROCESS MAPPING")
    print("Each logical team member name can spawn multiple subprocess agents:")
    print("(This happens because shutdown requests and task re-assignments")
    print(" each create a new subprocess for the same logical teammate.)\n")

    name_to_agents = defaultdict(list)
    for label, info in logs.items():
        role = info.get("role", "")
        # Group by base name
        base = role.split("(")[0].replace("shutdown-handler", "").replace("task-reassign", "")
        if label == "main":
            base = "team-lead"
        elif "shutdown-handler" in role:
            m = re.search(r'\(([^)]+)\)', role)
            base = m.group(1) if m else role
            name_to_agents[base].append((label, role, "shutdown"))
            continue
        elif "task-reassign" in role:
            name_to_agents[base or role].append((label, role, "reassign"))
            continue
        name_to_agents[base or role].append((label, role, "primary"))

    for name, agents in sorted(name_to_agents.items()):
        print(f"  {name}:")
        for label, role, kind in agents:
            print(f"    {label:<25} ({kind})")


def analyze_timeline(logs: dict):
    print_section("2. CHRONOLOGICAL TIMELINE (key events)")

    events = []

    for label, info in logs.items():
        role = info.get("role", "team-lead" if label == "main" else "?")
        for entry in info["entries"]:
            ts_str = entry.get("timestamp", "")
            ts = parse_timestamp(ts_str)
            if not ts:
                continue
            etype = entry.get("type")

            if etype == "assistant":
                for tc in extract_tool_calls(entry):
                    name = tc["name"]
                    inp = tc["input"]
                    desc = ""
                    if name == "TeamCreate":
                        desc = f"team={inp.get('team_name')}"
                    elif name == "TeamDelete":
                        desc = ""
                    elif name == "TaskCreate":
                        desc = f"subject=\"{inp.get('subject', '')[:50]}\""
                    elif name == "TaskUpdate":
                        parts = []
                        if "status" in inp:
                            parts.append(f"status={inp['status']}")
                        if "owner" in inp:
                            parts.append(f"owner={inp['owner']}")
                        desc = f"task#{inp.get('taskId')} {' '.join(parts)}"
                    elif name == "TaskGet":
                        desc = f"task#{inp.get('taskId')}"
                    elif name == "TaskList":
                        desc = ""
                    elif name == "Task":
                        desc = f"spawn name={inp.get('name')} model={inp.get('model')}"
                    elif name == "SendMessage":
                        desc = f"type={inp.get('type')} to={inp.get('recipient', 'all')}"
                    elif name in ("WebSearch", "WebFetch"):
                        continue  # skip web searches to reduce noise
                    else:
                        desc = ""
                    events.append((ts, ts_str, role, name, desc))

            elif etype == "user":
                msg = entry.get("message", {})
                content = msg.get("content", "")
                if isinstance(content, str) and "<teammate-message" in content:
                    m = re.search(r'teammate_id="([^"]+)"', content)
                    sender = m.group(1) if m else "?"
                    m2 = re.search(r'summary="([^"]*)"', content)
                    summary = m2.group(1)[:50] if m2 else ""
                    if "shutdown_request" in content:
                        events.append((ts, ts_str, role, "RECV_SHUTDOWN", f"from={sender}"))
                    elif "idle_notification" in content:
                        pass  # skip idle notifications
                    elif "task_assignment" in content:
                        events.append((ts, ts_str, role, "RECV_TASK_ASSIGN", f"from={sender}"))
                    else:
                        events.append((ts, ts_str, role, "RECV_MESSAGE", f"from={sender} \"{summary}\""))

    events.sort(key=lambda x: x[0])

    print(f"{'Time':>12}  {'Agent':<20} {'Event':<20} {'Details'}")
    print("-" * 90)
    t0 = events[0][0] if events else None
    for ts, ts_str, role, event, desc in events:
        elapsed = f"+{(ts - t0).total_seconds():6.1f}s" if t0 else ts_str[:19]
        print(f"{elapsed:>12}  {role:<20} {event:<20} {desc}")


def analyze_tool_usage(logs: dict):
    print_section("3. TOOL USAGE STATISTICS")

    # Per-agent tool counts
    agent_tools = {}
    global_counter = Counter()

    for label, info in logs.items():
        role = info.get("role", "team-lead" if label == "main" else "?")
        counter = Counter()
        for entry in info["entries"]:
            if entry.get("type") == "assistant":
                for tc in extract_tool_calls(entry):
                    counter[tc["name"]] += 1
                    global_counter[tc["name"]] += 1
        if counter:
            agent_tools[role] = counter

    print("Global tool call counts:")
    for tool, count in global_counter.most_common():
        print(f"  {tool:<20} {count:>4}")

    print(f"\nPer-agent breakdown:")
    for role, counter in sorted(agent_tools.items()):
        tools_str = ", ".join(f"{t}:{c}" for t, c in counter.most_common())
        print(f"  {role:<35} {tools_str}")


def analyze_task_lifecycle(logs: dict):
    print_section("4. TASK LIFECYCLE")

    # Collect all task-related events
    task_events = defaultdict(list)

    for label, info in logs.items():
        role = info.get("role", "team-lead" if label == "main" else "?")
        for entry in info["entries"]:
            ts = entry.get("timestamp", "")
            if entry.get("type") == "assistant":
                for tc in extract_tool_calls(entry):
                    name = tc["name"]
                    inp = tc["input"]
                    if name == "TaskCreate":
                        # TaskCreate doesn't have taskId in input, infer from sequence
                        task_events["(create)"].append((ts, role, name, inp.get("subject", "")[:60]))
                    elif name == "TaskUpdate":
                        tid = inp.get("taskId", "?")
                        parts = []
                        if "status" in inp:
                            parts.append(f"→{inp['status']}")
                        if "owner" in inp:
                            parts.append(f"owner={inp['owner']}")
                        if "activeForm" in inp:
                            parts.append(f"activeForm=\"{inp['activeForm'][:40]}\"")
                        task_events[tid].append((ts, role, "Update", " ".join(parts)))
                    elif name == "TaskGet":
                        tid = inp.get("taskId", "?")
                        task_events[tid].append((ts, role, "Get", ""))

    # Print creation events
    print("Task creation sequence (by team-lead):")
    for i, (ts, role, _, subject) in enumerate(task_events.get("(create)", []), 1):
        t = parse_timestamp(ts)
        print(f"  #{i}: \"{subject}\" (at {ts[:19]})")

    print()
    # Print per-task lifecycle
    for tid in sorted(k for k in task_events if k != "(create)"):
        print(f"Task #{tid} lifecycle:")
        for ts, role, action, detail in task_events[tid]:
            t = parse_timestamp(ts)
            time_str = ts[11:19] if ts else "?"
            print(f"  {time_str}  {role:<25} {action:<10} {detail}")
        print()


def analyze_communication(logs: dict):
    print_section("5. COMMUNICATION GRAPH")

    messages = []

    for label, info in logs.items():
        role = info.get("role", "team-lead" if label == "main" else "?")
        for entry in info["entries"]:
            ts = entry.get("timestamp", "")
            if entry.get("type") == "assistant":
                for tc in extract_tool_calls(entry):
                    if tc["name"] == "SendMessage":
                        inp = tc["input"]
                        msg_type = inp.get("type", "?")
                        recipient = inp.get("recipient", "broadcast")
                        summary = inp.get("summary", "")[:50]
                        content_len = len(inp.get("content", ""))
                        messages.append((ts, role, recipient, msg_type, summary, content_len))

    print(f"{'Time':>10}  {'From':<25} {'→':>1} {'To':<20} {'Type':<20} {'Chars':>6}  Summary")
    print("-" * 110)
    for ts, sender, recipient, msg_type, summary, content_len in sorted(messages):
        time_str = ts[11:19] if ts else "?"
        print(f"{time_str:>10}  {sender:<25} → {recipient:<20} {msg_type:<20} {content_len:>6}  {summary}")

    # Summary
    print(f"\nTotal messages sent: {len(messages)}")
    type_counts = Counter(m[3] for m in messages)
    for t, c in type_counts.most_common():
        print(f"  {t}: {c}")


def analyze_orchestration_pattern(logs: dict):
    print_section("6. PARENT ORCHESTRATION PATTERN")

    main_entries = logs.get("main", {}).get("entries", [])
    steps = []

    for entry in main_entries:
        ts = entry.get("timestamp", "")
        if entry.get("type") == "assistant":
            for tc in extract_tool_calls(entry):
                name = tc["name"]
                inp = tc["input"]
                if name in ("TeamCreate", "TeamDelete", "TaskCreate", "TaskUpdate",
                            "TaskList", "TaskGet", "Task", "SendMessage"):
                    steps.append((ts, name, inp))

    print("The parent (team-lead) orchestrates the entire lifecycle:\n")

    phases = [
        ("Phase 1: Team Setup", ["TeamCreate"]),
        ("Phase 2: Task Definition", ["TaskCreate"]),
        ("Phase 3: Task Assignment", ["TaskUpdate"]),
        ("Phase 4: Agent Spawning", ["Task"]),
        ("Phase 5: Monitoring", ["TaskList", "TaskGet"]),
        ("Phase 6: Shutdown", ["SendMessage"]),
        ("Phase 7: Cleanup", ["TeamDelete"]),
    ]

    for phase_name, tool_names in phases:
        matching = [(ts, name, inp) for ts, name, inp in steps if name in tool_names]
        if not matching:
            continue
        print(f"  {phase_name}:")
        for ts, name, inp in matching:
            time_str = ts[11:19] if ts else "?"
            if name == "TeamCreate":
                print(f"    {time_str}  TeamCreate(team_name=\"{inp.get('team_name')}\")")
            elif name == "TaskCreate":
                print(f"    {time_str}  TaskCreate(\"{inp.get('subject', '')[:55]}\")")
            elif name == "TaskUpdate":
                parts = [f"task#{inp.get('taskId')}"]
                if "owner" in inp:
                    parts.append(f"owner={inp['owner']}")
                if "status" in inp:
                    parts.append(f"→{inp['status']}")
                print(f"    {time_str}  TaskUpdate({', '.join(parts)})")
            elif name == "Task":
                print(f"    {time_str}  Task(spawn \"{inp.get('name')}\", model={inp.get('model')}, bg={inp.get('run_in_background')})")
            elif name == "TaskList":
                print(f"    {time_str}  TaskList() — check progress")
            elif name == "SendMessage":
                print(f"    {time_str}  SendMessage(type={inp.get('type')}, to={inp.get('recipient')})")
            elif name == "TeamDelete":
                print(f"    {time_str}  TeamDelete()")
        print()

    # Timing summary
    timestamps = [parse_timestamp(ts) for ts, _, _ in steps if parse_timestamp(ts)]
    if len(timestamps) >= 2:
        total = (timestamps[-1] - timestamps[0]).total_seconds()
        print(f"  Total orchestration time: {total:.1f}s ({total/60:.1f}min)")

    print()
    print("KEY INSIGHT: Agent Subprocess Model")
    print("-" * 50)
    print("""
Each "teammate" is NOT a persistent process. Instead:
  1. The parent spawns a subprocess (Task tool) for the initial work
  2. That subprocess runs to completion and terminates
  3. When the parent sends a message (e.g., shutdown_request), a NEW
     subprocess is created for the same logical teammate name
  4. This means one logical name (e.g., "tech-architect") may have
     2-3 separate agentId processes over its lifetime

This is evident in the logs:
  - agent-a0ca9b7 = tech-architect (primary work, 67 entries)
  - agent-a858cb2 = tech-architect (shutdown handler, 3 entries)

The communication model uses an inbox/mailbox pattern:
  - SendMessage puts a message in the recipient's inbox
  - The system spawns a new subprocess to process the inbox message
  - The subprocess responds and terminates
""")


def analyze_subagent_work_pattern(logs: dict):
    print_section("7. SUBAGENT WORK PATTERN")
    print("Typical subagent lifecycle (from tech-architect trace):\n")
    print("""
  1. RECEIVE TASK    ← teammate-message from team-lead with instructions
  2. TaskGet(#2)     ← Read assigned task details from shared task list
  3. TaskUpdate(#2)  ← Set status=in_progress, update activeForm
  4. [DO WORK]       ← WebSearch ×13 (parallel batches of 3-4)
                        ~3 minutes of research
  5. SendMessage     ← Send full report to team-lead (type=message)
  6. TaskUpdate(#2)  ← Set status=completed
  7. EXIT            ← Process terminates, system sends idle_notification

Key observations:
  - Subagents use parallel WebSearch calls (3-4 at a time)
  - The report is sent as a SendMessage, NOT written to a file
  - Task status updates bracket the work: in_progress → [work] → completed
  - The parent receives the report as a <teammate-message> user turn
  - After the agent goes idle, the parent checks TaskList to track progress
""")


# ── main ─────────────────────────────────────────────────────────────────────

def main():
    if not os.path.isdir(LOG_DIR):
        print(f"Error: log directory not found: {LOG_DIR}", file=sys.stderr)
        sys.exit(1)

    logs = discover_logs(LOG_DIR)
    if not logs:
        print("No log files found.", file=sys.stderr)
        sys.exit(1)

    print("╔══════════════════════════════════════════════════════════════════════╗")
    print("║   Claude Code Multi-Agent Coordination — Log Analysis Report       ║")
    print("╚══════════════════════════════════════════════════════════════════════╝")
    print(f"\nLog directory: {LOG_DIR}")
    print(f"Session ID:    {logs.get('main', {}).get('session_id', '?')}")
    print(f"Total agents:  {len(logs) - 1} subagents + 1 parent")
    print(f"Total entries: {sum(len(info['entries']) for info in logs.values())}")

    analyze_agents(logs)
    analyze_timeline(logs)
    analyze_tool_usage(logs)
    analyze_task_lifecycle(logs)
    analyze_communication(logs)
    analyze_orchestration_pattern(logs)
    analyze_subagent_work_pattern(logs)


if __name__ == "__main__":
    main()
