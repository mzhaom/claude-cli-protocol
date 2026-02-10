# Claude Code Multi-Agent Coordination — Log Analysis

> Analysis of JSONL logs from a Claude Code team session where a parent agent
> spawned 3 worker agents to research a CLI tool concept from different angles.

**Session ID:** `2fbd084c-4203-4dcb-829b-c1958c90e090`
**Duration:** ~5 minutes (00:30:15 → 00:35:22 UTC)
**Total log entries:** 294 across 9 log files

---

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                         USER PROMPT                                 │
│  "Create an agent team to explore a TODO tracker CLI idea"          │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│                     PARENT (team-lead)                               │
│                     main session log                                │
│                     64 entries                                       │
│                                                                     │
│  Tools used: TeamCreate, TaskCreate×3, TaskUpdate×3,                │
│              Task×3, TaskList×2, SendMessage×3, TeamDelete          │
└────┬──────────────────┬──────────────────┬──────────────────────────┘
     │                  │                  │
     │ Task(spawn)      │ Task(spawn)      │ Task(spawn)
     │ run_in_bg=true   │ run_in_bg=true   │ run_in_bg=true
     │ model=sonnet     │ model=sonnet     │ model=sonnet
     ▼                  ▼                  ▼
┌──────────────┐ ┌──────────────┐ ┌──────────────┐
│ux-researcher │ │tech-architect│ │devils-advocate│
│  agent-ab4ea │ │  agent-a0ca9 │ │  agent-a0ebe │
│  73 entries  │ │  67 entries  │ │  68 entries   │
│              │ │              │ │               │
│ WebSearch×12 │ │ WebSearch×13 │ │ WebSearch×13  │
│ WebFetch×4   │ │              │ │               │
│ TaskGet      │ │ TaskGet      │ │ TaskGet       │
│ TaskUpdate×2 │ │ TaskUpdate×2 │ │ TaskUpdate×3  │
│ SendMessage  │ │ SendMessage  │ │ SendMessage   │
│ TaskList     │ │              │ │               │
└──────────────┘ └──────────────┘ └───────────────┘
     │                  │                  │
     │ SendMessage      │ SendMessage      │ SendMessage
     │ (19,320 chars)   │ (29,709 chars)   │ (12,850 chars)
     │                  │                  │
     └──────────────────┴──────────────────┘
                        │
                        ▼
              ┌───────────────────┐
              │  Parent receives   │
              │  <teammate-message>│
              │  in user turns     │
              └───────────────────┘
```

---

## 2. The 7-Phase Orchestration Lifecycle

The parent follows a strict sequential pattern to manage the entire team:

```
Time ──────────────────────────────────────────────────────────────────────►

Phase 1       Phase 2          Phase 3         Phase 4
SETUP         DEFINE           ASSIGN          SPAWN
  │             │                │               │
  ▼             ▼                ▼               ▼
TeamCreate    TaskCreate #1    TaskUpdate #1   Task("ux-researcher")
              TaskCreate #2    TaskUpdate #2   Task("tech-architect")
              TaskCreate #3    TaskUpdate #3   Task("devils-advocate")
  │             │                │               │
  +0.0s         +13.5s           +34.4s          +43.7s


Phase 5              Phase 6              Phase 7
MONITOR              SHUTDOWN             CLEANUP
  │                    │                    │
  ▼                    ▼                    ▼
TaskList (×2)        SendMessage×3        TeamDelete
(after each          (shutdown_request
 report arrives)      to each agent)
  │                    │                    │
  +199s                +274.7s              +302.3s
```

### Phase Details

| Phase | Time Offset | Tool Calls | Purpose |
|-------|-------------|------------|---------|
| 1. Setup | +0.0s | `TeamCreate("todo-tracker-design")` | Creates team + shared task list |
| 2. Define | +13–31s | `TaskCreate` ×3 | Creates tasks with detailed descriptions |
| 3. Assign | +34–36s | `TaskUpdate` ×3 | Sets `owner` + `status=in_progress` |
| 4. Spawn | +44–53s | `Task` ×3 | Launches background subagent processes |
| 5. Monitor | +199–258s | `TaskList` ×2 | Checks completion after each report |
| 6. Shutdown | +275s | `SendMessage` ×3 | Sends `shutdown_request` to each agent |
| 7. Cleanup | +302s | `TeamDelete` | Removes team + task directory |

---

## 3. Agent Instance Model

### Key Discovery: Each Logical Name Maps to Multiple Agent Instances

Each logical teammate name maps to **multiple separate agent instances**, each with its own
`agentId`, log file, and conversation history. When the parent needs to communicate with a
teammate, the system creates a **new agent instance** to handle the message.

> **Note on terminology:** The logs don't contain OS-level process IDs. What we observe
> directly is that each instance has a **different `agentId`**, a **separate `.jsonl` log
> file**, and **no shared UUID chain** (the shutdown handler's `parentUuid` does not appear
> in the primary worker's log). We use "agent instance" to describe this — whether each
> instance is an OS-level subprocess, a thread, or something else is not visible in the logs.

**Evidence for separate instances (using ux-researcher as example):**

| Property | Primary worker (`ab4ea94`) | Shutdown handler (`abbb479`) |
|---|---|---|
| agentId | `ab4ea94` | `abbb479` |
| Log file | `agent-ab4ea94.jsonl` | `agent-abbb479.jsonl` |
| Entries | 73 | 3 |
| Last timestamp | `00:34:45` | `00:35:00` |
| parentUuid of entry [0] | `null` | `2eb04bba...` (not in ab4ea94's UUIDs) |
| Knows about prior work? | N/A | No — receives only the shutdown JSON |

```
Logical Name: "tech-architect"
─────────────────────────────────────────────────────────────────────────

Instance 1: agent-a0ca9b7 (PRIMARY WORK)
├─ 67 log entries, separate agentId
├─ Receives initial task instructions
├─ Does 13 WebSearches
├─ Sends report via SendMessage
├─ Marks task completed
└─ Goes idle → idle_notification sent to parent

Instance 2: agent-a858cb2 (SHUTDOWN HANDLER)
├─ 3 log entries, separate agentId
├─ Receives shutdown_request — no context from Instance 1
├─ Responds with shutdown_response(approve=true)
└─ Exits


Logical Name: "ux-researcher"
─────────────────────────────────────────────────────────────────────────

Instance 1: agent-ab4ea94 (PRIMARY WORK)
├─ 73 log entries
├─ 12 WebSearches + 4 WebFetches
├─ Sends report, marks task completed
└─ Goes idle

Instance 2: agent-a875f09 (TASK REASSIGNMENT)
├─ 8 log entries, separate agentId, no shared history with Instance 1
├─ Receives task_assignment for already-completed task
├─ Checks TaskGet → confirms already done
├─ Sends confirmation to team-lead
└─ Goes idle

Instance 3: agent-abbb479 (SHUTDOWN HANDLER)
├─ 3 log entries, separate agentId, no shared history
└─ Handles shutdown handshake


Logical Name: "devils-advocate"
─────────────────────────────────────────────────────────────────────────

Instance 1: agent-a0ebe4c (PRIMARY WORK) — 68 entries
Instance 2: agent-a54d4b6 (TASK REASSIGNMENT) — 5 entries
Instance 3: agent-ac16e09 (SHUTDOWN HANDLER) — 3 entries
```

### Full Agent Instance Mapping

| Logical Name | Agent ID | Role | Entries |
|---|---|---|---|
| **team-lead** | `2fbd084c` | Parent orchestrator | 64 |
| **tech-architect** | `a0ca9b7` | Primary worker | 67 |
| **tech-architect** | `a858cb2` | Shutdown handler | 3 |
| **ux-researcher** | `ab4ea94` | Primary worker | 73 |
| **ux-researcher** | `a875f09` | Task reassignment | 8 |
| **ux-researcher** | `abbb479` | Shutdown handler | 3 |
| **devils-advocate** | `a0ebe4c` | Primary worker | 68 |
| **devils-advocate** | `a54d4b6` | Task reassignment | 5 |
| **devils-advocate** | `ac16e09` | Shutdown handler | 3 |

---

## 4. Communication Model

### Mailbox/Inbox Pattern

```
┌─────────────┐    SendMessage     ┌──────────────┐    spawn new    ┌──────────────┐
│  Agent A     │ ──────────────►   │   Mailbox    │ ────────────►   │  Agent B     │
│  (sender)    │   puts message    │  (recipient  │   new agent     │  (new inst)  │
│              │   in recipient's  │   inbox)     │   to process    │              │
└─────────────┘   inbox            └──────────────┘   the message   └──────────────┘
```

### Message Flow Sequence

```
    Parent (team-lead)              Worker (tech-architect)
    ──────────────────              ──────────────────────
           │                                │
           │  Task(spawn, prompt=...)        │
           │──────────────────────────────►  │ (agent-a0ca9b7 created)
           │                                │
           │                                │── TaskGet(#2)
           │                                │── TaskUpdate(#2, in_progress)
           │                                │── WebSearch ×13
           │                                │── SendMessage(report)
           │                                │── TaskUpdate(#2, completed)
           │  <teammate-message>            │
           │◄───────────────────────────────│ (process terminates)
           │                                ×
           │
           │── TaskList() → checks progress
           │
           │  SendMessage(shutdown_request)
           │──────────────────────────────►  │ (agent-a858cb2 created)
           │                                │
           │                                │── SendMessage(shutdown_response,
           │  <teammate-message>            │      approve=true)
           │◄───────────────────────────────│ (process terminates)
           │                                ×
```

### All Messages Exchanged

| Time | From | To | Type | Size |
|------|------|----|------|------|
| 00:33:35 | devils-advocate | team-lead | `message` | 12,850 chars |
| 00:34:33 | ux-researcher | team-lead | `message` | 19,320 chars |
| 00:34:50 | tech-architect | team-lead | `message` | 29,709 chars |
| 00:34:53 | ux-researcher (reassign) | team-lead | `message` | 496 chars |
| 00:34:55 | team-lead | ux-researcher | `shutdown_request` | 57 chars |
| 00:34:55 | team-lead | tech-architect | `shutdown_request` | 57 chars |
| 00:34:56 | team-lead | devils-advocate | `shutdown_request` | 57 chars |
| 00:35:00 | devils-advocate | (response) | `shutdown_response` | 30 chars |
| 00:35:00 | ux-researcher | (response) | `shutdown_response` | 45 chars |
| 00:35:04 | tech-architect | (response) | `shutdown_response` | 54 chars |

**Total payload:** ~62,600 chars of content exchanged (61,879 in reports alone).

---

## 5. Task Tool Usage Patterns

### Shared Task List as Coordination Primitive

The `TaskCreate`/`TaskGet`/`TaskUpdate`/`TaskList` tools operate on a **shared filesystem-based task list** (`~/.claude/tasks/{team-name}/`). This is the primary coordination mechanism between parent and children.

```
                    Shared Task List
                 ~/.claude/tasks/todo-tracker-design/
                ┌─────────────────────────────────────┐
                │  #1: UX Research          [completed]│
                │  #2: Tech Architecture    [completed]│
                │  #3: Devil's Advocate     [completed]│
                └───────┬──────────────┬──────────────┘
                        │              │
           ┌────────────┘              └────────────┐
           │                                        │
    Parent writes:                          Workers write:
    • TaskCreate (define)                   • TaskGet (read assignment)
    • TaskUpdate (assign owner,             • TaskUpdate (in_progress)
      set initial status)                  • TaskUpdate (completed)
    • TaskList (poll progress)
```

### Task Lifecycle State Machine

```
                    TaskCreate
                        │
                        ▼
                  ┌──────────┐    TaskUpdate(owner=X,
                  │  pending  │    status=in_progress)     ┌─────────────┐
                  │          │ ─────────────────────────► │ in_progress  │
                  └──────────┘        (by parent)         │             │
                                                          └──────┬──────┘
                                                                 │
                                                    TaskUpdate(status=completed)
                                                          (by worker)
                                                                 │
                                                                 ▼
                                                          ┌─────────────┐
                                                          │  completed   │
                                                          └─────────────┘
```

### Per-Task Lifecycle Trace

**Task #1 — "Research UX patterns for the TODO tracker CLI"**
```
00:30:33  team-lead      TaskCreate     (defines task)
00:30:54  team-lead      TaskUpdate     owner=ux-researcher, →in_progress
00:31:06  ux-researcher  TaskGet        (reads assignment)
00:31:10  ux-researcher  TaskUpdate     →in_progress, activeForm="Researching UX patterns..."
          ... 3.5 minutes of WebSearch/WebFetch work ...
00:34:36  ux-researcher  TaskUpdate     →completed
```

**Task #2 — "Design technical architecture for the TODO tracker CLI"**
```
00:30:41  team-lead      TaskCreate     (defines task)
00:30:55  team-lead      TaskUpdate     owner=tech-architect, →in_progress
00:31:11  tech-architect TaskGet        (reads assignment)
00:31:15  tech-architect TaskUpdate     →in_progress, activeForm="Researching architecture..."
          ... 3.5 minutes of WebSearch work ...
00:34:53  tech-architect TaskUpdate     →completed
```

**Task #3 — "Play devil's advocate on the TODO tracker CLI concept"**
```
00:30:51  team-lead      TaskCreate     (defines task)
00:30:55  team-lead      TaskUpdate     owner=devils-advocate, →in_progress
00:31:16  devils-advocate TaskGet       (reads assignment)
00:31:19  devils-advocate TaskUpdate    →in_progress, activeForm="Researching landscape..."
          ... 2.3 minutes of WebSearch work ...
00:33:40  devils-advocate TaskUpdate    →completed
```

---

## 6. Tool Usage Statistics

### Global Counts (all agents combined)

| Tool | Count | Used By |
|------|-------|---------|
| WebSearch | 38 | Workers only |
| TaskUpdate | 10 | Parent (3) + Workers (7) |
| SendMessage | 10 | Parent (3 shutdown) + Workers (4 reports) + Shutdown handlers (3) |
| TaskList | 4 | Parent (2) + Workers (2) |
| TaskGet | 4 | Workers only |
| WebFetch | 4 | ux-researcher only |
| TaskCreate | 3 | Parent only |
| Task | 3 | Parent only |
| TeamCreate | 1 | Parent only |
| TeamDelete | 1 | Parent only |

### Tool Ownership Pattern

```
Parent-only tools:          Shared tools:           Worker-only tools:
──────────────────          ─────────────           ──────────────────
• TeamCreate                • TaskUpdate            • WebSearch
• TeamDelete                • TaskList              • WebFetch
• TaskCreate                • SendMessage           • TaskGet
• Task (spawn)
```

### Worker Parallel WebSearch Pattern

Workers issue WebSearch calls in **parallel batches of 3-4**, not sequentially:

```
tech-architect search timeline:

00:31:17 ─┬─ WebSearch("ripgrep architecture...")
           ├─ WebSearch("AST-based code search...")
           ├─ WebSearch("incremental file scanning...")
           └─ WebSearch("CLI tool distribution...")
                    │ (wait for all 4 results)
                    ▼
00:31:31 ─┬─ WebSearch("SQLite vs JSON storage...")
           ├─ WebSearch("git blame integration...")
           ├─ WebSearch("tokei architecture...")
           └─ WebSearch("Rust vs Go CLI tool...")
                    │ (wait for all 4 results)
                    ▼
00:31:44 ─┬─ WebSearch("tree-sitter comment extraction...")
           ├─ WebSearch("todo-tree vscode extension...")
           └─ WebSearch("ripgrep ignore crate...")
                    │ (wait for all 3 results)
                    ▼
00:31:57 ─┬─ WebSearch("Rust clap CLI framework...")
           └─ WebSearch("file watching notify crate...")
                    │
                    ▼
           (begin writing report — takes ~2.5 min of generation)
```

---

## 7. Timing Analysis

### End-to-End Timeline

```
00:30:15 ┤ User prompt
00:30:20 ┤ TeamCreate                              ─┐
00:30:33 ┤ TaskCreate #1                             │ Setup
00:30:41 ┤ TaskCreate #2                             │ ~35s
00:30:51 ┤ TaskCreate #3                             │
00:30:55 ┤ TaskUpdate ×3 (assign)                   ─┘
00:31:04 ┤ Task(spawn ux-researcher)                ─┐
00:31:08 ┤ Task(spawn tech-architect)                │ Spawn
00:31:13 ┤ Task(spawn devils-advocate)              ─┘ ~10s
         │
         │ ════════ Workers active (parallel) ═══════
         │
00:33:35 ┤ devils-advocate report arrives    (2m17s of work)
00:33:39 ┤ TaskList — 1 of 3 done
00:34:33 ┤ ux-researcher report arrives      (3m27s of work)
00:34:38 ┤ TaskList — 2 of 3 done
00:34:50 ┤ tech-architect report arrives     (3m42s of work)
         │
00:34:55 ┤ SendMessage(shutdown) ×3                 ─┐
00:35:04 ┤ All shutdowns acknowledged                │ Wind-down
00:35:22 ┤ TeamDelete                               ─┘ ~27s
         │
         Total wall-clock: 5 minutes 7 seconds
         Parallel work saved: ~4.5 min vs sequential execution
```

### Per-Agent Active Time

| Agent | Start | End | Active Time | Work Product |
|-------|-------|-----|-------------|-------------|
| devils-advocate | 00:31:13 | 00:33:40 | 2m 27s | 12,850 char report |
| ux-researcher | 00:31:04 | 00:34:36 | 3m 32s | 19,320 char report |
| tech-architect | 00:31:08 | 00:34:53 | 3m 45s | 29,709 char report |

---

## 8. Log File Structure Reference

### Directory Layout

```
subagents-logs/
├── 2fbd084c-4203-4dcb-829b-c1958c90e090.jsonl     ← Parent session
├── 2fbd084c-4203-4dcb-829b-c1958c90e090/
│   └── subagents/
│       ├── agent-a0ca9b7.jsonl   ← tech-architect (primary)
│       ├── agent-a0ebe4c.jsonl   ← devils-advocate (primary)
│       ├── agent-a54d4b6.jsonl   ← devils-advocate (task reassign)
│       ├── agent-a858cb2.jsonl   ← tech-architect (shutdown)
│       ├── agent-a875f09.jsonl   ← ux-researcher (task reassign)
│       ├── agent-ab4ea94.jsonl   ← ux-researcher (primary)
│       ├── agent-abbb479.jsonl   ← ux-researcher (shutdown)
│       └── agent-ac16e09.jsonl   ← devils-advocate (shutdown)
└── memory/
```

### JSONL Entry Types

Each line is a JSON object with a `type` field:

| Type | Description | Key Fields |
|------|-------------|------------|
| `user` | User or system message to the agent | `message.content`, `timestamp`, `agentId` |
| `assistant` | Agent's response (text + tool calls) | `message.content[]` (text/tool_use/thinking blocks) |
| `progress` | Streaming progress indicator | `timestamp` |
| `system` | System metadata | `subtype` (e.g., `turn_duration`) |
| `file-history-snapshot` | File state checkpoint | `snapshot` |

### Tool Call Block Structure

Inside `assistant` entries, `message.content` is an array of blocks:

```json
{
  "type": "tool_use",
  "id": "toolu_01ABC...",
  "name": "TaskCreate",
  "input": {
    "subject": "...",
    "description": "...",
    "activeForm": "..."
  }
}
```

Tool results come back as `tool_result` blocks in the next `user` entry:

```json
{
  "type": "tool_result",
  "tool_use_id": "toolu_01ABC...",
  "content": "Task #1 created successfully"
}
```

---

## 9. End-to-End Trace: Task #1 (ux-researcher) with Raw JSONL

This section walks through the **complete lifecycle of Task #1** — from team creation
to cleanup — showing the exact JSONL log entries on both the parent and subagent sides.

```
  PARENT LOG                              SUBAGENT LOG
  (2fbd084c-...jsonl)                     (agent-ab4ea94.jsonl)
  ═══════════════════                     ═══════════════════════

  ┌─ Step 1: TeamCreate ─────────────┐
  │  entry [5] assistant              │
  │  entry [6] user (tool_result)     │
  └───────────────────────────────────┘
              │
              ▼
  ┌─ Step 2: TaskCreate ──────────────┐
  │  entry [8] assistant              │
  │  entry [9] user (tool_result)     │
  └───────────────────────────────────┘
              │
              ▼
  ┌─ Step 3: TaskUpdate (assign) ─────┐
  │  entry [15] assistant             │
  │  entry [16] user (tool_result)    │
  └───────────────────────────────────┘
              │
              ▼
  ┌─ Step 4: Task (spawn) ───────────┐    ┌─ Step 5: Receive task ────────┐
  │  entry [21] assistant            │───►│  entry [0] user               │
  │  entry [22] user (tool_result)   │    │  (teammate-message)           │
  └──────────────────────────────────┘    └───────────────────────────────┘
                                                      │
              ┌───────────────────────────────────────┘
              ▼
                                          ┌─ Step 6: TaskGet ─────────────┐
                                          │  entry [2] assistant          │
                                          │  entry [3] user (tool_result) │
                                          └───────────────────────────────┘
                                                      │
                                                      ▼
                                          ┌─ Step 7: TaskUpdate ──────────┐
                                          │  entry [5] assistant          │
                                          │  entry [6] user (tool_result) │
                                          │  (→in_progress + activeForm)  │
                                          └───────────────────────────────┘
                                                      │
                                                      ▼
                                          ┌─ Step 8: WebSearch ×12 ───────┐
                                          │  entries [7]–[64]             │
                                          │  parallel batches of 3-4      │
                                          │  ~3.5 minutes                 │
                                          └───────────────────────────────┘
                                                      │
                                                      ▼
                                          ┌─ Step 9: SendMessage ─────────┐
                                          │  entry [65] assistant         │
                                          │  entry [66] user (tool_result)│
                                          │  (19,320 char report)         │
                                          └───────────────────────────────┘
              ┌───────────────────────────────────────┘
              ▼
  ┌─ Step 10: Receive report ─────────┐
  │  entry [37] user                  │
  │  (<teammate-message>)             │
  └───────────────────────────────────┘
              │
              ▼                           ┌─ Step 11b: TaskUpdate ────────┐
  ┌─ Step 11a: TaskList ──────────────┐   │  entry [68] assistant         │
  │  entry [39] assistant             │   │  entry [69] user (tool_result)│
  │  entry [40] user (tool_result)    │   │  (→completed)                 │
  └───────────────────────────────────┘   └───────────────────────────────┘
              │                                       │
              ▼                                       ▼
  ┌─ Step 12: SendMessage(shutdown) ──┐   ┌─ Step 12b: TaskList ──────────┐
  │  entry [46] assistant             │   │  entry [70] assistant         │
  │  entry [47] user (tool_result)    │   │  entry [71] user (tool_result)│
  └───────────────────────────────────┘   └───────────────────────────────┘
              │                                     (process idles)
              ▼
                                          NEW SUBPROCESS: agent-abbb479
                                          ┌─ Step 13: Handle shutdown ────┐
                                          │  entry [0] user               │
                                          │  (shutdown_request JSON)      │
                                          │  entry [1] assistant          │
                                          │  SendMessage(shutdown_response│
                                          │    approve=true)              │
                                          │  entry [2] user (tool_result) │
                                          └───────────────────────────────┘
              ┌───────────────────────────────────────┘
              ▼
  ┌─ Step 14: TeamDelete ─────────────┐
  │  entry [56] assistant             │
  │  entry [57] user (tool_result)    │
  └───────────────────────────────────┘
```

### Step 1 — TeamCreate (parent entry [5]→[6])

The parent creates the team, which provisions a shared task directory.

**Parent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_01QQAikvJHX989JeYkTPC62j",
  "name": "TeamCreate",
  "input": {
    "team_name": "todo-tracker-design",
    "description": "Exploring a CLI tool for tracking TODO comments across codebases - UX, architecture, and critical analysis"
  }
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_01QQAikvJHX989JeYkTPC62j",
  "type": "tool_result",
  "content": [{
    "type": "text",
    "text": "{\"team_name\":\"todo-tracker-design\",\"team_file_path\":\"/home/ming/.claude/teams/todo-tracker-design/config.json\",\"lead_agent_id\":\"team-lead@todo-tracker-design\"}"
  }]
}
```

> Creates `~/.claude/teams/todo-tracker-design/config.json` and `~/.claude/tasks/todo-tracker-design/`.
> The parent becomes `team-lead@todo-tracker-design`.

---

### Step 2 — TaskCreate (parent entry [8]→[9])

The parent defines Task #1 with a detailed description that serves as the worker's brief.

**Parent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_016NdKvhbxjR3thdqEZPfdbv",
  "name": "TaskCreate",
  "input": {
    "subject": "Research UX patterns for the TODO tracker CLI",
    "description": "You are the UX specialist for a CLI tool that helps developers track TODO comments across their codebase. Research and propose:\n\n1. **CLI Interface Design**: What commands and flags should the tool have?...\n2. **Output Formatting**: How should results be displayed?...\n3. **Developer Workflow Integration**: ...\n4. **TODO Comment Syntax**: ...\n5. **Configuration UX**: ...\n6. **Onboarding**: ...\n\nProvide concrete examples of CLI invocations and sample output.",
    "activeForm": "Researching UX patterns"
  }
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_016NdKvhbxjR3thdqEZPfdbv",
  "type": "tool_result",
  "content": "Task #1 created successfully: Research UX patterns for the TODO tracker CLI"
}
```

> Task is created with status `pending` in `~/.claude/tasks/todo-tracker-design/`.
> Note: `content` is a plain string here (not an array) — tool_result format varies.

---

### Step 3 — TaskUpdate: assign owner (parent entry [15]→[16])

The parent assigns the task to a logical name and sets it to `in_progress`.

**Parent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_01V4a6aqubDVNsSJmCmRdWcs",
  "name": "TaskUpdate",
  "input": {
    "taskId": "1",
    "owner": "ux-researcher",
    "status": "in_progress"
  }
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_01V4a6aqubDVNsSJmCmRdWcs",
  "type": "tool_result",
  "content": "Updated task #1 owner, status"
}
```

> The task now has `owner=ux-researcher` and `status=in_progress` in the shared task file.
> This happens BEFORE the agent is spawned — the task is pre-assigned.

---

### Step 4 — Task: spawn subagent (parent entry [21]→[22])

The parent uses the `Task` tool to launch the ux-researcher as a background agent instance.

**Parent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_0125BxyMaMLtKXzQ2sb7Xpuo",
  "name": "Task",
  "input": {
    "description": "UX research for TODO CLI",
    "prompt": "You are \"ux-researcher\", a UX specialist on the \"todo-tracker-design\" team. Your task is to research and propose UX patterns for a CLI tool that tracks TODO comments across codebases.\n\nRead your assigned task (#1) using TaskGet, then do thorough web research on:\n- Best CLI UX patterns from tools like ripgrep, fd, gh, jq\n- TODO comment conventions in the industry\n- Developer workflow integration patterns\n\nSearch the web for: existing TODO tracking CLI tools, CLI UX best practices, developer experience patterns for command-line tools.\n\nWrite a comprehensive but well-structured report...\n\nWhen done, send your full report to \"team-lead\" using SendMessage, then mark task #1 as completed.",
    "name": "ux-researcher",
    "subagent_type": "general-purpose",
    "team_name": "todo-tracker-design",
    "model": "sonnet",
    "run_in_background": true
  }
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_0125BxyMaMLtKXzQ2sb7Xpuo",
  "type": "tool_result",
  "content": [{
    "type": "text",
    "text": "Spawned successfully.\nagent_id: ux-researcher@todo-tracker-design\nname: ux-researcher\nteam_name: todo-tracker-design\nThe agent is now running and will receive instructions via mailbox."
  }]
}
```

> `run_in_background: true` means the parent doesn't block waiting for this agent.
> `model: sonnet` — workers use a cheaper/faster model than the parent.
> The prompt includes explicit instructions to use TaskGet, SendMessage, and TaskUpdate.

---

### Step 5 — Subagent receives initial message (subagent entry [0])

A new agent instance `agent-ab4ea94` is created. The parent's prompt is delivered as a
`<teammate-message>` in the subagent's first `user` entry.

**Subagent log entry [0] — type: user:**
```json
{
  "parentUuid": null,
  "isSidechain": true,
  "userType": "external",
  "cwd": "/home/ming/g/teamagent",
  "sessionId": "2fbd084c-4203-4dcb-829b-c1958c90e090",
  "version": "2.1.37",
  "gitBranch": "master",
  "agentId": "ab4ea94",
  "type": "user",
  "uuid": "4ce17a24-b78e-4d24-80e3-9dd381723733",
  "timestamp": "2026-02-08T00:31:04.125Z"
}
```
```
message.content:
<teammate-message teammate_id="team-lead" summary="UX research for TODO CLI">
You are "ux-researcher", a UX specialist on the "todo-tracker-design" team.
Your task is to research and propose UX patterns for a CLI tool that tracks
TODO comments across codebases.

Read your assigned task (#1) using TaskGet, then do thorough web research on:
- Best CLI UX patterns from tools like ripgrep, fd, gh, jq
- TODO comment conventions in the industry
- Developer workflow integration patterns
...
</teammate-message>
```

> Key fields: `isSidechain: true` (this is a child, not the main conversation),
> `agentId: "ab4ea94"` (unique process identifier), `sessionId` matches the parent's session.
> `parentUuid: null` because this is the first message (no preceding turn).

---

### Step 6 — Subagent calls TaskGet (subagent entry [2]→[3])

The worker reads its assigned task from the shared task list.

**Subagent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_012o2Srg8sG6g9wpmSa8d3GP",
  "name": "TaskGet",
  "input": {
    "taskId": "1"
  }
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_012o2Srg8sG6g9wpmSa8d3GP",
  "type": "tool_result",
  "content": "Task #1: Research UX patterns for the TODO tracker CLI\nStatus: in_progress\nDescription: You are the UX specialist for a CLI tool that helps developers track TODO comments across their codebase. Research and propose:\n\n1. **CLI Interface Design**: What commands and flags should the tool have?...\n2. **Output Formatting**: ...\n3. **Developer Workflow Integration**: ...\n4. **TODO Comment Syntax**: ...\n5. **Configuration UX**: ...\n6. **Onboarding**: ...\n\nProvide concrete examples of CLI invocations and sample output. Think about what makes tools like ripgrep, fd, and gh feel great to use."
}
```

> The worker reads the full task description that the parent wrote in Step 2.
> Status is already `in_progress` because the parent set it in Step 3.

---

### Step 7 — Subagent calls TaskUpdate: in_progress (subagent entry [5]→[6])

The worker confirms it's working and sets the `activeForm` spinner text.

**Subagent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_01NbkcBFyj8TJfe9HjRNvafV",
  "name": "TaskUpdate",
  "input": {
    "taskId": "1",
    "status": "in_progress",
    "activeForm": "Researching UX patterns for TODO tracker CLI"
  }
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_01NbkcBFyj8TJfe9HjRNvafV",
  "type": "tool_result",
  "content": "Updated task #1 activeForm"
}
```

> `activeForm` is shown as a spinner in the UI while the agent works.
> Status was already `in_progress` so only `activeForm` changed (the result says so).

---

### Step 8 — Subagent does work (subagent entries [7]–[64])

The worker performs 12 WebSearches and 4 WebFetches in parallel batches,
then spends ~2.5 minutes generating the report. (Entries omitted for brevity —
see Section 6 "Worker Parallel WebSearch Pattern" for the batch structure.)

---

### Step 9 — Subagent calls SendMessage: deliver report (subagent entry [65]→[66])

The worker sends its full report to the parent via the messaging system.

**Subagent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_016CtRaQmCLUmEK4CrJ1cKDU",
  "name": "SendMessage",
  "input": {
    "type": "message",
    "recipient": "team-lead",
    "summary": "Comprehensive UX research report for TODO tracker CLI",
    "content": "\n# UX Research Report: TODO Tracker CLI\n\n## Executive Summary\n\nThis report synthesizes research from industry-leading CLI tools (ripgrep, fd, gh, jq), existing TODO-tracking tools (leasot, tickgit, fixme, todo-tree)..."
  }
}
```
*(content is 19,320 chars total)*

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_016CtRaQmCLUmEK4CrJ1cKDU",
  "type": "tool_result",
  "content": [{
    "type": "text",
    "text": "{\"success\":true,\"message\":\"Message sent to team-lead's inbox\",\"routing\":{\"sender\":\"ux-researcher\",\"senderColor\":\"blue\",\"target\":\"@team-lead\",\"targetColor\":\"red\",\"summary\":\"Comprehensive UX research report for TODO tracker CLI\"...}}"
  }]
}
```

> The message goes into the parent's inbox. The routing metadata includes color coding
> for the UI display. The entire 19KB report is transmitted as the `content` field.

---

### Step 10 — Parent receives report (parent entry [37])

The report appears as a new `user` turn in the parent's log, wrapped in `<teammate-message>`.

**Parent log entry [37] — type: user:**
```json
{
  "parentUuid": "7ffd0a15-e2dc-44c3-98db-77dc752c4a52",
  "isSidechain": false,
  "teamName": "todo-tracker-design",
  "userType": "external",
  "type": "user",
  "timestamp": "2026-02-08T00:34:34.915Z"
}
```
```
message.content (19,464 chars):
<teammate-message teammate_id="ux-researcher" color="blue"
    summary="Comprehensive UX research report for TODO tracker CLI">

# UX Research Report: TODO Tracker CLI

## Executive Summary
This report synthesizes research from industry-leading CLI tools...
</teammate-message>
```

> The message is injected as a `user` turn — from the LLM's perspective, it looks like
> the user sent this. The `<teammate-message>` XML wrapper identifies the source.
> `isSidechain: false` — this is part of the main conversation thread, not a child.

---

### Step 11a — Parent calls TaskList to check progress (parent entry [39]→[40])

After receiving the report, the parent polls the shared task list.

**Parent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_018yMS5WHP65n8TobRPfhgtL",
  "name": "TaskList",
  "input": {}
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_018yMS5WHP65n8TobRPfhgtL",
  "type": "tool_result",
  "content": "#3 [completed] Play devil's advocate on the TODO tracker CLI concept (devils-advocate)\n#2 [in_progress] Design technical architecture for the TODO tracker CLI (tech-architect)\n#1 [completed] Research UX patterns for the TODO tracker CLI (ux-researcher)"
}
```

> Parent sees 2/3 tasks completed. Tech-architect still working. No action needed — just wait.

### Step 11b — Subagent marks task completed (subagent entry [68]→[69])

Back in the subagent, after sending the report, it updates the task status.

**Subagent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_01A6NK4iKcnLj8pFg7ZfmbQ6",
  "name": "TaskUpdate",
  "input": {
    "taskId": "1",
    "status": "completed"
  }
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_01A6NK4iKcnLj8pFg7ZfmbQ6",
  "type": "tool_result",
  "content": "Updated task #1 status\n\nTask completed. Call TaskList now to find your next available task or see if your work unblocked others."
}
```

> The system hints the agent should call TaskList to find more work.
> The subagent does call TaskList (entry [70]) and finds no unblocked tasks, then idles.

---

### Step 12 — Parent sends shutdown request (parent entry [46]→[47])

After all 3 reports are in, the parent initiates graceful shutdown.

**Parent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_01HNfqL9eWS4UoTM3wSykJJU",
  "name": "SendMessage",
  "input": {
    "type": "shutdown_request",
    "recipient": "ux-researcher",
    "content": "Your report is delivered. Thanks, shutting down the team."
  }
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_01HNfqL9eWS4UoTM3wSykJJU",
  "type": "tool_result",
  "content": [{
    "type": "text",
    "text": "{\"success\":true,\"message\":\"Shutdown request sent to ux-researcher. Request ID: shutdown-1770510895171@ux-researcher\",\"request_id\":\"shutdown-1770510895171@ux-researcher\",\"target\":\"ux-researcher\"}"
  }]
}
```

> The `request_id` format is `shutdown-{unix_ms}@{agent_name}`.
> This goes into ux-researcher's inbox and triggers a new agent instance.

---

### Step 13 — Shutdown handler instance (agent-abbb479, entries [0]→[1]→[2])

A **separate agent instance** `agent-abbb479` handles the shutdown request.
This is NOT the same instance as `agent-ab4ea94` (the primary worker) — it has a
different `agentId`, a separate log file, and no shared conversation history (its
`parentUuid` does not appear in `ab4ea94`'s UUID chain).

**Entry [0] — user (shutdown request delivered):**
```json
{
  "agentId": "abbb479",
  "isSidechain": true,
  "type": "user",
  "timestamp": "2026-02-08T00:34:56.696Z"
}
```
```
message.content:
<teammate-message teammate_id="team-lead">
{"type":"shutdown_request",
 "requestId":"shutdown-1770510895171@ux-researcher",
 "from":"team-lead",
 "reason":"Your report is delivered. Thanks, shutting down the team.",
 "timestamp":"2026-02-08T00:34:55.171Z"}
</teammate-message>
```

> The shutdown request is delivered as a JSON payload inside `<teammate-message>`.
> Note the different `agentId: "abbb479"` — this is a separate agent instance.

**Entry [1] — assistant (approves shutdown):**
```json
{
  "type": "tool_use",
  "id": "toolu_01NiMowEyTJgAzc1PaLgsL1d",
  "name": "SendMessage",
  "input": {
    "type": "shutdown_response",
    "request_id": "shutdown-1770510895171@ux-researcher",
    "approve": true,
    "content": "Report delivered, happy to shut down. Thanks!"
  }
}
```

**Entry [2] — user (confirmation, process exits):**
```json
{
  "type": "user",
  "agentId": "abbb479",
  "timestamp": "2026-02-08T00:35:00.039Z",
  "toolUseResult": {
    "success": true,
    "message": "Shutdown approved. Sent confirmation to team-lead. Agent ux-researcher is now exiting.",
    "request_id": "shutdown-1770510895171@ux-researcher"
  }
}
```

> After the `shutdown_response(approve=true)`, the agent instance exits.
> The `toolUseResult` field is embedded directly in the entry metadata here —
> this is a special pattern for shutdown confirmations.

---

### Step 14 — TeamDelete (parent entry [56]→[57])

After all shutdown acknowledgements, the parent cleans up.

**Parent assistant → tool_use:**
```json
{
  "type": "tool_use",
  "id": "toolu_01VpfwRX2sLB7eBCo5sQCuvH",
  "name": "TeamDelete",
  "input": {}
}
```

**System → tool_result:**
```json
{
  "tool_use_id": "toolu_01VpfwRX2sLB7eBCo5sQCuvH",
  "type": "tool_result",
  "content": [{
    "type": "text",
    "text": "{\"success\":true,\"message\":\"Cleaned up directories and worktrees for team \\\"todo-tracker-design\\\"\",\"team_name\":\"todo-tracker-design\"}"
  }]
}
```

> Removes `~/.claude/teams/todo-tracker-design/` and `~/.claude/tasks/todo-tracker-design/`.
> The team and all shared state are gone. Only the JSONL log files remain.

---

### Summary: What crosses the wire at each step

```
Step  Direction           Tool             Payload Key
────  ──────────────────  ───────────────  ──────────────────────────────────
 1    parent → system     TeamCreate       team_name, description
      system → parent     (result)         team_file_path, lead_agent_id
 2    parent → system     TaskCreate       subject, description, activeForm
      system → parent     (result)         "Task #1 created successfully"
 3    parent → system     TaskUpdate       taskId=1, owner, status
      system → parent     (result)         "Updated task #1 owner, status"
 4    parent → system     Task             prompt, name, team_name, model, bg
      system → parent     (result)         agent_id, "running via mailbox"
 5    system → subagent   (user message)   <teammate-message> with prompt
 6    subagent → system   TaskGet          taskId=1
      system → subagent   (result)         full task subject + description
 7    subagent → system   TaskUpdate       taskId=1, status, activeForm
      system → subagent   (result)         "Updated task #1 activeForm"
 8    subagent → system   WebSearch ×12    (research queries)
      system → subagent   (results ×12)    search results
 9    subagent → system   SendMessage      type=message, recipient, content
      system → subagent   (result)         routing confirmation
10    system → parent     (user message)   <teammate-message> with report
11a   parent → system     TaskList         (empty)
      system → parent     (result)         task status summary
11b   subagent → system   TaskUpdate       taskId=1, status=completed
      system → subagent   (result)         "Task completed"
12    parent → system     SendMessage      type=shutdown_request, recipient
      system → parent     (result)         request_id
13    system → new-proc   (user message)   <teammate-message> shutdown JSON
      new-proc → system   SendMessage      type=shutdown_response, approve
      system → new-proc   (result)         "Agent now exiting"
14    parent → system     TeamDelete       (empty)
      system → parent     (result)         "Cleaned up directories"
```

---

## 10. Key Takeaways

1. **Ephemeral agent instance model** — Teammates are not long-lived, stateful entities. Each message to a teammate creates a new agent instance with a fresh `agentId`, a separate log file, and no shared conversation history (verified: the shutdown handler's `parentUuid` does not appear in the primary worker's UUID chain). The "identity" is maintained by the logical name and shared task list, not by instance continuity. Whether each instance is an OS-level process, thread, or other construct is not visible in the logs.

2. **Task list as shared state** — The `~/.claude/tasks/{team}/` directory is the coordination backbone. The parent writes task definitions and assignments; workers read assignments and update status. `TaskList` serves as the polling mechanism for the parent to track progress.

3. **Parallel-by-default** — The parent spawns all 3 agents with `run_in_background=true` and waits for reports to arrive as `<teammate-message>` events. Workers also parallelize their own work (batched WebSearch calls).

4. **Structured shutdown protocol** — The parent doesn't just kill processes. It sends `shutdown_request` messages, and each agent has the opportunity to `approve` or `reject` the shutdown via `shutdown_response`.

5. **Report delivery via messaging, not files** — Workers send their full reports (up to 30KB) as `SendMessage` content. The parent receives them inline as user-turn messages. No intermediate files are written.

6. **Redundant instance creation** — The system sometimes creates unnecessary agent instances (task-reassignment handlers for already-completed tasks). This is a side effect of the mailbox model where every inbound message creates a new handler instance.
