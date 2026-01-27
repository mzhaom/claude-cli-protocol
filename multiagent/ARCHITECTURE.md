# Architecture

## Design Goals

1. **Hierarchical Control**: Orchestrator → Planner → Sub-agents
2. **Separation of Concerns**: Each agent has a single responsibility
3. **Full Observability**: All I/O logged for debugging and analysis
4. **Autonomous Execution**: Agents work without user intervention once started
5. **Cost Control**: Budget enforcement across all agents

## Agent Hierarchy

### Level 0: Orchestrator (User-Facing)

The Orchestrator is the entry point for all user interactions.

**Responsibilities:**
- Receive and interpret user requests
- Triage: simple questions vs complex tasks
- Answer simple questions directly (code explanation, architecture discussion)
- Delegate complex tasks to Planner
- Report results back to user

**Lifecycle:** Long-running (persists for entire user session)

**Tools:** `delegate_to_planner(mission)`

### Level 1: Planner (Task Coordinator)

The Planner handles complex multi-step tasks by coordinating specialized sub-agents.

**Responsibilities:**
- Break down missions into discrete tasks
- Call Designer for technical designs
- Call Builder for implementation
- Call Reviewer for feedback
- Decide when work is complete (not Reviewer)
- Manage iteration cycles (design → build → review → fix)

**Lifecycle:** Long-running (persists for entire mission)

**Tools:** `designer()`, `builder()`, `reviewer()`

### Level 2: Sub-Agents (Specialists)

#### Designer
- Creates technical designs based on requirements
- Outputs: architecture, file structure, interfaces, implementation notes
- Does NOT write code

#### Builder
- Implements code based on designs
- Can create/modify files, run tests, run builds
- Reports files changed and test results

#### Reviewer
- Reviews code changes and provides feedback
- Reports issues (critical, minor, nitpick)
- Does NOT approve or reject - only provides feedback
- Planner decides what to do with feedback

**Lifecycle:** Ephemeral (created fresh per task, destroyed after)

## Communication Patterns

### Long-Running Agent Communication

Orchestrator and Planner use `SendMessage()` + `WaitForTurn()`:

```go
// Orchestrator sends mission to Planner
_, err := o.plannerSession.SendMessage(ctx, formatMissionMessage(mission))
result, err := o.plannerSession.WaitForTurn(ctx)
```

This allows multi-turn conversations with accumulated context.

### Ephemeral Agent Communication

Designer, Builder, Reviewer use `Ask()` for single-turn request-response:

```go
// Planner calls Designer
designer := NewEphemeralSession(config)
result, err := designer.Execute(ctx, formatDesignPrompt(req))
// Session automatically stopped after
```

Each task gets a fresh session with no context pollution.

## Session Types

### LongRunningSession

```go
type LongRunningSession struct {
    session    *claude.Session
    sessionDir string
    role       AgentRole
    started    bool
    turnCount  int
    totalCost  float64
}
```

- Started once, runs until explicitly stopped
- Maintains conversation history across turns
- Single session directory for all turns

### EphemeralSession

```go
type EphemeralSession struct {
    config          AgentConfig
    swarmSessionID  string
    baseSessionDir  string
    taskCounter     int
    totalCost       float64
}
```

- Creates new `claude.Session` for each `Execute()` call
- Each task gets its own subdirectory (task-001, task-002, etc.)
- No context shared between tasks

## Inter-Agent Protocol

### DesignRequest / DesignResponse

```go
type DesignRequest struct {
    Task        string   `json:"task"`
    Context     string   `json:"context,omitempty"`
    Constraints []string `json:"constraints,omitempty"`
}

type DesignResponse struct {
    Architecture        string     `json:"architecture"`
    Files               []FileSpec `json:"files"`
    Interfaces          string     `json:"interfaces,omitempty"`
    ImplementationNotes []string   `json:"implementation_notes,omitempty"`
    Dependencies        []string   `json:"dependencies,omitempty"`
    Risks               []string   `json:"risks,omitempty"`
}
```

### BuildRequest / BuildResponse

```go
type BuildRequest struct {
    Task     string          `json:"task"`
    Design   *DesignResponse `json:"design,omitempty"`
    WorkDir  string          `json:"work_dir"`
    Feedback *ReviewResponse `json:"feedback,omitempty"`
}

type BuildResponse struct {
    FilesCreated  []string `json:"files_created"`
    FilesModified []string `json:"files_modified"`
    TestsRun      bool     `json:"tests_run"`
    TestsPassed   bool     `json:"tests_passed"`
    TestOutput    string   `json:"test_output,omitempty"`
    BuildOutput   string   `json:"build_output,omitempty"`
    Notes         string   `json:"notes,omitempty"`
}
```

### ReviewRequest / ReviewResponse

```go
type ReviewRequest struct {
    Task           string          `json:"task"`
    FilesChanged   []string        `json:"files_changed"`
    OriginalDesign *DesignResponse `json:"original_design,omitempty"`
}

type ReviewResponse struct {
    Summary     string   `json:"summary"`
    Issues      []Issue  `json:"issues"`
    Positives   []string `json:"positives,omitempty"`
    Suggestions []string `json:"suggestions,omitempty"`
}

type Issue struct {
    Severity   string `json:"severity"` // "critical", "minor", "nitpick"
    File       string `json:"file"`
    Line       int    `json:"line,omitempty"`
    Message    string `json:"message"`
    Suggestion string `json:"suggestion,omitempty"`
}
```

## Session Storage Structure

```
.claude-swarm/sessions/<session-id>/
├── orchestrator/           # Long-running: single directory
│   ├── to_cli.jsonl        # All messages sent to CLI
│   ├── from_cli.jsonl      # All responses from CLI
│   └── metadata.json       # Session metadata
├── planner/                # Long-running: single directory
│   ├── to_cli.jsonl
│   ├── from_cli.jsonl
│   └── metadata.json
├── designer/               # Ephemeral: per-task directories
│   ├── task-001/
│   │   ├── to_cli.jsonl
│   │   ├── from_cli.jsonl
│   │   └── metadata.json
│   └── task-002/
│       └── ...
├── builder/                # Ephemeral: per-task directories
│   └── ...
├── reviewer/               # Ephemeral: per-task directories
│   └── ...
└── summary.json            # Aggregated costs and timeline
```

## Permission Model

All agents run with `PermissionModeBypass` for full autonomy:

- Builder can create/modify files without prompts
- Builder can run tests and build commands
- No user intervention required during execution

## Cost Tracking

Each agent tracks its own costs:

```go
type Agent interface {
    TotalCost() float64
}
```

The Orchestrator aggregates costs from all agents into `SwarmSummary`:

```go
type SwarmSummary struct {
    SessionID        string
    TotalCost        float64
    AgentCosts       map[string]float64
    OrchestratorTurns int
    PlannerTurns     int
    DesignerTasks    int
    BuilderTasks     int
    ReviewerTasks    int
}
```

## Workflow Example

1. User: "Build a hello world CLI in Go"

2. **Orchestrator** receives request, determines it's complex, delegates to Planner

3. **Planner** analyzes mission, creates task list:
   - Task 1: Design CLI structure
   - Task 2: Implement CLI
   - Task 3: Add tests

4. **Planner** calls **Designer** with Task 1
   - Designer returns: architecture, files to create, interfaces

5. **Planner** calls **Builder** with design
   - Builder creates files, runs tests, returns results

6. **Planner** calls **Reviewer** with changed files
   - Reviewer reports: 1 critical issue (missing error handling)

7. **Planner** decides to fix, calls **Builder** again with feedback
   - Builder fixes issue, tests pass

8. **Planner** calls **Reviewer** again
   - Reviewer reports: no critical issues, 1 minor suggestion

9. **Planner** decides work is complete, returns to Orchestrator

10. **Orchestrator** summarizes results to user
