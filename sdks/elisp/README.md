# Claude CLI SDK for Emacs

An Emacs Lisp SDK for interacting with the Claude CLI over the NDJSON streaming protocol.

## Requirements

- Emacs 27.1 or later
- Claude CLI installed and available in PATH (or specify path via `claude-cli-cli-path`)

## Installation

Add the `sdks/elisp` directory to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/sdks/elisp")
(require 'claude-cli)
```

## Quick Start

### Basic Usage (Blocking)

```elisp
;; Create and start a session
(setq my-session (claude-cli-create-session :model "haiku"
                                            :permission-mode 'bypass))
(claude-cli-start my-session)

;; Send a message and wait for response
(let ((result (claude-cli-ask my-session "What is 2+2?")))
  (message "Answer: %s" (claude-cli-turn-result-result-text result))
  (message "Cost: $%.6f" (claude-cli-turn-usage-cost-usd
                          (claude-cli-turn-result-usage result))))

;; Clean up
(claude-cli-stop my-session)
```

### Streaming with Hooks

```elisp
;; Set up event handlers
(add-hook 'claude-cli-text-hook
          (lambda (event)
            (insert (claude-cli-text-event-text event))))

(add-hook 'claude-cli-turn-complete-hook
          (lambda (event)
            (message "Done! Tokens: %d"
                     (claude-cli-turn-usage-output-tokens
                      (claude-cli-turn-complete-event-usage event)))))

;; Start session and send message
(claude-cli-start my-session)
(claude-cli-send-message my-session "Write a haiku about Emacs")

;; Process events
(while (claude-cli-session-processing-p my-session)
  (accept-process-output nil 0.1))
```

## API Reference

### Session Creation

```elisp
(claude-cli-create-session &rest options)
```

Create a new session with options:
- `:model` - Model name (default: "haiku")
- `:work-dir` - Working directory
- `:permission-mode` - One of: `default`, `accept-edits`, `plan`, `bypass`
- `:cli-path` - Custom CLI path
- `:disable-plugins` - Disable plugins (faster startup)
- `:permission-handler` - Function to handle permission requests

### Session Lifecycle

```elisp
(claude-cli-start session)           ; Start the CLI process
(claude-cli-stop session)            ; Stop gracefully
```

### Messaging

```elisp
(claude-cli-send-message session content)  ; Non-blocking, returns turn number
(claude-cli-ask session content &optional timeout)  ; Blocking, returns result
```

### Control

```elisp
(claude-cli-set-permission-mode session mode)  ; Change permission mode
(claude-cli-interrupt session)                  ; Stop current operation
```

### Query

```elisp
(claude-cli-session-get-info session)      ; Session metadata
(claude-cli-session-get-state session)     ; Current state symbol
(claude-cli-session-ready-p session)       ; Check if ready
(claude-cli-session-processing-p session)  ; Check if processing
(claude-cli-current-turn-number session)   ; Current turn number
```

## Event Hooks

Events are dispatched via standard Emacs hook variables:

| Hook | Event Type | Description |
|------|------------|-------------|
| `claude-cli-ready-hook` | `claude-cli-ready-event` | Session initialized |
| `claude-cli-text-hook` | `claude-cli-text-event` | Streaming text chunk |
| `claude-cli-thinking-hook` | `claude-cli-thinking-event` | Extended thinking |
| `claude-cli-tool-start-hook` | `claude-cli-tool-start-event` | Tool started |
| `claude-cli-tool-progress-hook` | `claude-cli-tool-progress-event` | Partial tool input |
| `claude-cli-tool-complete-hook` | `claude-cli-tool-complete-event` | Tool input complete |
| `claude-cli-cli-tool-result-hook` | `claude-cli-cli-tool-result-event` | Tool result |
| `claude-cli-turn-complete-hook` | `claude-cli-turn-complete-event` | Turn finished |
| `claude-cli-error-hook` | `claude-cli-error-event` | Error occurred |
| `claude-cli-state-change-hook` | `claude-cli-state-change-event` | State transition |
| `claude-cli-event-hook` | Any event | Unified hook |

## Permission Handling

Built-in permission handlers:

```elisp
;; Allow all tools (use with caution!)
(claude-cli-create-session :permission-handler #'claude-cli-permission-allow-all)

;; Deny all tools
(claude-cli-create-session :permission-handler #'claude-cli-permission-deny-all)

;; Interactive prompts
(claude-cli-create-session :permission-handler #'claude-cli-permission-interactive)

;; Allow only read-only tools
(claude-cli-create-session :permission-handler (claude-cli-permission-allow-reads-only))
```

Custom handler example:

```elisp
(defun my-permission-handler (tool-name input)
  "Custom handler that allows Read but prompts for others."
  (if (string= tool-name "Read")
      (claude-cli-permission-allow)
    (claude-cli-permission-interactive tool-name input)))

(claude-cli-create-session :permission-handler #'my-permission-handler)
```

## Customization

```elisp
(setq claude-cli-default-model "sonnet")           ; Default model
(setq claude-cli-default-permission-mode 'default) ; Default permission mode
(setq claude-cli-cli-path "/path/to/claude")       ; CLI binary path
(setq claude-cli-ask-timeout 120)                  ; Timeout for claude-cli-ask (nil = unlimited)
```

## Claude Chat UI

The SDK includes a modern interactive chat interface with streaming responses, collapsible sections, and transient menus.

### Quick Start

```elisp
M-x claude-cli-chat
```

### Features

- **Streaming responses** with real-time text display
- **Magit-style collapsible sections** for turns, thinking, and tool calls
- **Transient menus** for model and permission mode switching
- **Token usage and cost tracking** in the header line
- **Export to Markdown, Org-mode, JSON, or plain text**
- **Input history** with M-p/M-n navigation

### Key Bindings

**In conversation area:**

| Key | Action |
|-----|--------|
| `n` / `p` | Next/previous turn |
| `M-n` / `M-p` | Next/previous section |
| `TAB` | Toggle section collapse/expand |
| `i` | Focus input area |
| `?` | Open command menu |
| `g` | Refresh display |
| `q` | Quit window |

**In input area:**

| Key | Action |
|-----|--------|
| `C-c C-c` | Send message |
| `C-RET` / `M-RET` | Send message |
| `M-p` / `M-n` | History previous/next |

**Global (work everywhere):**

| Key | Action |
|-----|--------|
| `C-c C-c` | Send message |
| `C-c C-k` | Interrupt current operation |
| `C-c C-n` | New session |
| `C-c C-q` | Close session |
| `C-c C-t` | Open transient menu |

### Requirements

The chat UI requires additional packages:
- `magit-section` - for collapsible sections
- `transient` - for keyboard menus

Install them via MELPA or your package manager.

## File Structure

```
sdks/elisp/
├── claude-cli.el             ; Main SDK entry point and session API
├── claude-cli-protocol.el    ; JSON/NDJSON protocol handling
├── claude-cli-process.el     ; CLI process management
├── claude-cli-events.el      ; Event types and hooks
├── claude-cli-state.el       ; State machine
├── claude-cli-turn.el        ; Turn tracking
├── claude-cli-permission.el  ; Permission handling
├── claude-cli-chat.el            ; Chat UI main entry point
├── claude-cli-chat-buffer.el     ; Buffer management and rendering
├── claude-cli-chat-sections.el   ; Magit-style collapsible sections
├── claude-cli-chat-input.el      ; Input area management
├── claude-cli-chat-faces.el      ; Face definitions for styling
├── claude-cli-chat-transient.el  ; Transient menu definitions
├── claude-cli-chat-navigation.el ; Navigation commands
├── claude-cli-chat-export.el     ; Export functionality
└── examples/
    ├── claude-cli-basic-example.el
    └── claude-cli-streaming-example.el
```

## Examples

Run the examples interactively:

```elisp
(require 'claude-cli-basic-example)
(claude-cli-example-basic)

(require 'claude-cli-streaming-example)
(claude-cli-example-streaming)
```

## License

See the main repository LICENSE file.
