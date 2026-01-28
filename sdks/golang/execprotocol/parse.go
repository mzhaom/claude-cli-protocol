package execprotocol

import (
	"encoding/json"
	"fmt"
)

// ParseEvent parses a JSON line from codex exec --json output.
func ParseEvent(data []byte) (*ThreadEvent, error) {
	// First extract the type
	var typeOnly struct {
		Type EventType `json:"type"`
	}
	if err := json.Unmarshal(data, &typeOnly); err != nil {
		return nil, fmt.Errorf("failed to extract event type: %w", err)
	}

	switch typeOnly.Type {
	case EventTypeThreadStarted:
		var ev struct {
			Type     EventType `json:"type"`
			ThreadID string    `json:"thread_id"`
		}
		if err := json.Unmarshal(data, &ev); err != nil {
			return nil, err
		}
		return &ThreadEvent{
			Type:     ev.Type,
			ThreadID: ev.ThreadID,
		}, nil

	case EventTypeTurnStarted:
		return &ThreadEvent{Type: EventTypeTurnStarted}, nil

	case EventTypeTurnCompleted:
		var ev struct {
			Type  EventType `json:"type"`
			Usage Usage     `json:"usage"`
		}
		if err := json.Unmarshal(data, &ev); err != nil {
			return nil, err
		}
		return &ThreadEvent{
			Type:  ev.Type,
			Usage: &ev.Usage,
		}, nil

	case EventTypeTurnFailed:
		var ev struct {
			Type  EventType `json:"type"`
			Error ErrorInfo `json:"error"`
		}
		if err := json.Unmarshal(data, &ev); err != nil {
			return nil, err
		}
		return &ThreadEvent{
			Type:  ev.Type,
			Error: &ev.Error,
		}, nil

	case EventTypeItemStarted, EventTypeItemUpdated, EventTypeItemCompleted:
		item, err := parseItem(data)
		if err != nil {
			return nil, err
		}
		return &ThreadEvent{
			Type: typeOnly.Type,
			Item: item,
		}, nil

	case EventTypeError:
		var ev struct {
			Type    EventType `json:"type"`
			Message string    `json:"message"`
		}
		if err := json.Unmarshal(data, &ev); err != nil {
			return nil, err
		}
		return &ThreadEvent{
			Type:    ev.Type,
			Message: ev.Message,
		}, nil

	default:
		return nil, fmt.Errorf("unknown event type: %s", typeOnly.Type)
	}
}

// parseItem parses an item from the JSON data.
func parseItem(data []byte) (*ThreadItem, error) {
	// Extract the item wrapper
	var wrapper struct {
		Item json.RawMessage `json:"item"`
	}
	if err := json.Unmarshal(data, &wrapper); err != nil {
		return nil, fmt.Errorf("failed to extract item: %w", err)
	}

	// Extract item type
	var itemType struct {
		ID   string   `json:"id"`
		Type ItemType `json:"type"`
	}
	if err := json.Unmarshal(wrapper.Item, &itemType); err != nil {
		return nil, fmt.Errorf("failed to extract item type: %w", err)
	}

	item := &ThreadItem{
		ID:   itemType.ID,
		Type: itemType.Type,
		Raw:  wrapper.Item,
	}

	return item, nil
}

// ParseAgentMessage parses an agent message item from a ThreadItem.
func ParseAgentMessage(item *ThreadItem) (*AgentMessageItem, error) {
	if item.Type != ItemTypeAgentMessage {
		return nil, fmt.Errorf("expected agent_message, got %s", item.Type)
	}
	var msg AgentMessageItem
	if err := json.Unmarshal(item.Raw, &msg); err != nil {
		return nil, err
	}
	return &msg, nil
}

// ParseReasoning parses a reasoning item from a ThreadItem.
func ParseReasoning(item *ThreadItem) (*ReasoningItem, error) {
	if item.Type != ItemTypeReasoning {
		return nil, fmt.Errorf("expected reasoning, got %s", item.Type)
	}
	var r ReasoningItem
	if err := json.Unmarshal(item.Raw, &r); err != nil {
		return nil, err
	}
	return &r, nil
}

// ParseCommandExecution parses a command execution item from a ThreadItem.
func ParseCommandExecution(item *ThreadItem) (*CommandExecutionItem, error) {
	if item.Type != ItemTypeCommandExecution {
		return nil, fmt.Errorf("expected command_execution, got %s", item.Type)
	}
	var cmd CommandExecutionItem
	if err := json.Unmarshal(item.Raw, &cmd); err != nil {
		return nil, err
	}
	return &cmd, nil
}

// ParseFileChange parses a file change item from a ThreadItem.
func ParseFileChange(item *ThreadItem) (*FileChangeItem, error) {
	if item.Type != ItemTypeFileChange {
		return nil, fmt.Errorf("expected file_change, got %s", item.Type)
	}
	var fc FileChangeItem
	if err := json.Unmarshal(item.Raw, &fc); err != nil {
		return nil, err
	}
	return &fc, nil
}

// ParseMcpToolCall parses an MCP tool call item from a ThreadItem.
func ParseMcpToolCall(item *ThreadItem) (*McpToolCallItem, error) {
	if item.Type != ItemTypeMcpToolCall {
		return nil, fmt.Errorf("expected mcp_tool_call, got %s", item.Type)
	}
	var tc McpToolCallItem
	if err := json.Unmarshal(item.Raw, &tc); err != nil {
		return nil, err
	}
	return &tc, nil
}

// ParseWebSearch parses a web search item from a ThreadItem.
func ParseWebSearch(item *ThreadItem) (*WebSearchItem, error) {
	if item.Type != ItemTypeWebSearch {
		return nil, fmt.Errorf("expected web_search, got %s", item.Type)
	}
	var ws WebSearchItem
	if err := json.Unmarshal(item.Raw, &ws); err != nil {
		return nil, err
	}
	return &ws, nil
}

// ParseTodoList parses a to-do list item from a ThreadItem.
func ParseTodoList(item *ThreadItem) (*TodoListItem, error) {
	if item.Type != ItemTypeTodoList {
		return nil, fmt.Errorf("expected todo_list, got %s", item.Type)
	}
	var tl TodoListItem
	if err := json.Unmarshal(item.Raw, &tl); err != nil {
		return nil, err
	}
	return &tl, nil
}

// ParseErrorItem parses an error item from a ThreadItem.
func ParseErrorItem(item *ThreadItem) (*ErrorItem, error) {
	if item.Type != ItemTypeError {
		return nil, fmt.Errorf("expected error, got %s", item.Type)
	}
	var ei ErrorItem
	if err := json.Unmarshal(item.Raw, &ei); err != nil {
		return nil, err
	}
	return &ei, nil
}
