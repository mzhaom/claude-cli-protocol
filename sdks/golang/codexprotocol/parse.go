package codexprotocol

import (
	"encoding/json"
	"fmt"
)

// RawEvent is used for initial type discrimination of events.
type RawEvent struct {
	Type EventMsgType    `json:"type"`
	Raw  json.RawMessage `json:"-"`
}

// ParseEvent parses a JSON event line into an Event struct.
func ParseEvent(data []byte) (*Event, error) {
	// First, extract the event from the Event wrapper
	var wrapper struct {
		ID  string          `json:"id"`
		Msg json.RawMessage `json:"msg"`
	}

	if err := json.Unmarshal(data, &wrapper); err != nil {
		return nil, fmt.Errorf("failed to parse event wrapper: %w", err)
	}

	msgData := wrapper.Msg
	if len(msgData) == 0 {
		return nil, fmt.Errorf("missing msg field in event")
	}

	msg, err := parseEventMsg(msgData)
	if err != nil {
		return nil, err
	}

	return &Event{
		ID:  wrapper.ID,
		Msg: msg,
	}, nil
}

// parseEventMsg parses the msg payload into the appropriate EventMsg type.
func parseEventMsg(msgData []byte) (EventMsg, error) {

	// Extract the type
	var raw struct {
		Type EventMsgType `json:"type"`
	}
	if err := json.Unmarshal(msgData, &raw); err != nil {
		return nil, fmt.Errorf("failed to extract event type: %w", err)
	}

	// Handle v1 wire format aliases
	eventType := raw.Type
	switch eventType {
	case "turn_started":
		eventType = EventTypeTurnStarted
	case "turn_complete":
		eventType = EventTypeTurnComplete
	}

	// Parse based on type
	switch eventType {
	case EventTypeSessionConfigured:
		var e SessionConfiguredEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeShutdownComplete:
		var e ShutdownCompleteEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeTurnStarted:
		var e TurnStartedEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeTurnComplete:
		var e TurnCompleteEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeTurnAborted:
		var e TurnAbortedEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeAgentMessage:
		var e AgentMessageEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeAgentMessageDelta:
		var e AgentMessageDeltaEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeAgentReasoning:
		var e AgentReasoningEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeAgentReasoningDelta:
		var e AgentReasoningDeltaEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeAgentReasoningRawContent:
		var e AgentReasoningRawContentEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeAgentReasoningRawContentDelta:
		var e AgentReasoningRawContentDeltaEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeUserMessage:
		var e UserMessageEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeExecApprovalRequest:
		var e ExecApprovalRequestEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeApplyPatchApprovalRequest:
		var e ApplyPatchApprovalRequestEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeElicitationRequest:
		var e ElicitationRequestEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeExecCommandBegin:
		var e ExecCommandBeginEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeExecCommandOutputDelta:
		var e ExecCommandOutputDeltaEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeExecCommandEnd:
		var e ExecCommandEndEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypePatchApplyBegin:
		var e PatchApplyBeginEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypePatchApplyEnd:
		var e PatchApplyEndEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeError:
		var e ErrorEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeWarning:
		var e WarningEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeTokenCount:
		var e TokenCountEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeBackgroundEvent:
		var e BackgroundEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeStreamError:
		var e StreamErrorEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeContextCompacted:
		var e ContextCompactedEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeThreadRolledBack:
		var e ThreadRolledBackEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeMcpToolCallBegin:
		var e McpToolCallBeginEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeMcpToolCallEnd:
		var e McpToolCallEndEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	case EventTypeUndoCompleted:
		var e UndoCompletedEvent
		if err := json.Unmarshal(msgData, &e); err != nil {
			return nil, err
		}
		return &e, nil

	default:
		// Return a generic event for unknown types
		return nil, fmt.Errorf("unknown event type: %s", raw.Type)
	}
}

// ParseSubmission parses a JSON submission line.
func ParseSubmission(data []byte) (*Submission, error) {
	var sub Submission
	if err := json.Unmarshal(data, &sub); err != nil {
		return nil, fmt.Errorf("failed to parse submission: %w", err)
	}
	return &sub, nil
}

// GetEventType extracts the event type from a raw event.
func GetEventType(data []byte) (EventMsgType, error) {
	var raw struct {
		Type EventMsgType `json:"type"`
	}
	if err := json.Unmarshal(data, &raw); err != nil {
		return "", fmt.Errorf("failed to extract event type: %w", err)
	}
	return raw.Type, nil
}

// GetOpType extracts the operation type from a raw operation.
func GetOpType(data []byte) (OpType, error) {
	var raw struct {
		Type OpType `json:"type"`
	}
	if err := json.Unmarshal(data, &raw); err != nil {
		return "", fmt.Errorf("failed to extract op type: %w", err)
	}
	return raw.Type, nil
}
