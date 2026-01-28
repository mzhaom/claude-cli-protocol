package codexprotocol

import (
	"encoding/json"
	"testing"
)

func TestParseEvent_SessionConfigured(t *testing.T) {
	data := `{"id":"sub_1","msg":{"type":"session_configured","session_id":"test-123","model":"o3-mini","approval_policy":"never","sandbox_policy":"read-only","cwd":"/tmp"}}`

	event, err := ParseEvent([]byte(data))
	if err != nil {
		t.Fatalf("ParseEvent failed: %v", err)
	}

	if event.ID != "sub_1" {
		t.Errorf("Expected ID 'sub_1', got %q", event.ID)
	}

	msg, ok := event.Msg.(*SessionConfiguredEvent)
	if !ok {
		t.Fatalf("Expected *SessionConfiguredEvent, got %T", event.Msg)
	}

	if msg.SessionID != "test-123" {
		t.Errorf("Expected SessionID 'test-123', got %q", msg.SessionID)
	}
	if msg.Model != "o3-mini" {
		t.Errorf("Expected Model 'o3-mini', got %q", msg.Model)
	}
}

func TestParseEvent_AgentMessageDelta(t *testing.T) {
	data := `{"id":"sub_1","msg":{"type":"agent_message_delta","delta":"Hello "}}`

	event, err := ParseEvent([]byte(data))
	if err != nil {
		t.Fatalf("ParseEvent failed: %v", err)
	}

	msg, ok := event.Msg.(*AgentMessageDeltaEvent)
	if !ok {
		t.Fatalf("Expected *AgentMessageDeltaEvent, got %T", event.Msg)
	}

	if msg.Delta != "Hello " {
		t.Errorf("Expected Delta 'Hello ', got %q", msg.Delta)
	}
}

func TestParseEvent_TurnStarted_V1Alias(t *testing.T) {
	// Test v1 wire format alias "task_started" -> TurnStartedEvent
	data := `{"id":"sub_1","msg":{"type":"task_started","turn_id":"turn-1"}}`

	event, err := ParseEvent([]byte(data))
	if err != nil {
		t.Fatalf("ParseEvent failed: %v", err)
	}

	_, ok := event.Msg.(*TurnStartedEvent)
	if !ok {
		t.Fatalf("Expected *TurnStartedEvent for task_started alias, got %T", event.Msg)
	}
}

func TestParseEvent_TurnComplete_V1Alias(t *testing.T) {
	// Test v1 wire format alias "task_complete" -> TurnCompleteEvent
	data := `{"id":"sub_1","msg":{"type":"task_complete","turn_id":"turn-1","last_agent_message":"Done"}}`

	event, err := ParseEvent([]byte(data))
	if err != nil {
		t.Fatalf("ParseEvent failed: %v", err)
	}

	msg, ok := event.Msg.(*TurnCompleteEvent)
	if !ok {
		t.Fatalf("Expected *TurnCompleteEvent for task_complete alias, got %T", event.Msg)
	}

	if msg.LastAgentMessage != "Done" {
		t.Errorf("Expected LastAgentMessage 'Done', got %q", msg.LastAgentMessage)
	}
}

func TestParseEvent_ExecApprovalRequest(t *testing.T) {
	data := `{"id":"sub_1","msg":{"type":"exec_approval_request","call_id":"call-1","turn_id":"turn-1","command":["ls","-la"],"cwd":"/tmp","parsed_cmd":[{"program":"ls","args":["-la"]}]}}`

	event, err := ParseEvent([]byte(data))
	if err != nil {
		t.Fatalf("ParseEvent failed: %v", err)
	}

	msg, ok := event.Msg.(*ExecApprovalRequestEvent)
	if !ok {
		t.Fatalf("Expected *ExecApprovalRequestEvent, got %T", event.Msg)
	}

	if msg.CallID != "call-1" {
		t.Errorf("Expected CallID 'call-1', got %q", msg.CallID)
	}
	if len(msg.Command) != 2 || msg.Command[0] != "ls" {
		t.Errorf("Expected Command ['ls', '-la'], got %v", msg.Command)
	}
}

func TestParseEvent_ExecCommandEnd(t *testing.T) {
	data := `{"id":"sub_1","msg":{"type":"exec_command_end","call_id":"call-1","turn_id":"turn-1","command":["echo","hello"],"cwd":"/tmp","exit_code":0,"timed_out":false}}`

	event, err := ParseEvent([]byte(data))
	if err != nil {
		t.Fatalf("ParseEvent failed: %v", err)
	}

	msg, ok := event.Msg.(*ExecCommandEndEvent)
	if !ok {
		t.Fatalf("Expected *ExecCommandEndEvent, got %T", event.Msg)
	}

	if msg.ExitCode != 0 {
		t.Errorf("Expected ExitCode 0, got %d", msg.ExitCode)
	}
	if msg.TimedOut != false {
		t.Errorf("Expected TimedOut false, got %v", msg.TimedOut)
	}
}

func TestParseEvent_Error(t *testing.T) {
	data := `{"id":"sub_1","msg":{"type":"error","message":"Something went wrong","codex_error_info":{"type":"bad_request"}}}`

	event, err := ParseEvent([]byte(data))
	if err != nil {
		t.Fatalf("ParseEvent failed: %v", err)
	}

	msg, ok := event.Msg.(*ErrorEvent)
	if !ok {
		t.Fatalf("Expected *ErrorEvent, got %T", event.Msg)
	}

	if msg.Message != "Something went wrong" {
		t.Errorf("Expected Message 'Something went wrong', got %q", msg.Message)
	}
	if msg.CodexErrorInfo == nil || msg.CodexErrorInfo.Type != "bad_request" {
		t.Errorf("Expected CodexErrorInfo.Type 'bad_request', got %v", msg.CodexErrorInfo)
	}
}

func TestSubmission_MarshalJSON(t *testing.T) {
	sub := Submission{
		ID: "sub_1",
		Op: &UserInputOp{
			Items: []UserInput{NewTextInput("Hello")},
		},
	}

	data, err := json.Marshal(sub)
	if err != nil {
		t.Fatalf("Marshal failed: %v", err)
	}

	// Verify it contains expected fields
	var result map[string]interface{}
	if err := json.Unmarshal(data, &result); err != nil {
		t.Fatalf("Unmarshal result failed: %v", err)
	}

	if result["id"] != "sub_1" {
		t.Errorf("Expected id 'sub_1', got %v", result["id"])
	}

	op, ok := result["op"].(map[string]interface{})
	if !ok {
		t.Fatalf("Expected op to be object, got %T", result["op"])
	}

	if op["type"] != "user_input" {
		t.Errorf("Expected op.type 'user_input', got %v", op["type"])
	}
}

func TestExecApprovalOp_MarshalJSON(t *testing.T) {
	op := NewExecApprovalOp("call-1", ReviewDecisionApproved)

	data, err := json.Marshal(op)
	if err != nil {
		t.Fatalf("Marshal failed: %v", err)
	}

	var result map[string]interface{}
	if err := json.Unmarshal(data, &result); err != nil {
		t.Fatalf("Unmarshal result failed: %v", err)
	}

	if result["type"] != "exec_approval" {
		t.Errorf("Expected type 'exec_approval', got %v", result["type"])
	}
	if result["id"] != "call-1" {
		t.Errorf("Expected id 'call-1', got %v", result["id"])
	}
	if result["decision"] != "approved" {
		t.Errorf("Expected decision 'approved', got %v", result["decision"])
	}
}
