package codex

import (
	"github.com/mzhaom/claude-cli-protocol/sdks/golang/codexprotocol"
)

// streamAccumulator accumulates streaming events into complete content.
type streamAccumulator struct {
	session *Session
}

// newStreamAccumulator creates a new stream accumulator.
func newStreamAccumulator(session *Session) *streamAccumulator {
	return &streamAccumulator{
		session: session,
	}
}

// HandleEvent processes an event message.
func (sa *streamAccumulator) HandleEvent(msg codexprotocol.EventMsg) {
	switch e := msg.(type) {
	case *codexprotocol.AgentMessageDeltaEvent:
		sa.handleAgentMessageDelta(e)
	case *codexprotocol.AgentReasoningDeltaEvent:
		sa.handleAgentReasoningDelta(e)
	case *codexprotocol.AgentReasoningRawContentDeltaEvent:
		sa.handleAgentReasoningRawDelta(e)
	}
}

func (sa *streamAccumulator) handleAgentMessageDelta(e *codexprotocol.AgentMessageDeltaEvent) {
	fullText := sa.session.turnManager.AppendText(e.Delta)
	sa.session.emit(TextEvent{
		TurnNumber: sa.session.turnManager.CurrentTurnNumber(),
		Text:       e.Delta,
		FullText:   fullText,
	})
}

func (sa *streamAccumulator) handleAgentReasoningDelta(e *codexprotocol.AgentReasoningDeltaEvent) {
	fullReasoning := sa.session.turnManager.AppendReasoning(e.Delta)
	sa.session.emit(ReasoningEvent{
		TurnNumber:    sa.session.turnManager.CurrentTurnNumber(),
		Reasoning:     e.Delta,
		FullReasoning: fullReasoning,
	})
}

func (sa *streamAccumulator) handleAgentReasoningRawDelta(e *codexprotocol.AgentReasoningRawContentDeltaEvent) {
	// Raw reasoning delta is also accumulated as reasoning
	fullReasoning := sa.session.turnManager.AppendReasoning(e.Delta)
	sa.session.emit(ReasoningEvent{
		TurnNumber:    sa.session.turnManager.CurrentTurnNumber(),
		Reasoning:     e.Delta,
		FullReasoning: fullReasoning,
	})
}

// Reset clears all accumulated state.
func (sa *streamAccumulator) Reset() {
	// Nothing to reset currently as we don't track block state
}
