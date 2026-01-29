module github.com/mzhaom/claude-cli-protocol/yoloswe

go 1.22

require (
	github.com/mzhaom/claude-cli-protocol/codex-review v0.0.0
	github.com/mzhaom/claude-cli-protocol/sdks/golang v0.0.0
)

replace github.com/mzhaom/claude-cli-protocol/sdks/golang => ../sdks/golang

replace github.com/mzhaom/claude-cli-protocol/codex-review => ../codex-review
