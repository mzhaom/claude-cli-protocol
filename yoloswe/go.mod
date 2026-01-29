module github.com/mzhaom/claude-cli-protocol/yoloswe

go 1.22

require (
	github.com/mzhaom/claude-cli-protocol/codex-review v0.0.0
	github.com/mzhaom/claude-cli-protocol/sdks/golang v0.0.0
	github.com/spf13/cobra v1.8.1
)

require github.com/inconshreveable/mousetrap v1.1.0 // indirect

require github.com/spf13/pflag v1.0.5 // indirect

replace github.com/mzhaom/claude-cli-protocol/sdks/golang => ../sdks/golang

replace github.com/mzhaom/claude-cli-protocol/codex-review => ../codex-review
