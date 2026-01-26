#!/bin/bash
# Run all Claude Chat tests

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

/Applications/Emacs.app/Contents/MacOS/Emacs --batch \
  -L "$SCRIPT_DIR" \
  -L "$SCRIPT_DIR/test" \
  -l ert \
  --eval "(load-file \"$SCRIPT_DIR/test/claude-cli-chat-input-test.el\")" \
  --eval "(load-file \"$SCRIPT_DIR/test/claude-cli-chat-flow-test.el\")" \
  -f ert-run-tests-batch-and-exit
