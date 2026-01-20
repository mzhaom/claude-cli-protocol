# Makefile for Claude CLI Elisp SDK
#
# Usage:
#   make test           - Run unit tests
#   make chat-test      - Run chat UI tests (requires magit-section, transient)
#   make integration    - Run integration tests (requires Claude CLI)
#   make test-all       - Run all tests
#   make lint           - Check for common issues
#   make clean          - Remove compiled files

EMACS ?= emacs
EMACS_BATCH = $(EMACS) --batch -Q

# Directories
SRC_DIR = .
TEST_DIR = test

# Source files - SDK core
SDK_SOURCES = claude-cli.el \
              claude-cli-protocol.el \
              claude-cli-process.el \
              claude-cli-events.el \
              claude-cli-state.el \
              claude-cli-turn.el \
              claude-cli-permission.el

# Source files - Chat UI
CHAT_SOURCES = claude-cli-chat.el \
               claude-cli-chat-buffer.el \
               claude-cli-chat-sections.el \
               claude-cli-chat-input.el \
               claude-cli-chat-faces.el \
               claude-cli-chat-transient.el \
               claude-cli-chat-navigation.el \
               claude-cli-chat-export.el

SOURCES = $(SDK_SOURCES) $(CHAT_SOURCES)

# Test files
UNIT_TESTS = $(TEST_DIR)/claude-test.el
CHAT_TESTS = $(TEST_DIR)/claude-cli-chat-journey-test.el
INTEGRATION_TESTS = $(TEST_DIR)/claude-integration-test.el

# Load path
LOAD_PATH = -L $(SRC_DIR) -L $(TEST_DIR)

.PHONY: all test unit-test chat-test integration lint clean compile check-emacs

all: test

# Check Emacs is available
check-emacs:
	@which $(EMACS) > /dev/null || (echo "Error: Emacs not found. Set EMACS=/path/to/emacs" && exit 1)
	@echo "Using Emacs: $$($(EMACS) --version | head -1)"

# Run unit tests
test: unit-test

unit-test: check-emacs
	@echo "Running unit tests..."
	$(EMACS_BATCH) $(LOAD_PATH) \
		-l ert \
		-l claude-test \
		-f ert-run-tests-batch-and-exit

# Run chat UI tests (requires magit-section and transient packages)
chat-test: check-emacs
	@echo "Running chat UI tests..."
	@echo "Note: Requires magit-section and transient packages"
	$(EMACS_BATCH) $(LOAD_PATH) \
		-l ert \
		-l claude-cli-chat-journey-test \
		--eval "(ert-run-tests-batch-and-exit '(tag :chat))" || \
	$(EMACS_BATCH) $(LOAD_PATH) \
		-l claude-cli-chat-journey-test \
		-f claude-cli-chat-test-run-all

# Run integration tests (requires Claude CLI and API key)
integration: check-emacs
	@echo "Running integration tests..."
	@echo "Note: This requires Claude CLI installed and configured"
	$(EMACS_BATCH) $(LOAD_PATH) \
		-l claude-integration-test \
		-f claude-integration-test-run-all

# Run all tests
test-all: unit-test chat-test integration

# Byte-compile all source files
compile: check-emacs
	@echo "Byte-compiling..."
	$(EMACS_BATCH) $(LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(SOURCES)

# Lint: check for common issues
lint: check-emacs
	@echo "Checking for issues..."
	@$(EMACS_BATCH) $(LOAD_PATH) \
		--eval "(require 'checkdoc)" \
		--eval "(setq checkdoc-spellcheck-documentation-flag nil)" \
		--eval "(dolist (f '($(SOURCES))) \
		          (with-current-buffer (find-file-noselect f) \
		            (checkdoc-current-buffer t)))" \
		2>&1 | grep -v "^Wrote" || true
	@echo "Lint complete"

# Clean compiled files
clean:
	rm -f *.elc $(TEST_DIR)/*.elc

# Help
help:
	@echo "Claude CLI Elisp SDK Makefile"
	@echo ""
	@echo "Targets:"
	@echo "  test         - Run unit tests (default)"
	@echo "  unit-test    - Run unit tests"
	@echo "  chat-test    - Run chat UI tests (requires magit-section, transient)"
	@echo "  integration  - Run integration tests (requires Claude CLI)"
	@echo "  test-all     - Run all tests"
	@echo "  compile      - Byte-compile source files"
	@echo "  lint         - Check for common issues"
	@echo "  clean        - Remove compiled files"
	@echo ""
	@echo "Variables:"
	@echo "  EMACS        - Path to Emacs (default: emacs)"
	@echo ""
	@echo "Examples:"
	@echo "  make test"
	@echo "  make chat-test"
	@echo "  make integration"
	@echo "  EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs make test"
