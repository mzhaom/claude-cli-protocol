.PHONY: all build test clean examples run-examples run-codex-basic run-basic run-streaming

# Default target
all: build test

# Build all packages
build:
	go build ./...

# Run all tests
test:
	go test ./... -v

# Run tests with coverage
test-coverage:
	go test ./... -coverprofile=coverage.out
	go tool cover -html=coverage.out -o coverage.html

# Build examples
examples: examples/codex_basic/codex_basic examples/basic/basic examples/streaming/streaming

examples/codex_basic/codex_basic: examples/codex_basic/main.go
	go build -o $@ ./examples/codex_basic

examples/basic/basic: examples/basic/main.go
	go build -o $@ ./examples/basic

examples/streaming/streaming: examples/streaming/main.go
	go build -o $@ ./examples/streaming

# Clean build artifacts
clean:
	rm -f examples/codex_basic/codex_basic
	rm -f examples/basic/basic
	rm -f examples/streaming/streaming
	rm -f coverage.out coverage.html

# Run all examples (codex only - claude examples require claude CLI)
run-examples: run-codex-basic
	@echo ""
	@echo "=== All examples completed successfully ==="

# Run codex basic example (uses codex app-server)
run-codex-basic: examples/codex_basic/codex_basic
	@echo "=== Running Codex Basic Example ==="
	./examples/codex_basic/codex_basic

# Run claude basic example (requires claude CLI)
run-basic: examples/basic/basic
	@echo "=== Running Claude Basic Example ==="
	./examples/basic/basic

# Run claude streaming example (requires claude CLI)
run-streaming: examples/streaming/streaming
	@echo "=== Running Claude Streaming Example ==="
	./examples/streaming/streaming

# Lint (requires golangci-lint)
lint:
	golangci-lint run ./...

# Format code
fmt:
	go fmt ./...

# Check for issues
vet:
	go vet ./...
