.PHONY: all build test test-unit test-integration lint fmt vet clean help

# Default target
all: lint build test-unit

# Build all packages
build:
	go build ./...

# Run all tests (unit only, no integration)
test: test-unit

# Run unit tests
test-unit:
	go test ./... -v

# Run integration tests (requires claude CLI)
test-integration:
	go test -tags=integration ./claude/... -v

# Run all tests including integration
test-all: test-unit test-integration

# Lint the code
lint: fmt vet
	@echo "Lint passed"

# Format code
fmt:
	gofmt -w .
	@if [ -n "$$(gofmt -l .)" ]; then \
		echo "Code not formatted. Run 'make fmt' to fix."; \
		gofmt -l .; \
		exit 1; \
	fi

# Vet code for suspicious constructs
vet:
	go vet ./...

# Run tests with race detector
test-race:
	go test -race ./...

# Run tests with coverage
test-coverage:
	go test -coverprofile=coverage.out ./...
	go tool cover -html=coverage.out -o coverage.html
	@echo "Coverage report: coverage.html"

# Clean build artifacts
clean:
	rm -f coverage.out coverage.html
	go clean ./...

# Show help
help:
	@echo "Available targets:"
	@echo "  all              - Run lint, build, and unit tests (default)"
	@echo "  build            - Build all packages"
	@echo "  test             - Run unit tests"
	@echo "  test-unit        - Run unit tests"
	@echo "  test-integration - Run integration tests (requires claude CLI)"
	@echo "  test-all         - Run both unit and integration tests"
	@echo "  test-race        - Run tests with race detector"
	@echo "  test-coverage    - Run tests with coverage report"
	@echo "  lint             - Run fmt and vet"
	@echo "  fmt              - Format code with gofmt"
	@echo "  vet              - Run go vet"
	@echo "  clean            - Clean build artifacts"
	@echo "  help             - Show this help"
