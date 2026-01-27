.PHONY: build test clean

# Build the yoloplanner binary
build:
	go build -o yoloplanner ./cmd

# Run all tests
test:
	go test ./...

# Run tests with verbose output
test-v:
	go test -v ./...

# Clean build artifacts
clean:
	rm -f yoloplanner

# Build and test
all: build test
