.PHONY: build test clean

# Build the yoloswe binary
build:
	go build -o yoloswe ./cmd

# Run all tests
test:
	go test ./...

# Run tests with verbose output
test-v:
	go test -v ./...

# Clean build artifacts
clean:
	rm -f yoloswe

# Build and test
all: build test
