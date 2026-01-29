.PHONY: build install test clean

BINARY := codex-review
INSTALL_PATH := ~/bin/$(BINARY)

build:
	go build -o $(BINARY) ./cmd/

install: build
	cp $(BINARY) $(INSTALL_PATH)
	@echo "Installed to $(INSTALL_PATH)"

test:
	go test ./...

clean:
	rm -f $(BINARY)
