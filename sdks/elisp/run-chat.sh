#!/bin/bash
# Quick launcher for testing Claude CLI Chat in Emacs
#
# Usage: ./run-chat.sh [options]
#
# Options:
#   -t, --terminal    Run in terminal mode (no GUI)
#   -d, --debug       Debug init (load minimal config, show errors)
#   -q, --quick       Skip user config entirely (-Q flag)
#   -h, --help        Show this help
#
# This starts Emacs with normal user config, loads the SDK, and opens the chat.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Parse options
TERMINAL_MODE=false
DEBUG_INIT=false
QUICK_MODE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -t|--terminal)
            TERMINAL_MODE=true
            shift
            ;;
        -d|--debug)
            DEBUG_INIT=true
            shift
            ;;
        -q|--quick)
            QUICK_MODE=true
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  -t, --terminal    Run in terminal mode (no GUI)"
            echo "  -d, --debug       Debug init (load minimal config, show errors)"
            echo "  -q, --quick       Skip user config entirely (-Q flag)"
            echo "  -h, --help        Show this help"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Find Emacs - check PATH first, then common macOS locations
if command -v emacs &> /dev/null; then
    EMACS="emacs"
elif [ -x "/Applications/Emacs.app/Contents/MacOS/Emacs" ]; then
    EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
elif [ -x "/Applications/Emacs.app/Contents/MacOS/emacs" ]; then
    EMACS="/Applications/Emacs.app/Contents/MacOS/emacs"
elif [ -x "$HOME/Applications/Emacs.app/Contents/MacOS/Emacs" ]; then
    EMACS="$HOME/Applications/Emacs.app/Contents/MacOS/Emacs"
elif [ -x "/opt/homebrew/bin/emacs" ]; then
    EMACS="/opt/homebrew/bin/emacs"
elif [ -x "/usr/local/bin/emacs" ]; then
    EMACS="/usr/local/bin/emacs"
else
    echo "Error: Emacs not found. Please install Emacs or set EMACS environment variable."
    exit 1
fi

echo "Using Emacs: $EMACS"
echo "Loading SDK from: $SCRIPT_DIR"

# Build Emacs arguments
EMACS_ARGS=()

# Terminal mode
if $TERMINAL_MODE; then
    EMACS_ARGS+=("-nw")
    echo "Mode: terminal"
else
    echo "Mode: GUI"
fi

# Quick mode (skip user config)
if $QUICK_MODE; then
    EMACS_ARGS+=("-Q")
    echo "Config: none (-Q)"
elif $DEBUG_INIT; then
    EMACS_ARGS+=("--debug-init")
    echo "Config: debug init"
else
    echo "Config: normal"
fi

# Add SDK load path and startup
EMACS_ARGS+=(
    --eval "(add-to-list 'load-path \"$SCRIPT_DIR\")"
    --eval "(require 'claude-cli-chat)"
    --eval "(claude-cli-chat)"
)

# Run Emacs
if $TERMINAL_MODE; then
    # Terminal mode runs in foreground
    exec "$EMACS" "${EMACS_ARGS[@]}"
else
    # GUI mode runs in background
    "$EMACS" "${EMACS_ARGS[@]}" &
    echo "Emacs launched (PID: $!)"
fi
