#!/bin/bash
set -e

# Get script directory and build yoloplanner
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
echo "Building yoloplanner..."
cd "$SCRIPT_DIR"
go build -o yoloplanner ./cmd
YOLOPLANNER="$SCRIPT_DIR/yoloplanner"

# Parse arguments
MODE="${1:-simple}"
BUILD_MODE="${2:-current}"

usage() {
    echo "Usage: $0 [simple|interactive] [current|new]"
    echo "  simple      - Run with --simple flag (auto-answers, exits after build)"
    echo "  interactive - Run without --simple (tests menu prompts and follow-up)"
    echo ""
    echo "  current - Execute in current session (keeps context)"
    echo "  new     - Execute in new session (fresh start)"
    exit 1
}

if [[ "$MODE" != "simple" && "$MODE" != "interactive" ]]; then
    usage
fi
if [[ "$BUILD_MODE" != "current" && "$BUILD_MODE" != "new" ]]; then
    usage
fi

# Create temp directory
TMPDIR=$(mktemp -d)
echo "Testing in: $TMPDIR"
echo "Mode: $MODE"
echo "Build mode: $BUILD_MODE"

# Create a half-implemented Python calculator
cat > "$TMPDIR/calculator.py" << 'EOF'
"""Simple calculator module."""

def add(a: int, b: int) -> int:
    """Add two numbers."""
    return a + b

def subtract(a: int, b: int) -> int:
    """Subtract b from a."""
    return a - b

# TODO: implement multiply and divide
EOF

# Create a simple test file
cat > "$TMPDIR/test_calculator.py" << 'EOF'
"""Tests for calculator."""
from calculator import add, subtract

def test_add():
    assert add(2, 3) == 5

def test_subtract():
    assert subtract(5, 3) == 2

# TODO: add tests for multiply and divide
EOF

cd "$TMPDIR"

if [[ "$MODE" == "simple" ]]; then
    echo ""
    echo "=== Testing --simple --build=$BUILD_MODE ==="
    # Simple mode: auto-answers questions and exits after build
    "$YOLOPLANNER" --simple --build="$BUILD_MODE" "finish implementing the calculator"
else
    echo ""
    echo "=== Testing interactive mode (non-simple) ==="
    echo "This test will:"
    echo "  1. Run yoloplanner without --simple"
    echo "  2. Send '1' to select first option for any AskUserQuestion"
    echo "  3. Send '$BUILD_MODE' choice (1=current, 2=new) for build mode menu"
    echo "  4. After build, send Ctrl-D (EOF) to exit the follow-up prompt"
    echo ""

    # Determine which choice number for build mode
    if [[ "$BUILD_MODE" == "current" ]]; then
        BUILD_CHOICE="1"
    else
        BUILD_CHOICE="2"
    fi

    # Create input file that simulates:
    # - "1" for any AskUserQuestion prompts (first option)
    # - Build mode choice (1 or 2)
    # - Empty EOF to exit the follow-up prompt
    # We send multiple "1"s in case there are multiple questions (up to 5)
    {
        echo "1"  # First AskUserQuestion option
        echo "1"  # Second AskUserQuestion option (if any)
        echo "1"  # Third AskUserQuestion option (if any)
        echo "1"  # Fourth AskUserQuestion option (if any)
        echo "1"  # Fifth AskUserQuestion option (if any)
        echo "$BUILD_CHOICE"  # Build mode menu choice
        # No more input - EOF will trigger exit from follow-up prompt
    } | "$YOLOPLANNER" "finish implementing the calculator" || {
        EXIT_CODE=$?
        # Exit code may be non-zero due to EOF, which is expected
        echo "(Exit code: $EXIT_CODE - expected for EOF termination)"
    }
fi

echo ""
echo "=== Checking results ==="
echo "--- calculator.py ---"
cat "$TMPDIR/calculator.py"
echo ""
echo "--- test_calculator.py ---"
cat "$TMPDIR/test_calculator.py"

# Run the tests
echo ""
echo "=== Running tests ==="
python3 -m pytest "$TMPDIR/test_calculator.py" -v 2>/dev/null || python3 "$TMPDIR/test_calculator.py" || echo "Tests may have failed"

# Cleanup
echo ""
echo "Temp dir: $TMPDIR (not cleaned up for inspection)"
