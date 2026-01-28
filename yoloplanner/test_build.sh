#!/bin/bash
set -e

# Get script directory and build yoloplanner
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
echo "Building yoloplanner..."
cd "$SCRIPT_DIR"
go build -o yoloplanner ./cmd
YOLOPLANNER="$SCRIPT_DIR/yoloplanner"

# Parse arguments
BUILD_MODE="${1:-current}"
if [[ "$BUILD_MODE" != "current" && "$BUILD_MODE" != "new" ]]; then
    echo "Usage: $0 [current|new]"
    echo "  current - Execute in current session (keeps context)"
    echo "  new     - Execute in new session (fresh start)"
    exit 1
fi

# Create temp directory
TMPDIR=$(mktemp -d)
echo "Testing in: $TMPDIR"
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

echo ""
echo "=== Testing --simple --build=$BUILD_MODE ==="
cd "$TMPDIR"
# Use a vague prompt - let plan mode research what's needed
"$YOLOPLANNER" --simple --build="$BUILD_MODE" "finish implementing the calculator"

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
