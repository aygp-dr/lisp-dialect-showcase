#!/bin/sh
# Run the Scheme computational models framework demonstration

SCRIPT_DIR=$(dirname "$0")
ROOT_DIR=$(cd "$SCRIPT_DIR/.." && pwd)

echo "Running Computational Models Framework..."
printf "=======================================\\n"

# Check if guile is installed
if ! command -v guile >/dev/null 2>&1; then
  echo "Error: Guile Scheme is required but not found in PATH."
  echo "Please install Guile Scheme and try again."
  exit 1
fi

# Run all the computational model files
guile -L "$ROOT_DIR/src/scheme" \
  -l "$ROOT_DIR/src/scheme/quantum-simulator.scm" \
  -l "$ROOT_DIR/src/scheme/stack-machine.scm" \
  -l "$ROOT_DIR/src/scheme/turing-machine.scm" \
  -l "$ROOT_DIR/src/scheme/life.scm" \
  -l "$ROOT_DIR/src/scheme/computational-models-guide.scm" \
  -c "(run-computational-models-demo)"

printf "\\nComputational models demonstration complete.\\n"