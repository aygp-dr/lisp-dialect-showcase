#!/bin/sh
# Run Scheme examples using Guile

# Create output directory if it doesn't exist
mkdir -p output

# Run standard algorithm examples
echo "==== Running Standard Algorithm Examples ===="
guile src/scheme/fibonacci.scm
guile src/scheme/factorial.scm
guile src/scheme/primes.scm
guile src/scheme/fizzbuzz.scm
guile src/scheme/quicksort.scm

# Run advanced examples
echo "\n==== Running Advanced Examples ===="
echo "\n--- Quantum Computing Simulator ---"
guile src/scheme/quantum-simulator.scm
echo "\n--- Stack-Based Computer Simulator ---"
guile src/scheme/stack-machine.scm
