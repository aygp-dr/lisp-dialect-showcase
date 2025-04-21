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
printf "\n==== Running Advanced Examples ====\n"
printf "\n--- Quantum Computing Simulator ---\n"
guile src/scheme/quantum-simulator.scm
printf "\n--- Stack-Based Computer Simulator ---\n"
guile src/scheme/stack-machine.scm
