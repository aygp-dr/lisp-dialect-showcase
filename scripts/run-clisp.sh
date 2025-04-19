#!/bin/sh
# Run CLISP examples

# Create output directory if it doesn't exist
mkdir -p output

echo "==== GNU CLISP Examples ===="

# Run Fibonacci example
echo "Running Fibonacci example..."
clisp -q src/clisp/fibonacci.lisp

# Run Factorial example
echo "Running Factorial example..."
clisp -q src/clisp/factorial.lisp

# Run Primes example
echo "Running Primes example..."
clisp -q src/clisp/primes.lisp

# Run FizzBuzz example
echo "Running FizzBuzz example..."
clisp -q src/clisp/fizzbuzz.lisp

# Run Quicksort example
echo "Running Quicksort example..."
clisp -q src/clisp/quicksort.lisp

echo "==== GNU CLISP Examples Complete ===="