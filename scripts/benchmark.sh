#!/bin/sh
# Benchmark the Fibonacci implementations

echo "Benchmarking Fibonacci (n=30):"
echo "=============================="

# Common Lisp (SBCL)
echo "Common Lisp (SBCL):"
time sbcl --noinform --eval "(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))" \
          --eval "(time (fib 30))" --eval "(quit)" 2>&1 | grep "Evaluation took"

# Clojure
echo "Clojure:"
time clojure -e "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (time (fib 30))"

# Scheme (Guile)
echo "Scheme (Guile):"
time guile -c "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)"

# Emacs Lisp
echo "Emacs Lisp:"
time emacs --batch --eval "(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)"

# Racket
echo "Racket:"
time racket -e "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)"

# Hy
echo "Hy:"
time hy -c "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)"

# Fennel
echo "Fennel:"
time fennel -e "(fn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)"

# Janet
echo "Janet:"
time janet -e "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)"