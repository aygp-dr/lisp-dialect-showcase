#!/bin/sh
emacs --batch \
  --load src/emacs-lisp/fibonacci.el \
  --load src/emacs-lisp/factorial.el \
  --load src/emacs-lisp/primes.el \
  --load src/emacs-lisp/fizzbuzz.el \
  --load src/emacs-lisp/quicksort.el \
  --eval "(progn (run-fibonacci-examples) (run-factorial-examples) (run-prime-examples) (run-fizzbuzz-examples) (run-quicksort-examples))"
