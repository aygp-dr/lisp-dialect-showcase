#!/bin/sh
sbcl --load src/common-lisp/fibonacci.lisp --eval '(cl-fibonacci:run-examples)' --quit
sbcl --load src/common-lisp/factorial.lisp --eval '(cl-factorial:run-examples)' --quit
sbcl --load src/common-lisp/primes.lisp --eval '(cl-primes:run-examples)' --quit
sbcl --load src/common-lisp/fizzbuzz.lisp --eval '(cl-fizzbuzz:run-examples)' --quit
sbcl --load src/common-lisp/quicksort.lisp --eval '(cl-quicksort:run-examples)' --quit
