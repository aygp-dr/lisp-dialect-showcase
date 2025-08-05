#!/bin/sh
clj -M -e "(load-file \"src/clojure/fibonacci.clj\") (clojure.fibonacci/-main)"
clj -M -e "(load-file \"src/clojure/factorial.clj\") (clojure.factorial/-main)"
clj -M -e "(load-file \"src/clojure/primes.clj\") (clojure.primes/-main)"
clj -M -e "(load-file \"src/clojure/fizzbuzz.clj\") (clojure.fizzbuzz/-main)"
clj -M -e "(load-file \"src/clojure/quicksort.clj\") (clojure.quicksort/-main)"
