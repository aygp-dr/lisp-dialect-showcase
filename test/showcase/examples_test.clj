(ns showcase.examples-test
  "Example-based and cross-implementation tests for the tangled showcase
  examples. They run with instrumentation on."
  (:require [clojure.factorial :as factorial]
            [clojure.fibonacci :as fibonacci]
            [clojure.fizzbuzz :as fizzbuzz]
            [clojure.primes :as primes]
            [clojure.quicksort :as quicksort]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [showcase.specs]))

;; Exercise every s/fdef :args spec while the unit tests run.
(use-fixtures :once
  (fn [f] (stest/instrument) (try (f) (finally (stest/unstrument)))))

(defn- output-lines [main]
  (str/split-lines (with-out-str (main))))

(deftest run-examples-output
  ;; What scripts/run-clojure.sh prints.
  (is (= ["Factorial of 5 (recursive): 120"
          "Factorial of 5 (tail): 120"
          "Factorial of 5 (reduce): 120"]
         (output-lines factorial/-main)))
  (is (= ["Fibonacci of 10 (recursive): 55"
          "Fibonacci of 10 (lazy): 55"
          "Fibonacci of 10 (tail): 55"
          "First 10 Fibonacci numbers: (0 1 1 2 3 5 8 13 21 34)"]
         (output-lines fibonacci/-main)))
  (is (= ["Primes up to 20: (2 3 5 7 11 13 17 19)"
          "Primes up to 20 (sieve): (2 3 5 7 11 13 17 19)"
          "Is 17 prime? true"
          "Is 15 prime? false"]
         (output-lines primes/-main)))
  (let [lines (output-lines fizzbuzz/-main)]
    (is (= "FizzBuzz (1-20) using cond:" (first lines)))
    (is (= ["1" "2" "Fizz" "4" "Buzz"] (subvec lines 1 6)))
    (is (= "(1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz)"
           (peek lines))))
  (is (= ["Original collection: [3 1 4 1 5 9 2 6 5 3 5]"
          "Quicksort: (1 1 2 3 3 4 5 5 5 6 9)"
          "Quicksort with partition: (1 1 2 3 3 4 5 5 5 6 9)"]
         (output-lines quicksort/-main))))

(defn- holds? [property]
  (let [result (tc/quick-check 100 property)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :seed])))))

(deftest implementations-agree
  (testing "factorial"
    (holds? (prop/for-all [n (gen/choose -3 20)]
                          (= (factorial/factorial-recursive n)
                             (factorial/factorial-tail n)
                             (factorial/factorial-reduce n)))))
  (testing "fibonacci"
    (holds? (prop/for-all [n (gen/choose 0 20)]
                          (= (fibonacci/fib-recursive n) (fibonacci/fib-lazy n) (fibonacci/fib-tail n)))))
  (testing "fizzbuzz"
    (holds? (prop/for-all [n (gen/choose -3 100)]
                          (= (fizzbuzz/fizzbuzz-cond n) (fizzbuzz/fizzbuzz-match n) (fizzbuzz/fizzbuzz-case n)))))
  (testing "primes"
    (holds? (prop/for-all [n (gen/choose -3 2000)]
                          (= (primes/sieve-of-eratosthenes n)
                             (primes/primes-up-to n)
                             (filter primes/prime? (range (inc n)))))))
  (testing "quicksort, on integers so that equal elements are indistinguishable"
    (holds? (prop/for-all [xs (gen/vector (gen/choose -10 10))]
                          (= (sort xs) (quicksort/quicksort xs) (quicksort/quicksort-partition xs))))))

(deftest fib-tail-below-the-overflow
  ;; TODO(spec): (fib-tail 92) throws (long overflow) although F(92) fits in
  ;; a long; see specs_test. Up to 91 it matches fib-lazy, and the 3-arity
  ;; follows G(n) = a*F(n-1) + b*F(n).
  (is (= (map fibonacci/fib-lazy (range 92)) (map fibonacci/fib-tail (range 92))))
  (holds? (prop/for-all [n (gen/choose 1 59)
                         a (gen/choose 0 999)
                         b (gen/choose 0 999)]
                        (= (fibonacci/fib-tail n a b)
                           (+ (* a (fibonacci/fib-lazy (dec n))) (* b (fibonacci/fib-lazy n)))))))
