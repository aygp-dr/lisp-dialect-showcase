(ns showcase.specs-test
  "Generative checks for every pure s/fdef'd fn, plus data-spec sanity.
  Per https://clojure.org/guides/spec (Testing)."
  (:require [clojure.fibonacci :as fibonacci]
            [clojure.fizzbuzz :as fizzbuzz]
            [clojure.primes :as primes]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing]]
            [showcase.specs :as specs]))

(def ^:private check-opts {:clojure.spec.test.check/opts {:num-tests 50}})

(def ^:private example-nses
  '[clojure.factorial clojure.fibonacci clojure.fizzbuzz clojure.primes clojure.quicksort])

;; Side-effecting fns: fdef'd for instrumentation, never generatively checked.
;; run-examples and -main print.
(def ^:private side-effecting
  (set (for [ns example-nses, f '[run-examples -main]]
         (symbol (str ns) (str f)))))

;; TODO(spec): (fib-tail 92) throws ArithmeticException (long overflow),
;; although F(92) fits in a long, as (fib-lazy 92) shows. The loop computes
;; F(93) one step past the answer. Excluded until the org source is fixed;
;; examples_test checks fib-tail for n <= 91.
(def ^:private known-failures
  #{`fibonacci/fib-tail})

(defn- checkable []
  (->> example-nses
       (mapcat stest/enumerate-namespace)
       (remove side-effecting)
       (remove known-failures)))

(deftest fdefs-hold-under-generative-testing
  (let [results (stest/check (checkable) check-opts)]
    (is (= 13 (count results)) (pr-str (sort (map :sym results))))
    (doseq [r results]
      (testing (str (:sym r))
        (is (nil? (:failure r))
            (pr-str (stest/abbrev-result r)))))))

(deftest data-specs-generate-and-conform
  (doseq [k [::specs/factorial-n ::specs/fib-n ::specs/upper-bound ::specs/sieve-n
             ::specs/fizzbuzz ::specs/prime ::specs/primes ::specs/prime-candidate
             ::specs/numbers]]
    (testing (str k)
      (is (every? (fn [[v _]] (s/valid? k v)) (s/exercise k 10))))))

(deftest real-values-conform
  (testing "the values run-examples uses"
    (is (s/valid? ::specs/numbers [3 1 4 1 5 9 2 6 5 3 5]))
    (is (s/valid? ::specs/primes (primes/primes-up-to 20)))
    (is (s/valid? ::specs/fizzbuzz (fizzbuzz/fizzbuzz-cond 20))))
  (testing "fibs holds F(0)..F(92) as longs"
    (is (s/valid? (s/coll-of nat-int?) (take 93 fibonacci/fibs))))
  (testing "NaN is outside the quicksort domain"
    (is (not (s/valid? ::specs/numbers [1 ##NaN 0])))))
