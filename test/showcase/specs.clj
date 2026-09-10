(ns showcase.specs
  "clojure.spec for the Clojure showcase examples (https://clojure.org/guides/spec).

  src/clojure/*.clj is tangled from showcase-clojure.org, so the s/fdefs
  can't sit next to their defns. They live here with the data specs. This
  file is under test/ because `make clean` deletes every file under src/."
  (:require [clojure.factorial :as factorial]
            [clojure.fibonacci :as fibonacci]
            [clojure.fizzbuzz :as fizzbuzz]
            [clojure.primes :as primes]
            [clojure.quicksort :as quicksort]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

;; ---------------------------------------------------------------------------
;; Data specs

;; n! fits in a long for n <= 20, and (factorial-* 21) throws
;; ArithmeticException. Every n <= 1 gives 1.
(s/def ::factorial-n
  (s/with-gen (s/and int? #(<= % 20))
    #(gen/choose -3 20)))

;; F(n) fits in a long for 0 <= n <= 92. A negative n never terminates
;; (fib-recursive, fib-tail) or throws (fib-lazy), so it's outside the domain.
(s/def ::fib-n
  (s/with-gen (s/int-in 0 93)
    #(gen/frequency [[4 (gen/choose 0 92)] [1 (gen/elements [0 1 2 91 92])]])))

;; The upper bound n of fizzbuzz-* and primes-up-to, i.e. (range 1 (inc n)).
;; n <= 0 gives an empty result. The generator stays small.
(s/def ::upper-bound
  (s/with-gen (s/and int? #(< % Long/MAX_VALUE))
    #(gen/choose -3 300)))

;; sieve-of-eratosthenes coerces n with (int n) and allocates n+1 booleans.
(s/def ::sieve-n
  (s/with-gen (s/int-in Integer/MIN_VALUE Integer/MAX_VALUE)
    #(gen/choose -3 3000)))

(s/def ::fizzbuzz-word #{"Fizz" "Buzz" "FizzBuzz"})
;; nonconforming, so an :fn spec sees the items themselves, not s/or tags
(s/def ::fizzbuzz-item (s/nonconforming (s/or :word ::fizzbuzz-word :number pos-int?)))
(s/def ::fizzbuzz (s/coll-of ::fizzbuzz-item :kind sequential?))

(defn probable-prime?
  "Primality from java.math.BigInteger, independent of clojure.primes."
  [n]
  (and (> n 1) (.isProbablePrime (biginteger n) 50)))

(s/def ::prime
  (s/with-gen (s/and int? probable-prime?)
    #(gen/elements [2 3 5 7 11 13 7919 104729 2147483647])))
(s/def ::primes (s/coll-of ::prime :kind sequential?))

;; prime? accepts any long. It's O(sqrt n), so the generator mixes small
;; numbers with a few large primes and composites.
(s/def ::prime-candidate
  (s/with-gen int?
    #(gen/one-of [(gen/choose -10 10000)
                  (gen/elements [2147483647 2147483649 4294967291 1000000007
                                 1000000008 999999999989])])))

;; quicksort compares with < and >=, so its input needs a total order. NaN
;; has none: every comparison with it is false, and quicksort drops it. NaN
;; is therefore outside the domain.
(s/def ::sortable-number
  (s/with-gen (s/and number? #(not (and (double? %) (Double/isNaN %))))
    #(gen/frequency [[6 (gen/choose -10 10)]
                     [2 (gen/large-integer)]
                     [2 (gen/double* {:NaN? false})]
                     [1 (gen/ratio)]])))
(s/def ::numbers (s/coll-of ::sortable-number :kind sequential? :gen-max 30))

(defn- fits-long? [x]
  (<= Long/MIN_VALUE x Long/MAX_VALUE))

;; ---------------------------------------------------------------------------
;; clojure.factorial

(defn- factorial-law
  "f(n) = 1 for n <= 1, else f(n) = n * f(n - 1): the defining recurrence,
  checked against f itself."
  [f n ret]
  (if (<= n 1) (= 1 ret) (= ret (* n (f (dec n))))))

(defn- factorial'
  "n! in arbitrary precision (1 for n <= 1), for domain preconditions."
  [n]
  (apply *' (range 1 (inc n))))

(s/fdef factorial/factorial-recursive
  :args (s/cat :n ::factorial-n)
  :ret pos-int?
  :fn (fn [{{:keys [n]} :args ret :ret}]
        (factorial-law factorial/factorial-recursive n ret)))

(s/fdef factorial/factorial-tail
  :args (s/alt :n (s/cat :n ::factorial-n)
               ;; The accumulator arity, which the unary arity calls with
               ;; acc 1. acc * n! must fit in a long.
               :n+acc (s/& (s/cat :n ::factorial-n
                                  :acc (s/with-gen pos-int? #(gen/choose 1 3)))
                           (fn [{:keys [n acc]}] (fits-long? (*' acc (factorial' n))))))
  :ret pos-int?
  :fn (fn [{[arity {:keys [n acc]}] :args ret :ret}]
        (case arity
          :n     (factorial-law factorial/factorial-tail n ret)
          :n+acc (= ret (* acc (factorial/factorial-tail n))))))

(s/fdef factorial/factorial-reduce
  :args (s/cat :n ::factorial-n)
  :ret pos-int?
  :fn (fn [{{:keys [n]} :args ret :ret}]
        (factorial-law factorial/factorial-reduce n ret)))

(s/fdef factorial/run-examples :args (s/cat))
(s/fdef factorial/-main :args (s/cat))

;; ---------------------------------------------------------------------------
;; clojure.fibonacci

(defn- fibonacci-law
  "F(0) = 0, F(1) = F(2) = 1, and Cassini's identity
  F(n)F(n-2) - F(n-1)^2 = (-1)^(n-1) for n > 2, evaluated with f itself in
  arbitrary precision. Together these pin down the Fibonacci numbers without
  restating the recurrence that the implementations use."
  [f n ret]
  (case n
    0 (= 0 ret)
    1 (= 1 ret)
    2 (= 1 ret)
    (= (-' (*' ret (f (- n 2))) (*' (f (dec n)) (f (dec n))))
       (if (even? n) -1 1))))

(defn- general-fib
  "G(n) for G(0) = a, G(1) = b, G(k) = G(k-1) + G(k-2), i.e. a*F(n-1) + b*F(n),
  computed iteratively in arbitrary precision."
  [n a b]
  (first (nth (iterate (fn [[x y]] [y (+' x y)]) [a b]) n)))

(s/fdef fibonacci/fib-recursive
  ;; exponential time, so the generator stays small
  :args (s/cat :n (s/with-gen ::fib-n #(gen/choose 0 20)))
  :ret nat-int?
  :fn (fn [{{:keys [n]} :args ret :ret}]
        (fibonacci-law fibonacci/fib-recursive n ret)))

(s/fdef fibonacci/fib-lazy
  :args (s/cat :n ::fib-n)
  :ret nat-int?
  :fn (fn [{{:keys [n]} :args ret :ret}]
        (fibonacci-law fibonacci/fib-lazy n ret)))

(s/fdef fibonacci/fib-tail
  :args (s/alt :n (s/cat :n ::fib-n)
               ;; The general recurrence G(0) = a, G(1) = b, which the unary
               ;; arity calls with a 0 and b 1. The answer G(n) must fit in a
               ;; long.
               :n+a+b (s/& (s/cat :n ::fib-n
                                  :a (s/with-gen int? #(gen/choose 0 999))
                                  :b (s/with-gen int? #(gen/choose 0 999)))
                           (fn [{:keys [n a b]}] (fits-long? (general-fib n a b)))))
  :ret int?
  :fn (fn [{[arity {:keys [n a b]}] :args ret :ret}]
        (case arity
          :n     (fibonacci-law fibonacci/fib-tail n ret)
          :n+a+b (= ret (general-fib n a b)))))

(s/fdef fibonacci/run-examples :args (s/cat))
(s/fdef fibonacci/-main :args (s/cat))

;; ---------------------------------------------------------------------------
;; clojure.fizzbuzz

(defn- fizzbuzz-law
  "One item for each i in 1..n: FizzBuzz when 15 divides i, Fizz when only 3
  does, Buzz when only 5 does, and i itself otherwise."
  [{{:keys [n]} :args ret :ret}]
  (and (= (count ret) (max n 0))
       (every? (fn [[i item]]
                 (let [by3 (zero? (mod i 3))
                       by5 (zero? (mod i 5))]
                   (= item (cond (and by3 by5) "FizzBuzz" by3 "Fizz" by5 "Buzz" :else i))))
               (map vector (iterate inc 1) ret))))

(s/fdef fizzbuzz/fizzbuzz-cond
  :args (s/cat :n ::upper-bound)
  :ret ::fizzbuzz
  :fn fizzbuzz-law)

(s/fdef fizzbuzz/fizzbuzz-match
  :args (s/cat :n ::upper-bound)
  :ret ::fizzbuzz
  :fn fizzbuzz-law)

(s/fdef fizzbuzz/fizzbuzz-case
  :args (s/cat :n ::upper-bound)
  :ret ::fizzbuzz
  :fn fizzbuzz-law)

(s/fdef fizzbuzz/run-examples :args (s/cat))
(s/fdef fizzbuzz/-main :args (s/cat))

;; ---------------------------------------------------------------------------
;; clojure.primes

(s/fdef primes/prime?
  :args (s/cat :n ::prime-candidate)
  :ret boolean?
  :fn (fn [{{:keys [n]} :args ret :ret}]
        (= ret (probable-prime? n))))

(defn- exactly-the-primes-through-n?
  "Every item is prime (checked with BigInteger, not clojure.primes), and
  together they are all the primes <= n, ascending."
  [{{:keys [n]} :args ret :ret}]
  (and (every? probable-prime? ret)
       (= (seq ret) (seq (filter probable-prime? (range 2 (inc n)))))))

(s/fdef primes/sieve-of-eratosthenes
  :args (s/cat :n ::sieve-n)
  :ret ::primes
  :fn exactly-the-primes-through-n?)

(s/fdef primes/primes-up-to
  :args (s/cat :n ::upper-bound)
  :ret ::primes
  :fn exactly-the-primes-through-n?)

(s/fdef primes/run-examples :args (s/cat))
(s/fdef primes/-main :args (s/cat))

;; ---------------------------------------------------------------------------
;; clojure.quicksort

(defn- sorted-permutation?
  "ret holds exactly the elements of coll, in non-decreasing order."
  [{{:keys [coll]} :args ret :ret}]
  (and (= (frequencies coll) (frequencies ret))
       (or (empty? ret) (apply <= ret))))

(s/fdef quicksort/quicksort
  :args (s/cat :coll ::numbers)
  :ret (s/coll-of ::sortable-number :kind sequential?)
  :fn sorted-permutation?)

(s/fdef quicksort/quicksort-partition
  :args (s/cat :coll ::numbers)
  :ret (s/coll-of ::sortable-number :kind sequential?)
  :fn sorted-permutation?)

(s/fdef quicksort/run-examples :args (s/cat))
(s/fdef quicksort/-main :args (s/cat))
