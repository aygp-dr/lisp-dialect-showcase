;; primes


;; [[file:../../showcase-hy.org::*primes][primes:1]]
#!/usr/bin/env hy

;; Check if a number is prime
(defn prime? [n]
  (when (> n 1)
    (setv limit (int (math.sqrt n)))
    (for [i (range 2 (+ limit 1))]
      (when (= (% n i) 0)
        (return False)))
    True))

;; Generate primes up to n
(defn primes-up-to [n]
  (lfor i (range 2 (+ n 1)) :if (prime? i) i))

;; Sieve of Eratosthenes
(defn sieve-of-eratosthenes [n]
  (setv sieve (* [True] (+ n 1)))
  (assoc sieve 0 False)
  (assoc sieve 1 False)
  
  (for [i (range 2 (+ (int (math.sqrt n)) 1))]
    (when (get sieve i)
      (for [j (range (* i i) (+ n 1) i)]
        (assoc sieve j False))))
  
  (lfor i (range 2 (+ n 1)) :if (get sieve i) i))

;; Example usage
(import math)

(defn run-examples []
  (print f"Primes up to 20: {(primes-up-to 20)}")
  (print f"Primes up to 20 (sieve): {(sieve-of-eratosthenes 20)}")
  (print f"Is 17 prime? {(prime? 17)}")
  (print f"Is 15 prime? {(prime? 15)}")
  )

(when (= __name__ "__main__")
  (run-examples))
;; primes:1 ends here
