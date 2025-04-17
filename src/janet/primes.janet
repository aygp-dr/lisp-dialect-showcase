# Check if a number is prime
(defn prime? [n]
  (when (> n 1)
    (var is-prime true)
    (def limit (math/sqrt n))
    (for i 2 (+ (math/floor limit) 1)
      (when (= (% n i) 0)
        (set is-prime false)
        (break)))
    is-prime))

# Generate primes up to n
(defn primes-up-to [n]
  (def result @[])
  (for i 2 (+ n 1)
    (when (prime? i)
      (array/push result i)))
  result)

# Sieve of Eratosthenes
(defn sieve-of-eratosthenes [n]
  (def sieve (array/new-filled (+ n 1) true))
  # 0 and 1 are not prime
  (put sieve 0 false)
  (put sieve 1 false)
  
  (def limit (math/sqrt n))
  (for i 2 (+ (math/floor limit) 1)
    (when (get sieve i)
      (var j (* i i))
      (while (<= j n)
        (put sieve j false)
        (set j (+ j i)))))
  
  (def primes @[])
  (for i 2 (+ n 1)
    (when (get sieve i)
      (array/push primes i)))
  
  primes)

# Example usage
(defn run-examples []
  (printf "Primes up to 20: %j" (primes-up-to 20))
  (printf "Primes up to 20 (sieve): %j" (sieve-of-eratosthenes 20))
  (printf "Is 17 prime? %s" (if (prime? 17) "Yes" "No"))
  (printf "Is 15 prime? %s" (if (prime? 15) "Yes" "No")))

(run-examples)
