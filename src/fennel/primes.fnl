;; primes


;; [[file:../../showcase-fennel.org::*primes][primes:1]]
;; Check if a number is prime
(fn is-prime? [n]
  (when (> n 1)
    (var prime? true)
    (let [limit (math.floor (math.sqrt n))]
      (for [i 2 limit 1]
        (when (= (% n i) 0)
          (set prime? false)
          (lua "break")))
      prime?)))

;; Generate primes up to n
(fn primes-up-to [n]
  (local result [])
  (for [i 2 n]
    (when (is-prime? i)
      (table.insert result i)))
  result)

;; Sieve of Eratosthenes
(fn sieve-of-eratosthenes [n]
  (local sieve [])
  ;; Initialize sieve
  (for [i 0 n]
    (tset sieve i true))
  
  ;; 0 and 1 are not prime
  (tset sieve 0 false)
  (tset sieve 1 false)
  
  ;; Mark multiples as non-prime
  (let [limit (math.floor (math.sqrt n))]
    (for [i 2 limit]
      (when (. sieve i)
        (var j (* i i))
        (while (<= j n)
          (tset sieve j false)
          (set j (+ j i))))))
  
  ;; Collect primes
  (local primes [])
  (for [i 2 n]
    (when (. sieve i)
      (table.insert primes i)))
  
  primes)

;; Example usage
(fn run-examples []
  (print "Primes up to 20:")
  (print (table.concat (primes-up-to 20) ", "))
  (print "\nPrimes up to 20 (sieve):")
  (print (table.concat (sieve-of-eratosthenes 20) ", "))
  (print (.. "Is 17 prime? " (if (is-prime? 17) "Yes" "No")))
  (print (.. "Is 15 prime? " (if (is-prime? 15) "Yes" "No"))))

(run-examples)
;; primes:1 ends here
