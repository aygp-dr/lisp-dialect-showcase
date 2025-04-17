#lang racket

;; Check if a number is prime
(define (prime? n)
  (and (> n 1)
       (let loop ([i 2])
         (or (> (sqr i) n)
             (and (not (zero? (remainder n i)))
                  (loop (add1 i)))))))

;; Generate primes up to n
(define (primes-up-to n)
  (for/list ([i (in-range 2 (add1 n))]
             #:when (prime? i))
    i))

;; Sieve of Eratosthenes
(define (sieve-of-eratosthenes n)
  (define sieve (make-vector (add1 n) #t))
  (vector-set! sieve 0 #f)
  (vector-set! sieve 1 #f)
  
  (for ([i (in-range 2 (add1 (exact-floor (sqrt n))))]
        #:when (vector-ref sieve i))
    (for ([j (in-range (sqr i) (add1 n) i)])
      (vector-set! sieve j #f)))
  
  (for/list ([i (in-range 2 (add1 n))]
             #:when (vector-ref sieve i))
    i))

;; Example usage
(define (run-examples)
  (displayln (format "Primes up to 20: ~a" (primes-up-to 20)))
  (displayln (format "Primes up to 20 (sieve): ~a" (sieve-of-eratosthenes 20)))
  (displayln (format "Is 17 prime? ~a" (prime? 17)))
  (displayln (format "Is 15 prime? ~a" (prime? 15))))

(module+ main
  (run-examples))
