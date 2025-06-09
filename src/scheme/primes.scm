;; primes


;; [[file:../../showcase-scheme.org::*primes][primes:1]]
;; Helper function: square root
(define (square x) (* x x))

;; Check if a number is prime
(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (and (> n 1) (= n (smallest-divisor n))))

;; Generate primes up to n
(define (primes-up-to n)
  (define (iter i result)
    (cond ((> i n) (reverse result))
          ((prime? i) (iter (+ i 1) (cons i result)))
          (else (iter (+ i 1) result))))
  (iter 2 '()))

;; Sieve of Eratosthenes (using lists)
(define (sieve-of-eratosthenes n)
  (define (sieve numbers)
    (if (null? numbers)
        '()
        (let ((p (car numbers)))
          (cons p (sieve (filter
                         (lambda (x) (not (= 0 (remainder x p))))
                         (cdr numbers)))))))
  (sieve (range 2 n)))

;; Helper: generate a range of numbers
(define (range start end)
  (if (> start end)
      '()
      (cons start (range (+ start 1) end))))

;; Example usage
(define (run-examples)
  (display "Primes up to 20: ")
  (display (primes-up-to 20))
  (newline)
  (display "Primes up to 20 (sieve): ")
  (display (sieve-of-eratosthenes 21))
  (newline)
  (display "Is 17 prime? ")
  (display (prime? 17))
  (newline)
  (display "Is 15 prime? ")
  (display (prime? 15))
  (newline))

(run-examples)
;; primes:1 ends here
