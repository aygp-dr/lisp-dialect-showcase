;; fizzbuzz


;; [[file:../../showcase-racket.org::*fizzbuzz][fizzbuzz:1]]
#lang racket

;; FizzBuzz using cond
(define (fizzbuzz n)
  (for ([i (in-range 1 (add1 n))])
    (displayln
     (cond
       [(zero? (remainder i 15)) "FizzBuzz"]
       [(zero? (remainder i 3)) "Fizz"]
       [(zero? (remainder i 5)) "Buzz"]
       [else i]))))

;; FizzBuzz as a list using for/list
(define (fizzbuzz-list n)
  (for/list ([i (in-range 1 (add1 n))])
    (cond
      [(zero? (remainder i 15)) "FizzBuzz"]
      [(zero? (remainder i 3)) "Fizz"]
      [(zero? (remainder i 5)) "Buzz"]
      [else i])))

;; FizzBuzz using match
(define (fizzbuzz-match n)
  (for/list ([i (in-range 1 (add1 n))])
    (match (list (zero? (remainder i 3))
                (zero? (remainder i 5)))
      [(list #t #t) "FizzBuzz"]
      [(list #t #f) "Fizz"]
      [(list #f #t) "Buzz"]
      [_ i])))

;; Example usage
(define (run-examples)
  (displayln "FizzBuzz (1-20):")
  (fizzbuzz 20)
  (displayln (format "\nFizzBuzz as list (1-20): ~a" (fizzbuzz-list 20))))

(module+ main
  (run-examples))
;; fizzbuzz:1 ends here
