;; fibonacci


;; [[file:../../showcase-racket.org::*fibonacci][fibonacci:1]]
#lang racket

;; Recursive implementation
(define (fib-recursive n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib-recursive (- n 1))
                 (fib-recursive (- n 2)))]))

;; Simple manual memoization
(define memo-table (make-hash))

(define (fib-memo n)
  (cond [(hash-has-key? memo-table n) (hash-ref memo-table n)]
        [(= n 0) (hash-set! memo-table n 0) 0]
        [(= n 1) (hash-set! memo-table n 1) 1]
        [else (let ([result (+ (fib-memo (- n 1))
                              (fib-memo (- n 2)))])
                (hash-set! memo-table n result)
                result)]))

;; Contract-based implementation
(provide/contract
 [fibonacci (-> exact-nonnegative-integer?
                exact-nonnegative-integer?)])

(define (fibonacci n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2)))]))

;; Example usage
(define (run-examples)
  (displayln (format "Fibonacci of 10 (recursive): ~a" (fib-recursive 10)))
  (displayln (format "Fibonacci of 10 (memoized): ~a" (fib-memo 10)))
  (displayln (format "Fibonacci of 10 (contract): ~a" (fibonacci 10))))

(module+ main
  (run-examples))
;; fibonacci:1 ends here
