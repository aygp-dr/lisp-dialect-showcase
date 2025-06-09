;; factorial


;; [[file:../../showcase-racket.org::*factorial][factorial:1]]
#lang racket

;; Recursive implementation
(define (factorial-recursive n)
  (if (<= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

;; Tail-recursive implementation
(define (factorial-tail n [acc 1])
  (if (<= n 1)
      acc
      (factorial-tail (- n 1) (* n acc))))

;; Implementation using fold
(define (factorial-fold n)
  (for/fold ([acc 1])
            ([i (in-range 1 (add1 n))])
    (* acc i)))

;; Example usage
(define (run-examples)
  (displayln (format "Factorial of 5 (recursive): ~a" (factorial-recursive 5)))
  (displayln (format "Factorial of 5 (tail): ~a" (factorial-tail 5)))
  (displayln (format "Factorial of 5 (fold): ~a" (factorial-fold 5))))

(module+ main
  (run-examples))
;; factorial:1 ends here
