#lang racket

;; Quicksort implementation using filter
(define (quicksort lst)
  (if (or (empty? lst) (empty? (rest lst)))
      lst
      (let ([pivot (first lst)]
            [rest (rest lst)])
        (append
         (quicksort (filter (λ (x) (< x pivot)) rest))
         (list pivot)
         (quicksort (filter (λ (x) (>= x pivot)) rest))))))

;; Quicksort using Racket's pattern matching
(define (quicksort-match lst)
  (match lst
    [(list) '()]
    [(list x) (list x)]
    [(cons pivot rest)
     (append
      (quicksort-match (filter (λ (x) (< x pivot)) rest))
      (list pivot)
      (quicksort-match (filter (λ (x) (>= x pivot)) rest)))]))

;; Quicksort using for/list
(define (quicksort-for lst)
  (if (or (empty? lst) (empty? (rest lst)))
      lst
      (let ([pivot (first lst)]
            [rest (rest lst)])
        (append
         (quicksort-for
          (for/list ([x rest] #:when (< x pivot)) x))
         (list pivot)
         (quicksort-for
          (for/list ([x rest] #:when (>= x pivot)) x))))))

;; Example usage
(define (run-examples)
  (let ([numbers '(3 1 4 1 5 9 2 6 5 3 5)])
    (displayln (format "Original list: ~a" numbers))
    (displayln (format "Quicksort: ~a" (quicksort numbers)))
    (displayln (format "Quicksort with match: ~a" (quicksort-match numbers)))
    (displayln (format "Quicksort with for/list: ~a" (quicksort-for numbers)))))

(module+ main
  (run-examples))
