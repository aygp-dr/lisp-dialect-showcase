;; Recursive implementation
(define (factorial-recursive n)
  (if (<= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

;; Tail-recursive implementation
(define (factorial-tail n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                  (+ counter 1)
                  max-count)))
  (fact-iter 1 1 n))

;; Example usage
(define (run-examples)
  (display "Factorial of 5 (recursive): ")
  (display (factorial-recursive 5))
  (newline)
  (display "Factorial of 5 (tail-recursive): ")
  (display (factorial-tail 5))
  (newline))

(run-examples)
