;; Recursive implementation
(define (fib-recursive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-recursive (- n 1))
                (fib-recursive (- n 2))))))

;; Tail-recursive implementation
(define (fib-tail n)
  (define (fib-iter a b count)
    (if (= count 0)
        a
        (fib-iter b (+ a b) (- count 1))))
  (fib-iter 0 1 n))

;; Continuation-passing style
(define (fib-cps n k)
  (cond ((= n 0) (k 0))
        ((= n 1) (k 1))
        (else (fib-cps (- n 1)
                      (lambda (v1)
                        (fib-cps (- n 2)
                               (lambda (v2)
                                 (k (+ v1 v2)))))))))

(define (fib n)
  (fib-cps n (lambda (x) x)))

;; Example usage
(define (run-examples)
  (display "Fibonacci of 10 (recursive): ")
  (display (fib-recursive 10))
  (newline)
  (display "Fibonacci of 10 (tail-recursive): ")
  (display (fib-tail 10))
  (newline)
  (display "Fibonacci of 10 (CPS): ")
  (display (fib 10))
  (newline))

(run-examples)
