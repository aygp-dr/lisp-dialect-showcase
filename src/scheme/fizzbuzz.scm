;; Helper: display FizzBuzz for a number
(define (fizzbuzz-number n)
  (cond ((= (remainder n 15) 0) "FizzBuzz")
        ((= (remainder n 3) 0) "Fizz")
        ((= (remainder n 5) 0) "Buzz")
        (else n)))

;; Print FizzBuzz sequence
(define (fizzbuzz n)
  (define (iter i)
    (when (<= i n)
      (display (fizzbuzz-number i))
      (newline)
      (iter (+ i 1))))
  (iter 1))

;; Create a list of FizzBuzz values
(define (fizzbuzz-list n)
  (define (iter i result)
    (if (> i n)
        (reverse result)
        (iter (+ i 1) (cons (fizzbuzz-number i) result))))
  (iter 1 '()))

;; Example usage
(define (run-examples)
  (display "FizzBuzz (1-20):\n")
  (fizzbuzz 20)
  (display "\nFizzBuzz as list (1-20): ")
  (display (fizzbuzz-list 20))
  (newline))

(run-examples)
