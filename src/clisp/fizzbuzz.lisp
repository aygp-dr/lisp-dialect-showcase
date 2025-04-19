;; FizzBuzz in GNU CLISP

;; Single FizzBuzz value
(defun fizzbuzz (n)
  "Return FizzBuzz value for N."
  (cond ((zerop (mod n 15)) "FizzBuzz")
        ((zerop (mod n 3)) "Fizz")
        ((zerop (mod n 5)) "Buzz")
        (t n)))

;; FizzBuzz for a range of numbers
(defun fizzbuzz-range (start end)
  "Generate FizzBuzz values for range from START to END."
  (loop for i from start to end
        collect (fizzbuzz i)))

;; Print FizzBuzz
(defun print-fizzbuzz (start end)
  "Print FizzBuzz for range from START to END."
  (loop for i from start to end
        do (format t "~a~%" (fizzbuzz i))))

;; Example usage
(format t "FizzBuzz (1-20):~%")
(print-fizzbuzz 1 20)
(format t "~%FizzBuzz as list (1-20): ~a~%" (fizzbuzz-range 1 20))
