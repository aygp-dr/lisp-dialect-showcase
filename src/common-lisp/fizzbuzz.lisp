;; fizzbuzz


;; [[file:../../showcase-common-lisp.org::*fizzbuzz][fizzbuzz:1]]
(defpackage :cl-fizzbuzz
  (:use :cl)
  (:export :fizzbuzz :fizzbuzz-list :run-examples))

(in-package :cl-fizzbuzz)

;; FizzBuzz implementation
(defun fizzbuzz (n)
  "Print FizzBuzz from 1 to n."
  (loop for i from 1 to n
        do (format t "~a~%"
                  (cond ((zerop (mod i 15)) "FizzBuzz")
                        ((zerop (mod i 3)) "Fizz")
                        ((zerop (mod i 5)) "Buzz")
                        (t i)))))

;; Functional implementation returning a list
(defun fizzbuzz-list (n)
  "Return a list of FizzBuzz values from 1 to n."
  (loop for i from 1 to n
        collect (cond ((zerop (mod i 15)) "FizzBuzz")
                     ((zerop (mod i 3)) "Fizz")
                     ((zerop (mod i 5)) "Buzz")
                     (t i))))

;; Example usage
(defun run-examples ()
  (format t "FizzBuzz (1-20):~%")
  (fizzbuzz 20)
  (format t "~%FizzBuzz as list (1-20): ~a~%" (fizzbuzz-list 20)))
;; FizzBuzz:1 ends here
;; fizzbuzz:1 ends here
