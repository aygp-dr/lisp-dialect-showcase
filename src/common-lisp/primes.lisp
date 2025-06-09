;; primes


;; [[file:../../showcase-common-lisp.org::*primes][primes:1]]
(defpackage :cl-primes
  (:use :cl)
  (:export :primep :sieve-of-eratosthenes :primes-up-to))

(in-package :cl-primes)

;; Function to check if a number is prime
(defun primep (n)
  "Check if n is a prime number."
  (when (> n 1)
    (loop for i from 2 to (isqrt n)
          never (zerop (mod n i)))))

;; Sieve of Eratosthenes implementation
(defun sieve-of-eratosthenes (max)
  "Generate all primes up to max using Sieve of Eratosthenes."
  (let ((sieve (make-array (1+ max) :initial-element t)))
    (setf (aref sieve 0) nil
          (aref sieve 1) nil)
    (loop for i from 2 to (isqrt max)
          when (aref sieve i)
          do (loop for j from (* i i) to max by i
                   do (setf (aref sieve j) nil)))
    (loop for i from 2 to max
          when (aref sieve i)
          collect i)))

;; Function to generate primes up to n
(defun primes-up-to (n)
  "Generate a list of primes up to n."
  (loop for i from 2 to n
        when (primep i)
        collect i))

;; Example usage
(defun run-examples ()
  (format t "Primes up to 20: ~a~%" (primes-up-to 20))
  (format t "Primes up to 20 (sieve): ~a~%" (sieve-of-eratosthenes 20))
  (format t "Is 17 prime? ~a~%" (primep 17))
  (format t "Is 15 prime? ~a~%" (primep 15)))
;; Prime Number Generator:1 ends here
;; primes:1 ends here
