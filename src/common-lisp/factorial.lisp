;; factorial


;; [[file:../../showcase-common-lisp.org::*factorial][factorial:1]]
(defpackage :cl-factorial
  (:use :cl)
  (:export :factorial-recursive :factorial-iterative :factorial-tail :run-examples))

(in-package :cl-factorial)

;; Recursive implementation
(defun factorial-recursive (n)
  "Calculate factorial recursively."
  (if (<= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

;; Iterative implementation
(defun factorial-iterative (n)
  "Calculate factorial iteratively."
  (let ((result 1))
    (dotimes (i n result)
      (setf result (* result (1+ i))))))

;; Tail-recursive implementation
(defun factorial-tail (n &optional (acc 1))
  "Calculate factorial using tail recursion."
  (if (<= n 1)
      acc
      (factorial-tail (- n 1) (* acc n))))

;; Example usage
(defun run-examples ()
  (format t "Factorial of 5 (recursive): ~a~%" (factorial-recursive 5))
  (format t "Factorial of 5 (iterative): ~a~%" (factorial-iterative 5))
  (format t "Factorial of 5 (tail-recursive): ~a~%" (factorial-tail 5)))
;; factorial:1 ends here
