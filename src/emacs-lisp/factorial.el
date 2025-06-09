;; factorial


;; [[file:../../showcase-emacs-lisp.org::*factorial][factorial:1]]
;;; factorial.el --- Factorial implementations in Emacs Lisp

;;; Commentary:
;; Various implementations of factorial calculation in Emacs Lisp

;;; Code:

;; Recursive implementation
(defun factorial-recursive (n)
  "Calculate factorial of N recursively."
  (if (<= n 1)
      1
    (* n (factorial-recursive (- n 1)))))

;; Iterative implementation
(defun factorial-iterative (n)
  "Calculate factorial of N iteratively."
  (let ((result 1))
    (dotimes (i n result)
      (setq result (* result (1+ i))))))

;; Tail-recursive implementation
(defun factorial-tail (n &optional (acc 1))
  "Calculate factorial of N using tail recursion with accumulator ACC."
  (if (<= n 1)
      acc
    (factorial-tail (1- n) (* acc n))))

;; Example usage
(defun run-factorial-examples ()
  "Run examples of various factorial implementations."
  (message "Factorial of 5 (recursive): %d" (factorial-recursive 5))
  (message "Factorial of 5 (iterative): %d" (factorial-iterative 5))
  (message "Factorial of 5 (tail-recursive): %d" (factorial-tail 5)))

(provide 'factorial)
;;; factorial.el ends here
;; factorial:1 ends here
