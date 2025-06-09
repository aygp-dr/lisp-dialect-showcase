;; fibonacci


;; [[file:../../showcase-emacs-lisp.org::*fibonacci][fibonacci:1]]
;;; fibonacci.el --- Fibonacci implementations in Emacs Lisp

;;; Commentary:
;; Various implementations of the Fibonacci sequence in Emacs Lisp

;;; Code:

;; Recursive implementation
(defun fib-recursive (n)
  "Calculate the Nth Fibonacci number recursively."
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib-recursive (- n 1))
              (fib-recursive (- n 2))))))

;; Dynamic programming implementation
(defun fib-dp (n)
  "Calculate the Nth Fibonacci number using dynamic programming."
  (let ((fib-table (make-vector (1+ n) nil)))
    (aset fib-table 0 0)
    (when (> n 0)
      (aset fib-table 1 1)
      (dotimes (i (- n 1))
        (aset fib-table (+ i 2)
              (+ (aref fib-table (+ i 1))
                 (aref fib-table i)))))
    (aref fib-table n)))

;; Memoization using lexical closure
(defun fib-memo-func ()
  "Create a memoized Fibonacci function."
  (let ((memo (make-hash-table)))
    (puthash 0 0 memo)
    (puthash 1 1 memo)
    (lambda (n)
      (or (gethash n memo)
          (puthash n
                   (+ (funcall this (- n 1))
                      (funcall this (- n 2)))
                   memo)))))

(defvar fib-memo (fib-memo-func)
  "Memoized Fibonacci function.")

;; Example usage
(defun run-fibonacci-examples ()
  "Run examples of various Fibonacci implementations."
  (message "Fibonacci of 10 (recursive): %d" (fib-recursive 10))
  (message "Fibonacci of 10 (dynamic programming): %d" (fib-dp 10))
  (message "Fibonacci of 10 (memoized): %d" (funcall fib-memo 10)))

(provide 'fibonacci)
;;; fibonacci.el ends here
;; fibonacci:1 ends here
