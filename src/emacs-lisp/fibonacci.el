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

;; Memoization using global hash table
(defvar fib-memo-table (make-hash-table)
  "Memoization table for Fibonacci numbers.")

(puthash 0 0 fib-memo-table)
(puthash 1 1 fib-memo-table)

(defun fib-memo (n)
  "Calculate the Nth Fibonacci number using memoization."
  (or (gethash n fib-memo-table)
      (puthash n
               (+ (fib-memo (- n 1))
                  (fib-memo (- n 2)))
               fib-memo-table)))

;; Example usage
(defun run-fibonacci-examples ()
  "Run examples of various Fibonacci implementations."
  (message "Fibonacci of 10 (recursive): %d" (fib-recursive 10))
  (message "Fibonacci of 10 (dynamic programming): %d" (fib-dp 10))
  (message "Fibonacci of 10 (memoized): %d" (fib-memo 10)))

(provide 'fibonacci)
;;; fibonacci.el ends here
