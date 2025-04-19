;; Fibonacci in GNU CLISP

;; Recursive implementation
(defun fib-recursive (n)
  "Calculate the nth Fibonacci number recursively."
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib-recursive (- n 1))
              (fib-recursive (- n 2))))))

;; Iterative implementation
(defun fib-iterative (n)
  "Calculate the nth Fibonacci number iteratively."
  (if (< n 2)
      n
      (let ((a 0) (b 1))
        (dotimes (i (- n 1) b)
          (let ((temp (+ a b)))
            (setf a b)
            (setf b temp))))))

;; Tail-recursive implementation
(defun fib-tail (n &optional (a 0) (b 1))
  "Calculate the nth Fibonacci number using tail recursion."
  (if (= n 0)
      a
      (fib-tail (1- n) b (+ a b))))

;; Memoization implementation
(let ((memo (make-hash-table)))
  (defun fib-memo (n)
    "Calculate the nth Fibonacci number using memoization."
    (or (gethash n memo)
        (setf (gethash n memo)
              (if (< n 2)
                  n
                  (+ (fib-memo (- n 1))
                     (fib-memo (- n 2))))))))

;; Example usage
(format t "Fibonacci of 10 (recursive): ~a~%" (fib-recursive 10))
(format t "Fibonacci of 10 (iterative): ~a~%" (fib-iterative 10))
(format t "Fibonacci of 10 (tail-recursive): ~a~%" (fib-tail 10))
(format t "Fibonacci of 10 (memoized): ~a~%" (fib-memo 10))
