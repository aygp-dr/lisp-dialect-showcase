;; Factorial in GNU CLISP

;; Recursive implementation
(defun factorial-recursive (n)
  "Calculate factorial of N recursively."
  (if (<= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

;; Tail-recursive implementation
(defun factorial-tail (n &optional (acc 1))
  "Calculate factorial of N using tail recursion with accumulator ACC."
  (if (<= n 1)
      acc
      (factorial-tail (1- n) (* acc n))))

;; Iterative implementation
(defun factorial-iterative (n)
  "Calculate factorial of N iteratively."
  (let ((result 1))
    (dotimes (i n result)
      (setf result (* result (1+ i))))))

;; Example usage
(format t "Factorial of 5 (recursive): ~a~%" (factorial-recursive 5))
(format t "Factorial of 5 (tail-recursive): ~a~%" (factorial-tail 5))
(format t "Factorial of 5 (iterative): ~a~%" (factorial-iterative 5))
