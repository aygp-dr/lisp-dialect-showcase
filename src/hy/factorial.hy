#!/usr/bin/env hy

;; Recursive implementation
(defn factorial-recursive [n]
  (if (<= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

;; Tail-recursive implementation
(defn factorial-tail [n &optional [acc 1]]
  (if (<= n 1)
      acc
      (factorial-tail (- n 1) (* n acc))))

;; Using Python's functionality
(import math)

(defn factorial-math [n]
  (math.factorial n))

;; Example usage
(defn run-examples []
  (print f"Factorial of 5 (recursive): {(factorial-recursive 5)}")
  (print f"Factorial of 5 (tail-recursive): {(factorial-tail 5)}")
  (print f"Factorial of 5 (math): {(factorial-math 5)}")
  )

(when (= __name__ "__main__")
  (run-examples))
