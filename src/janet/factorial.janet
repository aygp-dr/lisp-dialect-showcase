;;; factorial


;;; [[file:../../showcase-janet.org::*factorial][factorial:1]]
# Recursive implementation
(defn factorial-recursive [n]
  (if (<= n 1)
    1
    (* n (factorial-recursive (- n 1)))))

# Tail-recursive implementation
(defn factorial-tail [n &opt acc]
  (default acc 1)
  (if (<= n 1)
    acc
    (factorial-tail (- n 1) (* n acc))))

# Iterative implementation
(defn factorial-iterative [n]
  (var result 1)
  (for i 1 (+ n 1)
    (set result (* result i)))
  result)

# Example usage
(defn run-examples []
  (printf "Factorial of 5 (recursive): %d" (factorial-recursive 5))
  (printf "Factorial of 5 (tail-recursive): %d" (factorial-tail 5))
  (printf "Factorial of 5 (iterative): %d" (factorial-iterative 5)))

(run-examples)
;;; factorial:1 ends here
