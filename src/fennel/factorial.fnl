;;; factorial


;;; [[file:../../showcase-fennel.org::*factorial][factorial:1]]
;; Recursive implementation
(fn factorial-recursive [n]
  (if (<= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

;; Tail-recursive implementation
(fn factorial-tail [n acc]
  (let [acc (or acc 1)]
    (if (<= n 1)
        acc
        (factorial-tail (- n 1) (* n acc)))))

;; Iterative implementation
(fn factorial-iterative [n]
  (var result 1)
  (for [i 1 n]
    (set result (* result i)))
  result)

;; Example usage
(fn run-examples []
  (print (.. "Factorial of 5 (recursive): " (factorial-recursive 5)))
  (print (.. "Factorial of 5 (tail-recursive): " (factorial-tail 5)))
  (print (.. "Factorial of 5 (iterative): " (factorial-iterative 5))))

(run-examples)
;;; factorial:1 ends here
