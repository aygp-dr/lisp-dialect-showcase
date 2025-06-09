;; fibonacci


;; [[file:../../showcase-janet.org::*fibonacci][fibonacci:1]]
# Recursive implementation
(defn fib-recursive [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    (+ (fib-recursive (- n 1))
       (fib-recursive (- n 2)))))

# Memoized implementation
(defn fib-memo [n]
  (def cache @{})
  (defn fib [n]
    (if (get cache n)
      (get cache n)
      (let [result (if (< n 2)
                     n
                     (+ (fib (- n 1))
                        (fib (- n 2))))]
        (put cache n result)
        result)))
  (fib n))

# Tail-recursive implementation
(defn fib-tail [n]
  (defn fib-iter [a b count]
    (if (= count 0)
        a
        (fib-iter b (+ a b) (- count 1))))
  (fib-iter 0 1 n))

# Example usage
(defn run-examples []
  (printf "Fibonacci of 10 (recursive): %d" (fib-recursive 10))
  (printf "Fibonacci of 10 (memoized): %d" (fib-memo 10))
  (printf "Fibonacci of 10 (tail-recursive): %d" (fib-tail 10)))

(run-examples)
;; fibonacci:1 ends here
