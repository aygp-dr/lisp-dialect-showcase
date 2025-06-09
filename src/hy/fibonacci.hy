;; fibonacci


;; [[file:../../showcase-hy.org::*fibonacci][fibonacci:1]]
#!/usr/bin/env hy

;; Recursive implementation
(defn fib-recursive [n]
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [True (+ (fib-recursive (- n 1))
                 (fib-recursive (- n 2)))]))

;; Using Python's functionality
(import functools)

(defn fib-memo [n]
  (with-decorator (functools.lru_cache)
    (defn fib [n]
      (if (< n 2)
          n
          (+ (fib (- n 1))
             (fib (- n 2))))))
  (fib n))

;; Generator-based implementation
(defn fib-seq [n]
  (setv a 0)
  (setv b 1)
  (for [_ (range n)]
    (yield a)
    (setv [a b] [(, b (+ a b))]))
  (yield a))

(defn fib-gen [n]
  (list (take (+ n 1) (fib-seq 1000)))[-1])

;; Example usage
(defn run-examples []
  (print f"Fibonacci of 10 (recursive): {(fib-recursive 10)}")
  (print f"Fibonacci of 10 (memoized): {(fib-memo 10)}")
  (print f"Fibonacci of 10 (generator): {(fib-gen 10)}")
  (print f"First 10 Fibonacci numbers: {(list (take 10 (fib-seq 20)))}")
  )

(when (= __name__ "__main__")
  (run-examples))
;; fibonacci:1 ends here
