;; Recursive implementation
(fn fib-recursive [n]
  (if (= n 0) 0
      (= n 1) 1
      (+ (fib-recursive (- n 1))
         (fib-recursive (- n 2)))))

;; Tail-recursive implementation
(fn fib-tail [n]
  (var [a b] [0 1])
  (for [_ 1 n]
    (set [a b] [b (+ a b)]))
  a)

;; Memoized implementation
(local memo {})

(fn fib-memo [n]
  (if (. memo n)
      (. memo n)
      (let [result (if (< n 2)
                       n
                       (+ (fib-memo (- n 1))
                          (fib-memo (- n 2))))]
        (tset memo n result)
        result)))

;; Example usage
(fn run-examples []
  (print (.. "Fibonacci of 10 (recursive): " (fib-recursive 10)))
  (print (.. "Fibonacci of 10 (tail-recursive): " (fib-tail 10)))
  (print (.. "Fibonacci of 10 (memoized): " (fib-memo 10))))

(run-examples)
