;; fizzbuzz


;; [[file:../../showcase-hy.org::*fizzbuzz][fizzbuzz:1]]
#!/usr/bin/env hy

;; FizzBuzz implementation
(defn fizzbuzz [n]
  (for [i (range 1 (+ n 1))]
    (print (cond [(= (% i 15) 0) "FizzBuzz"]
                 [(= (% i 3) 0) "Fizz"]
                 [(= (% i 5) 0) "Buzz"]
                 [True i]))))

;; FizzBuzz returning a list
(defn fizzbuzz-list [n]
  (lfor i (range 1 (+ n 1))
        (cond [(= (% i 15) 0) "FizzBuzz"]
              [(= (% i 3) 0) "Fizz"]
              [(= (% i 5) 0) "Buzz"]
              [True i])))

;; Using Python list comprehension style
(defn fizzbuzz-comp [n]
  (lfor i (range 1 (+ n 1))
        (if (= (% i 15) 0) "FizzBuzz"
            (if (= (% i 3) 0) "Fizz"
                (if (= (% i 5) 0) "Buzz" i)))))

;; Example usage
(defn run-examples []
  (print "FizzBuzz (1-20):")
  (fizzbuzz 20)
  (print "\nFizzBuzz as list (1-20):")
  (print (fizzbuzz-list 20))
  )

(when (= __name__ "__main__")
  (run-examples))
;; fizzbuzz:1 ends here
