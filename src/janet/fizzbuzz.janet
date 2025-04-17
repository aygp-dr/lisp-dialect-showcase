# FizzBuzz implementation
(defn fizzbuzz [n]
  (for i 1 (+ n 1)
    (print (cond
             (= (% i 15) 0) "FizzBuzz"
             (= (% i 3) 0) "Fizz"
             (= (% i 5) 0) "Buzz"
             i))))

# FizzBuzz returning an array
(defn fizzbuzz-array [n]
  (def result @[])
  (for i 1 (+ n 1)
    (array/push result 
                (cond
                  (= (% i 15) 0) "FizzBuzz"
                  (= (% i 3) 0) "Fizz"
                  (= (% i 5) 0) "Buzz"
                  i)))
  result)

# Example usage
(defn run-examples []
  (print "FizzBuzz (1-20):")
  (fizzbuzz 20)
  (print "\nFizzBuzz as array (1-20):")
  (printf "%j" (fizzbuzz-array 20)))

(run-examples)
