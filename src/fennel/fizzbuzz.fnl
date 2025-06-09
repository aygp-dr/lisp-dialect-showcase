;; fizzbuzz


;; [[file:../../showcase-fennel.org::*fizzbuzz][fizzbuzz:1]]
;; FizzBuzz implementation
(fn fizzbuzz [n]
  (for [i 1 n]
    (print (if (= (% i 15) 0) "FizzBuzz"
               (= (% i 3) 0) "Fizz"
               (= (% i 5) 0) "Buzz"
               i))))

;; FizzBuzz returning a table
(fn fizzbuzz-table [n]
  (local result [])
  (for [i 1 n]
    (table.insert result
                 (if (= (% i 15) 0) "FizzBuzz"
                     (= (% i 3) 0) "Fizz"
                     (= (% i 5) 0) "Buzz"
                     i)))
  result)

;; Example usage
(fn run-examples []
  (print "FizzBuzz (1-20):")
  (fizzbuzz 20)
  (print "\nFizzBuzz as table (1-20):")
  (local result (fizzbuzz-table 20))
  (var output "")
  (each [i v (ipairs result)]
    (set output (.. output v " ")))
  (print output))

(run-examples)
;; fizzbuzz:1 ends here
