;; fizzbuzz


;; [[file:../../showcase-clojure.org::*fizzbuzz][fizzbuzz:1]]
(prn (binding [*out* (java.io.StringWriter.)](ns clojure.fizzbuzz
  (:gen-class))


(defn fizzbuzz-cond [n]
  (map (fn [i]
         (cond
           (zero? (mod i 15)) "FizzBuzz"
           (zero? (mod i 3)) "Fizz"
           (zero? (mod i 5)) "Buzz"
           :else i))
       (range 1 (inc n))))


(defn fizzbuzz-match [n]
  (for [i (range 1 (inc n))]
    (condp #(zero? (mod %2 %1)) i
      15 "FizzBuzz"
      3 "Fizz"
      5 "Buzz"
      i)))


(defn fizzbuzz-case [n]
  (for [i (range 1 (inc n))
        :let [div3 (zero? (mod i 3))
              div5 (zero? (mod i 5))]]
    (case [div3 div5]
      [true true] "FizzBuzz"
      [true false] "Fizz"
      [false true] "Buzz"
      i)))


(defn run-examples []
  (println "FizzBuzz (1-20) using cond:")
  (doseq [item (fizzbuzz-cond 20)]
    (println item))
  (println "\nFizzBuzz (1-20) using pattern matching:")
  (println (fizzbuzz-match 20)))

(defn -main []
  (run-examples))))
;; fizzbuzz:1 ends here
