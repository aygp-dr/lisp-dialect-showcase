;; factorial


;; [[file:../../showcase-clojure.org::*factorial][factorial:1]]
(ns clojure.factorial
  (:gen-class))


(defn factorial-recursive [n]
  (if (<= n 1)
    1
    (* n (factorial-recursive (dec n)))))


(defn factorial-tail
  ([n] (factorial-tail n 1))
  ([n acc]
   (if (<= n 1)
     acc
     (recur (dec n) (* acc n)))))


(defn factorial-reduce [n]
  (reduce * (range 1 (inc n))))


(defn run-examples []
  (println "Factorial of 5 (recursive):" (factorial-recursive 5))
  (println "Factorial of 5 (tail):" (factorial-tail 5))
  (println "Factorial of 5 (reduce):" (factorial-reduce 5)))

(defn -main []
  (run-examples))
;; factorial:1 ends here
