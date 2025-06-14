;; fibonacci


;; [[file:../../showcase-clojure.org::*fibonacci][fibonacci:1]]
(prn (binding [*out* (java.io.StringWriter.)](ns clojure.fibonacci
  (:gen-class))


(defn fib-recursive [n]
  (cond 
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib-recursive (- n 1))
             (fib-recursive (- n 2)))))


(def fibs
  (lazy-cat [0 1] (map + fibs (rest fibs))))

(defn fib-lazy [n]
  (nth fibs n))


(defn fib-tail
  ([n] (fib-tail n 0 1))
  ([n a b]
   (if (zero? n)
     a
     (recur (dec n) b (+ a b)))))


(defn run-examples []
  (println "Fibonacci of 10 (recursive):" (fib-recursive 10))
  (println "Fibonacci of 10 (lazy):" (fib-lazy 10))
  (println "Fibonacci of 10 (tail):" (fib-tail 10))
  (println "First 10 Fibonacci numbers:" (take 10 fibs)))

(defn -main []
  (run-examples))))
;; fibonacci:1 ends here
