;; primes


;; [[file:../../showcase-clojure.org::*primes][primes:1]]
(prn (binding [*out* (java.io.StringWriter.)](ns clojure.primes
  (:gen-class))


(defn prime? [n]
  (cond
    (<= n 1) false
    (= n 2) true
    (even? n) false
    :else (not-any? #(zero? (mod n %))
                    (range 3 (inc (Math/sqrt n)) 2))))


(defn sieve-of-eratosthenes [n]
  (let [n (int n)]
    (if (<= n 1)
      []
      (let [sqrt-n (int (Math/sqrt n))
            sieve (boolean-array (inc n) true)]
        (aset sieve 0 false)
        (aset sieve 1 false)
        (doseq [i (range 2 (inc sqrt-n))]
          (when (aget sieve i)
            (doseq [j (range (* i i) (inc n) i)]
              (aset sieve j false))))
        (filter #(aget sieve %) (range 2 (inc n)))))))


(defn primes-up-to [n]
  (filter prime? (range 2 (inc n))))


(defn run-examples []
  (println "Primes up to 20:" (primes-up-to 20))
  (println "Primes up to 20 (sieve):" (sieve-of-eratosthenes 20))
  (println "Is 17 prime?" (prime? 17))
  (println "Is 15 prime?" (prime? 15)))

(defn -main []
  (run-examples))))
;; primes:1 ends here
