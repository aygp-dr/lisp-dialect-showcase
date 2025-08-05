;; quicksort


;; [[file:../../showcase-clojure.org::*quicksort][quicksort:1]]
(ns clojure.quicksort
  (:gen-class))


(defn quicksort [coll]
  (if (empty? coll)
    []
    (let [pivot (first coll)
          rest-coll (rest coll)]
      (concat (quicksort (filter #(< % pivot) rest-coll))
              [pivot]
              (quicksort (filter #(>= % pivot) rest-coll))))))


(defn quicksort-partition [coll]
  (if (or (empty? coll) (= 1 (count coll)))
    coll
    (let [pivot (first coll)
          parts (group-by #(compare % pivot) (rest coll))]
      (concat (quicksort-partition (get parts -1 []))
              [pivot]
              (quicksort-partition (get parts 1 []))
              (quicksort-partition (get parts 0 []))))))


(defn run-examples []
  (let [numbers [3 1 4 1 5 9 2 6 5 3 5]]
    (println "Original collection:" numbers)
    (println "Quicksort:" (quicksort numbers))
    (println "Quicksort with partition:" (quicksort-partition numbers))))

(defn -main []
  (run-examples))
;; quicksort:1 ends here
