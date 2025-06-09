;; quicksort


;; [[file:../../showcase-hy.org::*quicksort][quicksort:1]]
#!/usr/bin/env hy

;; Quicksort implementation
(defn quicksort [lst]
  (if (or (not lst) (= (len lst) 1))
      lst
      (let [pivot (first lst)
            rest (list (rest lst))]
        (+ (quicksort (lfor x rest :if (< x pivot) x))
           [pivot]
           (quicksort (lfor x rest :if (>= x pivot) x))))))

;; Quicksort using filter
(defn quicksort-filter [lst]
  (if (or (not lst) (= (len lst) 1))
      lst
      (let [pivot (first lst)
            rest (list (rest lst))]
        (+ (quicksort-filter (list (filter (fn [x] (< x pivot)) rest)))
           [pivot]
           (quicksort-filter (list (filter (fn [x] (>= x pivot)) rest)))))))

;; Example usage
(defn run-examples []
  (setv numbers [3 1 4 1 5 9 2 6 5 3 5])
  (print f"Original list: {numbers}")
  (print f"Sorted list: {(quicksort numbers)}")
  (print f"Sorted with filter: {(quicksort-filter numbers)}")
  )

(when (= __name__ "__main__")
  (run-examples))
;; quicksort:1 ends here
