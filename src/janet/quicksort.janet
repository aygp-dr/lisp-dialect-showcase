;; quicksort


;; [[file:../../showcase-janet.org::*quicksort][quicksort:1]]
# Quicksort implementation
(defn quicksort [arr]
  (if (or (= (length arr) 0) (= (length arr) 1))
    arr
    (let [pivot (first arr)
          rest (array/slice arr 1)
          lesser (filter |(< $ pivot) rest)
          greater (filter |(>= $ pivot) rest)]
      (array/concat (quicksort lesser) @[pivot] (quicksort greater)))))

# Example usage
(defn run-examples []
  (def numbers @[3 1 4 1 5 9 2 6 5 3 5])
  (printf "Original array: %j" numbers)
  (printf "Sorted array: %j" (quicksort numbers)))

(run-examples)
;; quicksort:1 ends here
