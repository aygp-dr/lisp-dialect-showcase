;; quicksort


;; [[file:../../showcase-fennel.org::*quicksort][quicksort:1]]
;; Quicksort implementation
(fn quicksort [arr]
  (if (or (= (length arr) 0) (= (length arr) 1))
      arr
      (let [pivot (. arr 1)
            lesser []
            greater []]
        (for [i 2 (length arr)]
          (let [value (. arr i)]
            (if (< value pivot)
                (table.insert lesser value)
                (table.insert greater value))))
        (let [result (quicksort lesser)]
          (table.insert result pivot)
          (each [_ v (ipairs (quicksort greater))]
            (table.insert result v))
          result))))

;; Example usage
(fn run-examples []
  (local numbers [3 1 4 1 5 9 2 6 5 3 5])
  (print "Original list:")
  (print (table.concat numbers ", "))
  (print "\nSorted list:")
  (print (table.concat (quicksort numbers) ", ")))

(run-examples)
;; quicksort:1 ends here
