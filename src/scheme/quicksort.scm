;; Quicksort implementation
(define (quicksort lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (append (quicksort (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (quicksort (filter (lambda (x) (>= x pivot)) rest))))))

;; Helper: filter a list
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

;; Example usage
(define (run-examples)
  (let ((numbers '(3 1 4 1 5 9 2 6 5 3 5)))
    (display "Original list: ")
    (display numbers)
    (newline)
    (display "Sorted list: ")
    (display (quicksort numbers))
    (newline)))

(run-examples)
