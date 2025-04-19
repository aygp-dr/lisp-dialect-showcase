;; Quicksort in GNU CLISP

;; Partition function for quicksort
(defun partition (seq start end predicate)
  "Partition sequence SEQ from START to END using PREDICATE."
  (let ((pivot (elt seq start))
        (i start))
    (loop for j from (1+ start) to end do
      (when (funcall predicate (elt seq j) pivot)
        (incf i)
        (rotatef (elt seq i) (elt seq j))))
    (rotatef (elt seq start) (elt seq i))
    i))

;; In-place quicksort
(defun quicksort-inplace (sequence &optional (predicate #'<) (start 0) (end (1- (length sequence))))
  "Sort SEQUENCE in-place using quicksort algorithm."
  (when (< start end)
    (let ((pivot-index (partition sequence start end predicate)))
      (quicksort-inplace sequence predicate start (1- pivot-index))
      (quicksort-inplace sequence predicate (1+ pivot-index) end)))
  sequence)

;; Functional quicksort using filter
(defun quicksort (list &optional (predicate #'<))
  "Sort LIST using quicksort algorithm in a functional style."
  (if (or (null list) (null (cdr list)))
      list
      (let* ((pivot (car list))
             (rest (cdr list))
             (lesser (remove-if-not (lambda (x) (funcall predicate x pivot)) rest))
             (greater (remove-if (lambda (x) (funcall predicate x pivot)) rest)))
        (append (quicksort lesser predicate)
                (list pivot)
                (quicksort greater predicate)))))

;; Example usage
(let ((numbers '(3 1 4 1 5 9 2 6 5 3 5)))
  (format t "Original list: ~a~%" numbers)
  (format t "Sorted list (in-place): ~a~%" (quicksort-inplace (copy-list numbers)))
  (format t "Sorted list (functional): ~a~%" (quicksort numbers)))
