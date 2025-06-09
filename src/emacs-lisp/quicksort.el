;; quicksort


;; [[file:../../showcase-emacs-lisp.org::*quicksort][quicksort:1]]
;;; quicksort.el --- Quicksort implementation in Emacs Lisp

;;; Commentary:
;; Quicksort implementation in Emacs Lisp

;;; Code:

;; Quicksort implementation for lists
(defun quicksort-list (list)
  "Sort LIST using quicksort algorithm."
  (if (or (null list) (null (cdr list)))
      list
    (let* ((pivot (car list))
           (rest (cdr list))
           (lesser (cl-remove-if-not (lambda (x) (< x pivot)) rest))
           (greater (cl-remove-if-not (lambda (x) (>= x pivot)) rest)))
      (append (quicksort-list lesser)
              (list pivot)
              (quicksort-list greater)))))

;; Quicksort implementation for vectors
(defun quicksort-vector (vec &optional start end)
  "Sort vector VEC in-place using quicksort from START to END."
  (let ((start (or start 0))
        (end (or end (length vec))))
    (when (> (- end start) 1)
      (let ((pivot-pos (partition vec start end)))
        (quicksort-vector vec start pivot-pos)
        (quicksort-vector vec (1+ pivot-pos) end)))
    vec))

;; Helper function for vector quicksort
(defun partition (vec start end)
  "Partition vector VEC from START to END and return pivot position."
  (let* ((pivot (aref vec start))
         (i (1+ start)))
    (cl-loop for j from (1+ start) below end
             when (< (aref vec j) pivot)
             do (progn
                  (cl-rotatef (aref vec i) (aref vec j))
                  (setq i (1+ i))))
    (cl-rotatef (aref vec start) (aref vec (1- i)))
    (1- i)))

;; Example usage
(defun run-quicksort-examples ()
  "Run examples of quicksort functions."
  (let ((list1 '(3 1 4 1 5 9 2 6 5 3 5))
        (vec1 [3 1 4 1 5 9 2 6 5 3 5]))
    (message "Original list: %S" list1)
    (message "Sorted list: %S" (quicksort-list list1))
    (message "Original vector: %S" vec1)
    (message "Sorted vector: %S" (quicksort-vector (copy-sequence vec1)))))

(provide 'quicksort)
;;; quicksort.el ends here
;; quicksort:1 ends here
