;; quicksort


;; [[file:../../showcase-common-lisp.org::*quicksort][quicksort:1]]
;; Quicksort


;; [[file:../../SETUP.org::*Quicksort][Quicksort:1]]
(defpackage :cl-quicksort
  (:use :cl)
  (:export :quicksort :quicksort-functional))

(in-package :cl-quicksort)

;; Functional implementation of quicksort
(defun quicksort-functional (list)
  "Sort a list using functional quicksort."
  (if (or (null list) (null (cdr list)))
      list
      (let* ((pivot (car list))
             (rest (cdr list))
             (lesser (remove-if-not (lambda (x) (< x pivot)) rest))
             (greater (remove-if-not (lambda (x) (>= x pivot)) rest)))
        (append (quicksort-functional lesser)
                (list pivot)
                (quicksort-functional greater)))))

;; Destructive in-place quicksort
(defun quicksort (sequence &key (start 0) (end (length sequence)) (predicate #'<))
  "Sort a sequence in-place using quicksort."
  (when (> (- end start) 1)
    (let ((pivot-pos (partition sequence start end predicate)))
      (quicksort sequence :start start :end pivot-pos :predicate predicate)
      (quicksort sequence :start (1+ pivot-pos) :end end :predicate predicate)))
  sequence)

;; Helper function for in-place quicksort
(defun partition (sequence start end predicate)
  (let ((pivot (elt sequence start))
        (i (1+ start)))
    (loop for j from (1+ start) below end
          when (funcall predicate (elt sequence j) pivot)
          do (progn
               (rotatef (elt sequence i) (elt sequence j))
               (incf i)))
    (rotatef (elt sequence start) (elt sequence (1- i)))
    (1- i)))

;; Example usage
(defun run-examples ()
  (let ((list1 '(3 1 4 1 5 9 2 6 5 3 5))
        (list2 '(3 1 4 1 5 9 2 6 5 3 5))
        (vector1 #(3 1 4 1 5 9 2 6 5 3 5)))
    (format t "Original list: ~a~%" list1)
    (format t "Functional quicksort: ~a~%" (quicksort-functional list1))
    (format t "Original list unchanged: ~a~%" list1)
    (format t "Destructive quicksort (list): ~a~%" (quicksort list2))
    (format t "Destructive quicksort (vector): ~a~%" (quicksort vector1))))
;; Quicksort:1 ends here
;; quicksort:1 ends here
