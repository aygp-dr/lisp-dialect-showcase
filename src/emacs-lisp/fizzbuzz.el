;; fizzbuzz


;; [[file:../../showcase-emacs-lisp.org::*fizzbuzz][fizzbuzz:1]]
;;; fizzbuzz.el --- FizzBuzz implementation in Emacs Lisp

;;; Commentary:
;; FizzBuzz implementation in Emacs Lisp

;;; Code:

;; FizzBuzz implementation
(defun fizzbuzz (n)
  "Print FizzBuzz from 1 to N."
  (dotimes (i n)
    (let ((num (1+ i)))
      (message "%s"
               (cond
                ((zerop (mod num 15)) "FizzBuzz")
                ((zerop (mod num 3)) "Fizz")
                ((zerop (mod num 5)) "Buzz")
                (t num))))))

;; Function to return a list of FizzBuzz values
(defun fizzbuzz-list (n)
  "Return a list of FizzBuzz values from 1 to N."
  (let ((result nil))
    (dotimes (i n)
      (let ((num (1+ i)))
        (push
         (cond
          ((zerop (mod num 15)) "FizzBuzz")
          ((zerop (mod num 3)) "Fizz")
          ((zerop (mod num 5)) "Buzz")
          (t num))
         result)))
    (nreverse result)))

;; Example usage
(defun run-fizzbuzz-examples ()
  "Run examples of FizzBuzz functions."
  (message "FizzBuzz (1-20):")
  (fizzbuzz 20)
  (message "\nFizzBuzz as list (1-20): %S" (fizzbuzz-list 20)))

(provide 'fizzbuzz)
;;; fizzbuzz.el ends here
;; fizzbuzz:1 ends here
