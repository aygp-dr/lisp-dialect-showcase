;;; primes.el --- Prime number functions in Emacs Lisp

;;; Commentary:
;; Functions for working with prime numbers in Emacs Lisp

;;; Code:

;; Simple integer square root function
(defun int-sqrt (n)
  "Calculate integer square root of N."
  (floor (sqrt n)))

;; Check if a number is prime
(defun primep (n)
  "Check if N is a prime number."
  (when (> n 1)
    (let ((limit (int-sqrt n))
          (is-prime t)
          (divisor 2))
      (while (and is-prime (<= divisor limit))
        (when (= (mod n divisor) 0)
          (setq is-prime nil))
        (setq divisor (1+ divisor)))
      is-prime)))

;; Generate primes up to n
(defun primes-up-to (n)
  "Generate a list of primes up to N."
  (let ((result nil))
    (dotimes (i n)
      (let ((num (1+ i)))
        (when (primep num)
          (push num result))))
    (nreverse result)))

;; Sieve of Eratosthenes
(defun sieve-of-eratosthenes (n)
  "Generate primes up to N using the Sieve of Eratosthenes."
  (let ((sieve (make-bool-vector (1+ n) t)))
    ;; 0 and 1 are not prime
    (aset sieve 0 nil)
    (aset sieve 1 nil)
    ;; Mark multiples of each prime as non-prime
    (let ((limit (int-sqrt n)))
      (dotimes (i limit)
        (let ((num (+ i 2)))
          (when (aref sieve num)
            (let ((j (* num num)))
              (while (<= j n)
                (aset sieve j nil)
                (setq j (+ j num))))))))
    ;; Collect the primes
    (let ((primes nil))
      (dotimes (i (1- n))
        (let ((num (+ i 2)))
          (when (aref sieve num)
            (push num primes))))
      (nreverse primes))))

;; Example usage
(defun run-prime-examples ()
  "Run examples of prime number functions."
  (message "Primes up to 20: %S" (primes-up-to 20))
  (message "Primes up to 20 (sieve): %S" (sieve-of-eratosthenes 20))
  (message "Is 17 prime? %s" (if (primep 17) "Yes" "No"))
  (message "Is 15 prime? %s" (if (primep 15) "Yes" "No")))

(provide 'primes)
;;; primes.el ends here
