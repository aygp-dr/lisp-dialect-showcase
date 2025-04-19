;; Prime numbers in GNU CLISP

;; Check if a number is prime
(defun prime-p (n)
  "Check if N is a prime number."
  (cond ((<= n 1) nil)  ; 0 and 1 are not prime
        ((= n 2) t)     ; 2 is prime
        ((evenp n) nil) ; even numbers > 2 aren't prime
        (t (loop for i from 3 to (isqrt n) by 2
                 never (zerop (mod n i))))))

;; Generate primes up to n using a simple test
(defun primes-up-to (n)
  "Generate a list of prime numbers up to N."
  (loop for i from 2 to n
        when (prime-p i)
        collect i))

;; Sieve of Eratosthenes implementation
(defun sieve-of-eratosthenes (n)
  "Generate prime numbers up to N using the Sieve of Eratosthenes."
  (let ((sieve (make-array (1+ n) :initial-element t)))
    ;; 0 and 1 are not prime
    (setf (aref sieve 0) nil
          (aref sieve 1) nil)
    ;; Mark multiples of each prime as non-prime
    (loop for i from 2 to (isqrt n)
          when (aref sieve i)
          do (loop for j from (* i i) to n by i
                   do (setf (aref sieve j) nil)))
    ;; Collect the primes
    (loop for i from 2 to n
          when (aref sieve i)
          collect i)))

;; Example usage
(format t "Primes up to 20: ~a~%" (primes-up-to 20))
(format t "Primes up to 20 (sieve): ~a~%" (sieve-of-eratosthenes 20))
(format t "Is 17 prime? ~:[No~;Yes~]~%" (prime-p 17))
(format t "Is 15 prime? ~:[No~;Yes~]~%" (prime-p 15))
