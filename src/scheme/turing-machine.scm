;; Turing Machine Simulator
;; Implementation-agnostic Scheme code for a basic Turing machine

;;; Tape Implementation
;; We represent the tape as an expandable vector with a current position
;; This allows for infinite tape in both directions

;; Create a new tape with default blank symbol
(define (make-tape . initial-contents)
  (let ((tape-vector (make-vector 100 0))  ; Default tape size
        (position 50)                      ; Start in the middle
        (blank-symbol 0))                  ; Default blank symbol
    
    ;; Initialize tape with contents if provided
    (when (not (null? initial-contents))
      (let ((contents (car initial-contents)))
        (let loop ((i 0) (p position))
          (when (< i (length contents))
            (vector-set! tape-vector p (list-ref contents i))
            (loop (+ i 1) (+ p 1))))))
    
    ;; Return tape object with operations
    (lambda (op . args)
      (case op
        ((read)
         (vector-ref tape-vector position))
        
        ((write)
         (vector-set! tape-vector position (car args)))
        
        ((move-left)
         (set! position (- position 1))
         ;; Expand tape if needed
         (when (< position 0)
           (let ((new-vector (make-vector (* (vector-length tape-vector) 2) blank-symbol)))
             (let ((new-position (+ position (vector-length tape-vector))))
               (let loop ((i 0))
                 (when (< i (vector-length tape-vector))
                   (vector-set! new-vector (+ i (vector-length tape-vector)) 
                               (vector-ref tape-vector i))
                   (loop (+ i 1))))
               (set! tape-vector new-vector)
               (set! position new-position)))))
        
        ((move-right)
         (set! position (+ position 1))
         ;; Expand tape if needed
         (when (>= position (vector-length tape-vector))
           (let ((new-vector (make-vector (* (vector-length tape-vector) 2) blank-symbol)))
             (let loop ((i 0))
               (when (< i (vector-length tape-vector))
                 (vector-set! new-vector i (vector-ref tape-vector i))
                 (loop (+ i 1))))
             (set! tape-vector new-vector))))
        
        ((position)
         position)
        
        ((get-region)
         (let* ((start (max 0 (- position 10)))
                (end (min (- (vector-length tape-vector) 1) (+ position 10)))
                (result '()))
           (let loop ((i start))
             (if (> i end)
                 (reverse result)
                 (begin
                   (set! result (cons (vector-ref tape-vector i) result))
                   (loop (+ i 1)))))))
        
        ((display)
         (let* ((start (max 0 (- position 10)))
                (end (min (- (vector-length tape-vector) 1) (+ position 10))))
           (display "Tape: ")
           (let loop ((i start))
             (when (<= i end)
               (if (= i position)
                   (begin
                     (display "[")
                     (display (vector-ref tape-vector i))
                     (display "]"))
                   (begin
                     (display " ")
                     (display (vector-ref tape-vector i))
                     (display " ")))
               (loop (+ i 1))))
           (newline)))
        
        (else (error "Unknown tape operation:" op))))))

;;; Transition Function
;; Each transition is (current-state symbol) -> (new-state new-symbol direction)
;; Direction can be 'left, 'right, or 'stay

;; Helper to define transitions
(define (make-transition-table)
  (let ((transitions '()))
    ;; Return transition function
    (lambda (op . args)
      (case op
        ((add)
         (let ((from-state (list-ref args 0))
               (read-symbol (list-ref args 1))
               (to-state (list-ref args 2))
               (write-symbol (list-ref args 3))
               (direction (list-ref args 4)))
           (set! transitions 
                 (cons (list (list from-state read-symbol) 
                             (list to-state write-symbol direction))
                       transitions))))
        
        ((lookup)
         (let ((state (list-ref args 0))
               (symbol (list-ref args 1)))
           (let ((result (assoc (list state symbol) transitions)))
             (if result
                 (cadr result)
                 #f))))
        
        ((clear)
         (set! transitions '()))
        
        ((display)
         (display "Transition Table:\n")
         (for-each 
          (lambda (transition)
            (let ((from (car transition))
                  (to (cadr transition)))
              (format #t "(~a, ~a) -> (~a, ~a, ~a)\n" 
                      (car from) (cadr from)
                      (car to) (cadr to) (caddr to))))
          transitions))
        
        (else (error "Unknown transition table operation:" op))))))

;;; Turing Machine
;; Create a new Turing machine with given states and transition function
(define (make-turing-machine . initial-tape)
  (let ((tape (apply make-tape initial-tape))
        (state 'q0)                 ; Start state
        (accept-state 'qaccept)     ; Accept state
        (reject-state 'qreject)     ; Reject state
        (transitions (make-transition-table))
        (step-count 0)              ; Number of steps executed
        (max-steps 10000)           ; Maximum steps to prevent infinite loops
        (halted #f))                ; Whether the machine has halted
    
    ;; Execute a single step of the machine
    (define (step)
      (if halted
          'halted
          (let* ((symbol (tape 'read))
                 (transition (transitions 'lookup state symbol)))
            (set! step-count (+ step-count 1))
            
            (if (or (not transition) 
                    (eq? state accept-state) 
                    (eq? state reject-state) 
                    (>= step-count max-steps))
                (begin
                  (set! halted #t)
                  (if (>= step-count max-steps)
                      'exceeded-max-steps
                      (if (eq? state accept-state)
                          'accepted
                          'rejected)))
                (let ((new-state (car transition))
                      (new-symbol (cadr transition))
                      (direction (caddr transition)))
                  (tape 'write new-symbol)
                  (case direction
                    ((left) (tape 'move-left))
                    ((right) (tape 'move-right))
                    ((stay) 'stay)
                    (else (error "Invalid direction:" direction)))
                  (set! state new-state)
                  state)))))
    
    ;; Return machine interface
    (lambda (op . args)
      (case op
        ((step)
         (step))
        
        ((run)
         (let ((steps (if (null? args) -1 (car args))))
           (if (= steps -1)
               ;; Run until halted
               (let loop ((result (step)))
                 (if (or (eq? result 'halted)
                         (eq? result 'accepted)
                         (eq? result 'rejected)
                         (eq? result 'exceeded-max-steps))
                     result
                     (loop (step))))
               ;; Run specified number of steps
               (let loop ((i 0) (result 'running))
                 (if (or (>= i steps)
                         (eq? result 'halted)
                         (eq? result 'accepted)
                         (eq? result 'rejected)
                         (eq? result 'exceeded-max-steps))
                     result
                     (loop (+ i 1) (step)))))))
        
        ((add-transition)
         (apply transitions (cons 'add args)))
        
        ((state)
         state)
        
        ((set-state)
         (set! state (car args)))
        
        ((set-accept)
         (set! accept-state (car args)))
        
        ((set-reject)
         (set! reject-state (car args)))
        
        ((set-max-steps)
         (set! max-steps (car args)))
        
        ((halted?)
         halted)
        
        ((reset)
         (set! state 'q0)
         (set! step-count 0)
         (set! halted #f)
         (tape 'write 0)  ; Write blank at current position
         'reset)
        
        ((display)
         (display "Turing Machine State: ")
         (display state)
         (display ", Steps: ")
         (display step-count)
         (if halted
             (begin
               (display " (HALTED)")
               (if (eq? state accept-state)
                   (display " - ACCEPTED")
                   (if (eq? state reject-state)
                       (display " - REJECTED")
                       (if (>= step-count max-steps)
                           (display " - EXCEEDED MAX STEPS")))))
             (display " (RUNNING)"))
         (newline)
         (tape 'display))
        
        ((status)
         (list 'state state 'halted halted 'steps step-count))
        
        ((tape)
         (if (null? args)
             tape
             (apply tape args)))
        
        (else (error "Unknown Turing machine operation:" op))))))

;;; Example Turing Machine Programs

;; Binary increment: Increments a binary number on the tape
;; Format: A binary number with least significant bit on the left
(define (make-binary-incrementer)
  (let ((tm (make-turing-machine (list 0 1 1 0 1))))  ; Example: 10110 (LSB first)
    ;; Define the transition function
    ;; q0: Initial state, moving right to the end of the number
    (tm 'add-transition 'q0 0 'q0 0 'right)
    (tm 'add-transition 'q0 1 'q0 1 'right)
    (tm 'add-transition 'q0 #f 'q1 #f 'left)  ; End of number, move left
    
    ;; q1: Moving left, performing the increment
    (tm 'add-transition 'q1 0 'qaccept 1 'stay)  ; Replace 0 with 1 and accept
    (tm 'add-transition 'q1 1 'q1 0 'left)       ; Replace 1 with 0 and continue left
    (tm 'add-transition 'q1 #f 'qaccept 1 'stay) ; Add a new digit 1 at the left
    
    ;; Return the Turing machine
    tm))

;; Palindrome recognizer: Checks if a string of a's and b's is a palindrome
(define (make-palindrome-checker)
  (let ((tm (make-turing-machine (list 'a 'b 'b 'a))))  ; Example: abba
    ;; Define the transition function
    ;; q0: Initial state, moving right until we find a blank
    (tm 'add-transition 'q0 'a 'move-right-a 'X 'right)  ; Mark 'a' as 'X' and remember it
    (tm 'add-transition 'q0 'b 'move-right-b 'Y 'right)  ; Mark 'b' as 'Y' and remember it
    (tm 'add-transition 'q0 'X 'q0 'X 'right)           ; Skip over already checked symbols
    (tm 'add-transition 'q0 'Y 'q0 'Y 'right)           ; Skip over already checked symbols
    (tm 'add-transition 'q0 #f 'q-accept #f 'stay)      ; Empty string is a palindrome
    
    ;; move-right-a: Move right until we find a blank, then move left to check for 'a'
    (tm 'add-transition 'move-right-a 'a 'move-right-a 'a 'right)
    (tm 'add-transition 'move-right-a 'b 'move-right-a 'b 'right)
    (tm 'add-transition 'move-right-a 'X 'move-right-a 'X 'right)
    (tm 'add-transition 'move-right-a 'Y 'move-right-a 'Y 'right)
    (tm 'add-transition 'move-right-a #f 'check-a #f 'left)
    
    ;; move-right-b: Move right until we find a blank, then move left to check for 'b'
    (tm 'add-transition 'move-right-b 'a 'move-right-b 'a 'right)
    (tm 'add-transition 'move-right-b 'b 'move-right-b 'b 'right)
    (tm 'add-transition 'move-right-b 'X 'move-right-b 'X 'right)
    (tm 'add-transition 'move-right-b 'Y 'move-right-b 'Y 'right)
    (tm 'add-transition 'move-right-b #f 'check-b #f 'left)
    
    ;; check-a: Check if the rightmost character is 'a', mark it, and return to the left
    (tm 'add-transition 'check-a 'a 'move-left 'X 'left)     ; Match! Mark as checked
    (tm 'add-transition 'check-a 'X 'move-left 'X 'left)     ; Already marked, continue
    (tm 'add-transition 'check-a 'b 'qreject 'b 'stay)       ; Mismatch - reject
    (tm 'add-transition 'check-a 'Y 'qreject 'Y 'stay)       ; Mismatch - reject
    
    ;; check-b: Check if the rightmost character is 'b', mark it, and return to the left
    (tm 'add-transition 'check-b 'b 'move-left 'Y 'left)     ; Match! Mark as checked
    (tm 'add-transition 'check-b 'Y 'move-left 'Y 'left)     ; Already marked, continue
    (tm 'add-transition 'check-b 'a 'qreject 'a 'stay)       ; Mismatch - reject
    (tm 'add-transition 'check-b 'X 'qreject 'X 'stay)       ; Mismatch - reject
    
    ;; move-left: Move left to the first unmarked character
    (tm 'add-transition 'move-left 'a 'move-left 'a 'left)
    (tm 'add-transition 'move-left 'b 'move-left 'b 'left)
    (tm 'add-transition 'move-left 'X 'move-left 'X 'left)
    (tm 'add-transition 'move-left 'Y 'move-left 'Y 'left)
    (tm 'add-transition 'move-left #f 'q-match #f 'right)    ; Reached left end
    
    ;; q-match: All characters have been matched
    (tm 'add-transition 'q-match 'X 'q0 'X 'right)           ; Continue with next character
    (tm 'add-transition 'q-match 'Y 'q0 'Y 'right)           ; Continue with next character
    (tm 'add-transition 'q-match #f 'qaccept #f 'stay)       ; All characters matched - accept!
    
    ;; Return the Turing machine
    tm))

;; Simple unary addition: Adds two unary numbers separated by a '+'
(define (make-unary-adder)
  (let ((tm (make-turing-machine (list 1 1 1 '+ 1 1))))  ; Example: 111+11 (= 5)
    ;; Define the transition function
    ;; q0: Initial state, looking for the '+'
    (tm 'add-transition 'q0 1 'q0 1 'right)      ; Skip over 1's
    (tm 'add-transition 'q0 '+ 'q1 0 'right)     ; Found '+', replace with 0, move right
    
    ;; q1: Moving right past the second number
    (tm 'add-transition 'q1 1 'q1 1 'right)      ; Skip over 1's
    (tm 'add-transition 'q1 #f 'q2 #f 'left)     ; Reached end, move back left
    
    ;; q2: Found the last 1, change to '+' to mark end of result
    (tm 'add-transition 'q2 1 'q3 '+ 'left)      ; Change last 1 to '+', move left
    
    ;; q3: Moving left to the 0 (former '+')
    (tm 'add-transition 'q3 1 'q3 1 'left)       ; Skip over 1's
    (tm 'add-transition 'q3 0 'q4 1 'right)      ; Found 0, change to 1, move right
    
    ;; q4: Moving right to the '+'
    (tm 'add-transition 'q4 1 'q4 1 'right)      ; Skip over 1's
    (tm 'add-transition 'q4 '+ 'qaccept '+ 'stay) ; Found '+', accept
    
    ;; Return the Turing machine
    tm))

;;; Demo functions

;; Demonstrate binary incrementer
(define (demo-binary-incrementer)
  (let ((tm (make-binary-incrementer)))
    (display "Binary Incrementer Demonstration\n")
    (display "================================\n")
    (display "Initial state (LSB first):\n")
    (tm 'display)
    
    (display "\nRunning...\n")
    (tm 'run)
    
    (display "\nFinal state:\n")
    (tm 'display)
    
    (display "\nIncrementing again...\n")
    (tm 'reset)
    (tm 'run)
    (tm 'display)))

;; Demonstrate palindrome checker
(define (demo-palindrome-checker)
  (let ((palindrome-tm (make-palindrome-checker)))
    (display "\nPalindrome Checker Demonstration\n")
    (display "================================\n")
    (display "Testing 'abba':\n")
    (palindrome-tm 'display)
    
    (display "\nRunning...\n")
    (palindrome-tm 'run)
    
    (display "\nFinal state:\n")
    (palindrome-tm 'display)
    
    (display "\nTesting non-palindrome 'abc':\n")
    (let ((non-palindrome-tm (make-turing-machine (list 'a 'b 'c))))
      (non-palindrome-tm 'add-transition 'q0 'a 'move-right-a 'X 'right)
      (non-palindrome-tm 'add-transition 'q0 'b 'move-right-b 'Y 'right)
      (non-palindrome-tm 'add-transition 'q0 'c 'move-right-c 'Z 'right)
      (non-palindrome-tm 'add-transition 'q0 'X 'q0 'X 'right)
      (non-palindrome-tm 'add-transition 'q0 'Y 'q0 'Y 'right)
      (non-palindrome-tm 'add-transition 'q0 'Z 'q0 'Z 'right)
      (non-palindrome-tm 'add-transition 'q0 #f 'qaccept #f 'stay)
      
      (non-palindrome-tm 'add-transition 'move-right-a 'a 'move-right-a 'a 'right)
      (non-palindrome-tm 'add-transition 'move-right-a 'b 'move-right-a 'b 'right)
      (non-palindrome-tm 'add-transition 'move-right-a 'c 'move-right-a 'c 'right)
      (non-palindrome-tm 'add-transition 'move-right-a 'X 'move-right-a 'X 'right)
      (non-palindrome-tm 'add-transition 'move-right-a 'Y 'move-right-a 'Y 'right)
      (non-palindrome-tm 'add-transition 'move-right-a 'Z 'move-right-a 'Z 'right)
      (non-palindrome-tm 'add-transition 'move-right-a #f 'check-a #f 'left)
      
      (non-palindrome-tm 'add-transition 'check-a 'a 'move-left 'X 'left)
      (non-palindrome-tm 'add-transition 'check-a 'X 'move-left 'X 'left)
      (non-palindrome-tm 'add-transition 'check-a 'b 'qreject 'b 'stay)
      (non-palindrome-tm 'add-transition 'check-a 'c 'qreject 'c 'stay)
      (non-palindrome-tm 'add-transition 'check-a 'Y 'qreject 'Y 'stay)
      (non-palindrome-tm 'add-transition 'check-a 'Z 'qreject 'Z 'stay)
      
      (non-palindrome-tm 'display)
      (display "\nRunning...\n")
      (non-palindrome-tm 'run)
      (display "\nFinal state:\n")
      (non-palindrome-tm 'display))))

;; Demonstrate unary adder
(define (demo-unary-adder)
  (let ((tm (make-unary-adder)))
    (display "\nUnary Adder Demonstration\n")
    (display "=========================\n")
    (display "Adding 3 + 2 (111+11):\n")
    (tm 'display)
    
    (display "\nRunning...\n")
    (tm 'run)
    
    (display "\nFinal state:\n")
    (tm 'display)))

;; Run all demos
(define (turing-machine-demo)
  (display "Turing Machine Simulator in Scheme\n")
  (display "==================================\n")
  
  (demo-binary-incrementer)
  (demo-palindrome-checker)
  (demo-unary-adder)
  
  (display "\nTuring machine demonstrations complete.\n"))

;; Run the demo if this file is executed directly
(turing-machine-demo)