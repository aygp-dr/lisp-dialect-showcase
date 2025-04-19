;; Stack-Based Computer Simulator
;; Implementation-agnostic Scheme code for a Forth-like stack machine

;;; Stack Operations
;; Create a new stack
(define (make-stack)
  '())

;; Push a value onto the stack
(define (stack-push stack value)
  (cons value stack))

;; Pop a value from the stack
(define (stack-pop stack)
  (if (null? stack)
      (error "Stack underflow")
      (values (car stack) (cdr stack))))

;; Peek at the top value without popping
(define (stack-peek stack)
  (if (null? stack)
      (error "Empty stack")
      (car stack)))

;; Check if stack is empty
(define (stack-empty? stack)
  (null? stack))

;; Stack size
(define (stack-size stack)
  (length stack))

;;; Memory
;; Create a new memory with specified size
(define (make-memory size)
  (make-vector size 0))

;; Read from memory
(define (memory-read memory address)
  (if (and (>= address 0) (< address (vector-length memory)))
      (vector-ref memory address)
      (error "Memory address out of bounds:" address)))

;; Write to memory
(define (memory-write! memory address value)
  (if (and (>= address 0) (< address (vector-length memory)))
      (vector-set! memory address value)
      (error "Memory address out of bounds:" address)))

;;; Instruction Set
;; Define operation codes
(define OP-PUSH 0)      ; Push a value onto the stack
(define OP-POP 1)       ; Pop a value from the stack
(define OP-ADD 2)       ; Add top two values
(define OP-SUB 3)       ; Subtract
(define OP-MUL 4)       ; Multiply
(define OP-DIV 5)       ; Divide
(define OP-MOD 6)       ; Modulo
(define OP-AND 7)       ; Bitwise AND
(define OP-OR 8)        ; Bitwise OR
(define OP-XOR 9)       ; Bitwise XOR
(define OP-NOT 10)      ; Bitwise NOT
(define OP-EQ 11)       ; Equal comparison
(define OP-LT 12)       ; Less than
(define OP-GT 13)       ; Greater than
(define OP-DUP 14)      ; Duplicate top value
(define OP-SWAP 15)     ; Swap top two values
(define OP-OVER 16)     ; Copy second value to top
(define OP-DROP 17)     ; Discard top value
(define OP-STORE 18)    ; Store value in memory
(define OP-LOAD 19)     ; Load value from memory
(define OP-JMP 20)      ; Unconditional jump
(define OP-JZ 21)       ; Jump if zero
(define OP-JNZ 22)      ; Jump if not zero
(define OP-CALL 23)     ; Call subroutine
(define OP-RET 24)      ; Return from subroutine
(define OP-HALT 25)     ; Halt execution

;;; Virtual Machine
;; Create a new stack machine
(define (make-stack-machine memory-size)
  (let ((data-stack (make-stack))
        (return-stack (make-stack))
        (memory (make-memory memory-size))
        (ip 0)          ; Instruction pointer
        (running #t))   ; Execution state
    
    ;; Execute one instruction
    (define (execute-instruction opcode operand)
      (case opcode
        ((0) ; PUSH
         (set! data-stack (stack-push data-stack operand)))
        
        ((1) ; POP
         (let-values (((value new-stack) (stack-pop data-stack)))
           (set! data-stack new-stack)))
        
        ((2) ; ADD
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (+ a b))))))
        
        ((3) ; SUB
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (- a b))))))
        
        ((4) ; MUL
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (* a b))))))
        
        ((5) ; DIV
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (quotient a b))))))
        
        ((6) ; MOD
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (remainder a b))))))
        
        ((7) ; AND
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (logand a b))))))
        
        ((8) ; OR
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (logior a b))))))
        
        ((9) ; XOR
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (logxor a b))))))
        
        ((10) ; NOT
         (let-values (((a new-stack) (stack-pop data-stack)))
           (set! data-stack (stack-push new-stack (lognot a)))))
        
        ((11) ; EQ
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (if (= a b) 1 0))))))
        
        ((12) ; LT
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (if (< a b) 1 0))))))
        
        ((13) ; GT
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push new-stack2 (if (> a b) 1 0))))))
        
        ((14) ; DUP
         (let-values (((value new-stack) (stack-pop data-stack)))
           (set! data-stack (stack-push (stack-push new-stack value) value))))
        
        ((15) ; SWAP
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push (stack-push new-stack2 b) a)))))
        
        ((16) ; OVER
         (let-values (((b new-stack1) (stack-pop data-stack)))
           (let-values (((a new-stack2) (stack-pop new-stack1)))
             (set! data-stack (stack-push (stack-push (stack-push new-stack2 a) b) a)))))
        
        ((17) ; DROP
         (let-values (((value new-stack) (stack-pop data-stack)))
           (set! data-stack new-stack)))
        
        ((18) ; STORE
         (let-values (((address new-stack1) (stack-pop data-stack)))
           (let-values (((value new-stack2) (stack-pop new-stack1)))
             (memory-write! memory address value)
             (set! data-stack new-stack2))))
        
        ((19) ; LOAD
         (let-values (((address new-stack) (stack-pop data-stack)))
           (let ((value (memory-read memory address)))
             (set! data-stack (stack-push new-stack value)))))
        
        ((20) ; JMP
         (set! ip operand))
        
        ((21) ; JZ
         (let-values (((value new-stack) (stack-pop data-stack)))
           (set! data-stack new-stack)
           (when (= value 0)
             (set! ip operand))))
        
        ((22) ; JNZ
         (let-values (((value new-stack) (stack-pop data-stack)))
           (set! data-stack new-stack)
           (when (not (= value 0))
             (set! ip operand))))
        
        ((23) ; CALL
         (set! return-stack (stack-push return-stack ip))
         (set! ip operand))
        
        ((24) ; RET
         (let-values (((return-addr new-stack) (stack-pop return-stack)))
           (set! return-stack new-stack)
           (set! ip return-addr)))
        
        ((25) ; HALT
         (set! running #f))
        
        (else
         (error "Unknown opcode:" opcode))))
    
    ;; Run one cycle of the machine
    (define (run-cycle)
      (if running
          (let ((opcode (memory-read memory ip))
                (operand (memory-read memory (+ ip 1))))
            (set! ip (+ ip 2))  ; Move to next instruction
            (execute-instruction opcode operand)
            #t)
          #f))
    
    ;; Run until halted
    (define (run)
      (let loop ()
        (when (run-cycle)
          (loop))))
    
    ;; Load program into memory
    (define (load-program program)
      (let loop ((addr 0)
                 (instructions program))
        (when (and (< addr (vector-length memory)) 
                   (not (null? instructions)))
          (memory-write! memory addr (car instructions))
          (loop (+ addr 1) (cdr instructions))))
      (set! ip 0)
      (set! running #t))
    
    ;; Display machine state
    (define (display-state)
      (display "Instruction Pointer: ")
      (display ip)
      (newline)
      (display "Data Stack: ")
      (display data-stack)
      (newline)
      (display "Return Stack: ")
      (display return-stack)
      (newline))
    
    ;; Return interface functions
    (lambda (cmd . args)
      (case cmd
        ((load) (apply load-program args))
        ((step) (run-cycle))
        ((run) (run))
        ((stack) data-stack)
        ((memory) (if (null? args) 
                      memory 
                      (memory-read memory (car args))))
        ((write-memory) (memory-write! memory (car args) (cadr args)))
        ((ip) ip)
        ((reset) (set! ip 0) (set! data-stack '()) (set! return-stack '()))
        ((status) (display-state))
        (else (error "Unknown command:" cmd))))))

;;; Example Programs

;; Calculate factorial of 5
(define factorial-program
  (list
   ;; Initialize
   OP-PUSH 5    ; Push initial value (5)
   OP-PUSH 1    ; Push accumulator (1)
   
   ;; Main loop (starts at address 4)
   OP-SWAP      ; Swap n and accumulator
   OP-OVER      ; Duplicate n
   OP-JZ 16     ; Jump to end if n is 0
   OP-SWAP      ; Swap back
   OP-OVER      ; Duplicate accumulator
   OP-MUL 0     ; Multiply n * accumulator
   OP-SWAP      ; Swap result and n
   OP-PUSH 1    ; Push 1
   OP-SUB 0     ; Decrement n
   OP-JMP 4     ; Jump back to start of loop
   
   ;; End (address 16)
   OP-DROP 0    ; Drop n (should be 0 now)
   OP-HALT 0    ; Halt
  ))

;; Fibonacci sequence: Calculate the 10th Fibonacci number
(define fibonacci-program
  (list
   ;; Initialize
   OP-PUSH 10   ; Push n (10th number)
   OP-PUSH 0    ; Push first Fibonacci number
   OP-PUSH 1    ; Push second Fibonacci number
   
   ;; Main loop (starts at address 6)
   OP-OVER      ; Duplicate second number
   OP-OVER      ; Duplicate first number
   OP-ADD 0     ; Add first and second
   OP-SWAP      ; Swap first and sum (new second)
   OP-DROP 0    ; Drop old first
   
   ;; Decrement counter
   OP-SWAP      ; Swap n and second
   OP-PUSH 1    ; Push 1
   OP-SUB 0     ; Decrement n
   OP-DUP 0     ; Duplicate n
   OP-PUSH 1    ; Push 1
   OP-EQ 0      ; Compare n to 1
   OP-JZ 6      ; If n != 1, loop again
   
   ;; End (address 20)
   OP-DROP 0    ; Drop n
   OP-SWAP      ; Swap to get result on top
   OP-HALT 0    ; Halt
  ))

;; Example usage
(define (factorial-demo)
  (let ((machine (make-stack-machine 100)))
    
    (machine 'load factorial-program)
    (display "Initial state:\n")
    (machine 'status)
    
    (machine 'run)
    
    (display "\nFinal state:\n")
    (machine 'status)
    
    (display "\nResult: ")
    (display (car (machine 'stack)))
    (newline)))

(define (fibonacci-demo)
  (let ((machine (make-stack-machine 100)))
    
    (machine 'load fibonacci-program)
    (display "Initial state:\n")
    (machine 'status)
    
    (machine 'run)
    
    (display "\nFinal state:\n")
    (machine 'status)
    
    (display "\nResult: ")
    (display (car (machine 'stack)))
    (newline)))

;; Run demos if this file is executed directly
(display "Stack-Based Computer Simulator Demos")
(newline)
(display "================================")
(newline)

(display "\nFactorial of 5:\n")
(factorial-demo)

(display "\nFibonacci (10th number):\n")
(fibonacci-demo)