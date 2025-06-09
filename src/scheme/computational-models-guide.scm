;; Computational Models Integration Guide
;; A framework for unifying different computational models in Scheme

;;; Generic Computational Model Interface

;; Factory function to create computational models of different types
(define (make-computation-model type . options)
  (case type
    ((quantum) (apply make-quantum-computer options))
    ((logic) (apply make-virtual-computer options))
    ((stack) (apply make-stack-machine options))
    ((cellular) (apply make-life-computer options))
    ((turing) (apply make-turing-machine options))
    (else (error "Unknown computation model type:" type))))

;;; Model Implementations

;; Quantum Computer Interface Wrapper
;; Assumes quantum-simulator.scm has been loaded
(define (make-quantum-computer)
  (let ((qubit (new-qubit))
        (measurements '()))
    
    ;; Interface implementation
    (lambda (op . args)
      (case op
        ((initialize)
         (set! qubit (new-qubit))
         (set! measurements '()))
        
        ((step)
         (let ((gate (if (null? args) 'h (car args))))
           (case gate
             ((x) (set! qubit (apply-x qubit)))
             ((h) (set! qubit (apply-h qubit)))
             ((z) (set! qubit (apply-z qubit)))
             ((measure) 
              (let ((result (measure-qubit! qubit)))
                (set! measurements (cons result measurements))
                result))
             (else (error "Unknown quantum gate:" gate)))))
        
        ((run)
         (let ((steps (if (null? args) 1 (car args)))
               (gate (if (or (null? args) (null? (cdr args))) 'h (cadr args))))
           (do ((i 0 (+ i 1)))
               ((>= i steps))
             (if (eq? gate 'measure)
                 ((lambda () (display "Measurement result: ") 
                           (display ((self 'step) 'measure))
                           (newline)))
                 ((lambda () ((self 'step) gate)))))))
        
        ((state)
         (list 'qubit qubit 'measurements measurements))
        
        ((reset)
         ((self 'initialize)))
        
        (else (error "Unknown operation:" op))))))

;; Logic Computer Interface Wrapper
;; Assumes logic-gates.scm is loaded
(define (make-virtual-computer . args)
  (let ((memory-size (if (null? args) 256 (car args)))
        (memory (make-vector 256 #f))
        (registers (make-vector 8 #f)))
    
    ;; Define the interface function
    (define (self op . args)
      (case op
        ((initialize)
         (vector-fill! memory #f)
         (vector-fill! registers #f))
        
        ((step)
         (let ((instr (if (null? args) 'noop (car args))))
           (case instr
             ((and) (logic-and))
             ((or) (logic-or))
             ((not) (logic-not))
             ((add) (adder))
             (else (error "Unknown logic instruction:" instr)))))
        
        ((run)
         (let ((steps (if (null? args) 1 (car args))))
           (do ((i 0 (+ i 1)))
               ((>= i steps))
             ((self 'step)))))
        
        ((state)
         (list 'memory memory 'registers registers))
        
        ((reset)
         ((self 'initialize)))
        
        (else (error "Unknown operation:" op))))
    
    ;; Initialize and return the interface
    (self 'initialize)
    self))

;; Stack Machine Interface Wrapper
;; Assumes stack-machine.scm is loaded
(define (make-stack-machine . args)
  (let ((memory-size (if (null? args) 100 (car args)))
        (machine (make-stack-machine memory-size)))
    
    ;; Interface implementation
    (lambda (op . args)
      (case op
        ((initialize)
         (machine 'reset))
        
        ((step)
         (machine 'step))
        
        ((run)
         (let ((steps (if (null? args) 1 (car args))))
           (if (= steps -1)
               (machine 'run)  ; Run until halted
               (do ((i 0 (+ i 1)))
                   ((>= i steps))
                 (machine 'step)))))
        
        ((state)
         (machine 'status))
        
        ((reset)
         (machine 'reset))
        
        ((load)
         (if (not (null? args))
             (machine 'load (car args))))
        
        (else (error "Unknown operation:" op))))))

;; Cellular Automaton (Game of Life) Interface Wrapper
;; Assumes life.scm is loaded
(define (make-life-computer . args)
  (let* ((width (if (null? args) 20 (car args)))
         (height (if (or (null? args) (null? (cdr args))) 20 (cadr args)))
         (life (make-life width height)))
    
    ;; Interface implementation
    (lambda (op . args)
      (case op
        ((initialize)
         ;; Reset the grid
         (life 'clear))
        
        ((step)
         ;; Compute one generation
         (life 'step))
        
        ((run)
         (let ((steps (if (null? args) 1 (car args))))
           (life 'run steps)))
        
        ((state)
         (life 'state))
        
        ((set-pattern)
         (if (not (null? args))
             (life 'pattern (car args) 
                  (if (null? (cdr args)) 5 (cadr args))
                  (if (or (null? (cdr args)) (null? (cddr args))) 5 (caddr args)))))
        
        ((display)
         (life 'display))
        
        ((reset)
         (life 'clear))
        
        (else (error "Unknown operation:" op))))))

;; Turing Machine Interface Wrapper
;; Assumes turing-machine.scm is loaded
(define (make-turing-machine . args)
  (let* ((initial-tape (if (null? args) '() (car args)))
         (tm (make-turing-machine initial-tape)))
    
    ;; Interface implementation
    (lambda (op . args)
      (case op
        ((initialize)
         ;; Reset the Turing machine
         (tm 'reset))
        
        ((step)
         ;; Execute one Turing Machine step
         (tm 'step))
        
        ((run)
         (let ((steps (if (null? args) 1 (car args))))
           (tm 'run steps)))
        
        ((add-transition)
         (if (not (null? args))
             (apply tm (cons 'add-transition args))))
        
        ((state)
         (tm 'status))
        
        ((display)
         (tm 'display))
        
        ((reset)
         (tm 'reset))
        
        (else (error "Unknown operation:" op))))))

;;; Demonstration Functions

;; Demonstrate the quantum computer model
(define (demo-quantum)
  (let ((qc (make-computation-model 'quantum)))
    (display "Quantum Computer Demonstration\n")
    (display "==============================\n")
    
    (display "Initial state: ")
    (display (qc 'state))
    (newline)
    
    (display "Applying H gate: ")
    (qc 'step 'h)
    (display (qc 'state))
    (newline)
    
    (display "Measuring: ")
    (qc 'step 'measure)
    (display (qc 'state))
    (newline)))

;; Demonstrate the stack machine model
(define (demo-stack)
  (let ((sm (make-computation-model 'stack 100)))
    (display "Stack Machine Demonstration\n")
    (display "===========================\n")
    
    ;; Load the factorial program defined in stack-machine.scm
    (sm 'load factorial-program)
    
    (display "Initial state:\n")
    (sm 'state)
    (newline)
    
    (display "Running program...\n")
    (sm 'run -1)  ; Run until halted
    
    (display "Final state:\n")
    (sm 'state)
    (newline)))

;; Demonstrate the cellular automaton (Game of Life) model
(define (demo-life)
  (let ((life (make-computation-model 'cellular 20 10)))
    (display "Cellular Automaton (Game of Life) Demonstration\n")
    (display "============================================\n")
    
    ;; Set up a glider pattern
    (life 'set-pattern 'glider 2 2)
    
    (display "Initial state:\n")
    (life 'display)
    (newline)
    
    (display "After 5 generations:\n")
    (life 'run 5)
    (life 'display)
    (newline)))

;; Demonstrate the Turing machine model
(define (demo-turing)
  (let ((tm (make-computation-model 'turing)))
    (display "Turing Machine Demonstration\n")
    (display "===========================\n")
    
    ;; Set up a binary incrementer
    (tm 'add-transition 'q0 0 'q0 0 'right)
    (tm 'add-transition 'q0 1 'q0 1 'right)
    (tm 'add-transition 'q0 #f 'q1 #f 'left)
    (tm 'add-transition 'q1 0 'qaccept 1 'stay)
    (tm 'add-transition 'q1 1 'q1 0 'left)
    (tm 'add-transition 'q1 #f 'qaccept 1 'stay)
    
    ;; Display initial state
    (display "Initial state (before setting tape):\n")
    (tm 'display)
    (newline)
    
    ;; Run the machine
    (display "Running the binary incrementer...\n")
    (tm 'run)
    
    ;; Display final state
    (display "Final state:\n")
    (tm 'display)
    (newline)))

;; Run all model demonstrations
(define (run-computational-models-demo)
  (display "Computational Models Integration Framework\n")
  (display "========================================\n\n")
  
  (demo-quantum)
  (newline)
  (demo-stack)
  (newline)
  (demo-life)
  (newline)
  (demo-turing)
  (newline)
  
  (display "Integration framework demonstration complete.\n"))

;; The demos would be run if this file is loaded directly
;; Note: This requires the model implementations to be loaded first