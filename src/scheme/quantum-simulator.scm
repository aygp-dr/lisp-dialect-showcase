;; Quantum Computing Simulator in Scheme
;; Implementation-agnostic version
;; Designed to work with various Scheme implementations

;;; Complex number representation and operations
;; We use pairs for complex numbers: (real-part . imag-part)
;; These can be replaced with implementation-specific versions via wrappers

;; Basic complex number constructors and accessors
(define (make-complex real imag)
  (cons real imag))

(define (real-part z)
  (car z))

(define (imag-part z)
  (cdr z))

;; Complex arithmetic operations
(define (complex-add z1 z2)
  (make-complex
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))

(define (complex-multiply z1 z2)
  (make-complex
    (- (* (real-part z1) (real-part z2))
       (* (imag-part z1) (imag-part z2)))
    (+ (* (real-part z1) (imag-part z2))
       (* (imag-part z1) (real-part z2)))))

(define (complex-abs-squared z)
  (+ (* (real-part z) (real-part z))
     (* (imag-part z) (imag-part z))))

(define (complex-conjugate z)
  (make-complex (real-part z) (- (imag-part z))))

;;; Quantum state representations
;; Represent |0⟩ as [1, 0] and |1⟩ as [0, 1]
(define qubit-0
  (list (make-complex 1 0) (make-complex 0 0)))

(define qubit-1
  (list (make-complex 0 0) (make-complex 1 0)))

;; Create a new qubit in state |0⟩
(define (new-qubit)
  (list (make-complex 1 0) (make-complex 0 0)))

;;; Quantum gates as matrices

;; X (NOT) gate: [[0, 1], [1, 0]]
(define (apply-x qubit)
  (list (list-ref qubit 1) (list-ref qubit 0)))

;; H (Hadamard) gate: 1/√2 * [[1, 1], [1, -1]]
(define (apply-h qubit)
  (let ((inv-sqrt2 (make-complex (/ 1 (sqrt 2)) 0)))
    (list (complex-add (complex-multiply inv-sqrt2 (list-ref qubit 0))
                       (complex-multiply inv-sqrt2 (list-ref qubit 1)))
          (complex-add (complex-multiply inv-sqrt2 (list-ref qubit 0))
                       (complex-multiply 
                         (make-complex (- (/ 1 (sqrt 2))) 0) 
                         (list-ref qubit 1))))))

;; Z gate: [[1, 0], [0, -1]]
(define (apply-z qubit)
  (list (list-ref qubit 0)
        (complex-multiply (make-complex -1 0) (list-ref qubit 1))))

;; Random number generator - must be provided by a wrapper
;; Should return a real number between 0 and 1
(define (random-uniform)
  ;; Placeholder - replace with implementation-specific version
  0.5)

;; Measure a qubit in the standard basis
;; Returns 0 or 1 and collapses the qubit
;; This is a destructive operation that modifies the qubit
(define (measure-qubit! qubit)
  (let ((prob-0 (complex-abs-squared (list-ref qubit 0))))
    (if (< (random-uniform) prob-0)
        (begin
          ;; Collapse to |0⟩
          (set-car! qubit (make-complex 1 0))
          (set-car! (cdr qubit) (make-complex 0 0))
          0)
        (begin
          ;; Collapse to |1⟩
          (set-car! qubit (make-complex 0 0))
          (set-car! (cdr qubit) (make-complex 1 0))
          1))))

;; Non-destructive version of measurement
;; Returns only the measurement outcome without modifying the qubit
(define (measure-qubit-probability qubit)
  (complex-abs-squared (list-ref qubit 0)))

;; Function to get string representation of a qubit for display
(define (qubit->string qubit)
  (let ((alpha (list-ref qubit 0))
        (beta (list-ref qubit 1)))
    (string-append
      "(" (number->string (real-part alpha))
      " + " (number->string (imag-part alpha)) "i)|0⟩ + ("
      (number->string (real-part beta))
      " + " (number->string (imag-part beta)) "i)|1⟩")))

;; Helper to run multiple measurements and get statistics
;; This is a non-destructive operation that preserves the qubit state
(define (measure-many qubit n)
  (let loop ((i 0) (zeros 0))
    (if (>= i n)
        (/ zeros n)
        (let ((q-copy (list (car qubit) (cadr qubit))))
          (let ((result (measure-qubit! q-copy)))
            (loop (+ i 1) (if (= result 0) (+ zeros 1) zeros)))))))

;;; Example quantum algorithms

;; Quantum coin flip using Hadamard gate
(define (demonstrate-coin-flip)
  (let ((q (new-qubit)))
    (display "Initial state: ")
    (display (qubit->string q))
    (newline)
    
    (display "Applying H gate (creating superposition): ")
    (let ((q2 (apply-h q)))
      (display (qubit->string q2))
      (newline)
      
      (display "Probability of |0⟩: ")
      (display (measure-qubit-probability q2))
      (newline)
      
      (display "Measuring the qubit: ")
      (let ((result (measure-qubit! q2)))
        (display "Result: ")
        (display result)
        (newline)
        
        (display "State after measurement: ")
        (display (qubit->string q2))
        (newline)
        
        (display "Theoretical probability of measuring |0⟩ over many trials: 0.5")
        (newline)
        
        (display "Simulated probability of |0⟩ over 1000 measurements: ")
        (display (measure-many (apply-h (new-qubit)) 1000))
        (newline)))))

;; Bell state (entanglement) conceptual demonstration
(define (demonstrate-bell-state)
  (display "\nCreating Bell State (|00⟩ + |11⟩)/√2:")
  (newline)
  (display "1. Start with two qubits in |0⟩:")
  (newline)
  (display "   |q1⟩ = |0⟩, |q2⟩ = |0⟩")
  (newline)
  (display "2. Apply H gate to first qubit:")
  (newline)
  (display "   |q1⟩ = (|0⟩ + |1⟩)/√2, |q2⟩ = |0⟩")
  (newline)
  (display "3. Apply CNOT gate (conceptually):")
  (newline)
  (display "   Final state = (|00⟩ + |11⟩)/√2")
  (newline)
  (display "This is a maximally entangled state!")
  (newline))

;; Grover's search algorithm conceptual demonstration
(define (demonstrate-grovers-search)
  (display "\nGrover's Search Algorithm (conceptual, 2 qubits):")
  (newline)
  (display "1. Start with |00⟩")
  (newline)
  (display "2. Apply H to both qubits: (|00⟩ + |01⟩ + |10⟩ + |11⟩)/2")
  (newline)
  (display "3. Oracle marks solution |10⟩")
  (newline)
  (display "4. Amplitude amplification increases probability of |10⟩")
  (newline)
  (display "5. Measure and get solution |10⟩ with high probability")
  (newline))

;; Main entry point
(define (quantum-simulator-demo)
  (display "Quantum Computing Simulator in Scheme")
  (newline)
  (display "========================================")
  (newline)
  (demonstrate-coin-flip)
  (demonstrate-bell-state)
  (demonstrate-grovers-search))

;; Run the demonstration if this file is executed directly
(quantum-simulator-demo)