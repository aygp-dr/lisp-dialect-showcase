;; Conway's Game of Life Implementation
;; Implementation-agnostic Scheme code for cellular automaton

;;; Grid implementation
;; We represent the grid as a vector of vectors for efficient access

;; Create a new grid with the specified dimensions
(define (make-grid width height)
  (let ((grid (make-vector height)))
    (do ((i 0 (+ i 1)))
        ((= i height) grid)
      (vector-set! grid i (make-vector width #f)))))

;; Get the state of a cell in the grid
(define (grid-ref grid x y)
  (let ((width (vector-length (vector-ref grid 0)))
        (height (vector-length grid)))
    (if (and (>= x 0) (< x width) (>= y 0) (< y height))
        (vector-ref (vector-ref grid y) x)
        #f)))  ; Out of bounds is treated as dead

;; Set the state of a cell in the grid
(define (grid-set! grid x y state)
  (let ((width (vector-length (vector-ref grid 0)))
        (height (vector-length grid)))
    (if (and (>= x 0) (< x width) (>= y 0) (< y height))
        (vector-set! (vector-ref grid y) x state))))

;; Count living neighbors around a cell
(define (count-neighbors grid x y)
  (let ((count 0))
    (do ((dx -1 (+ dx 1)))
        ((> dx 1) count)
      (do ((dy -1 (+ dy 1)))
          ((> dy 1))
        (if (and (not (and (= dx 0) (= dy 0)))  ; Skip the cell itself
                 (grid-ref grid (+ x dx) (+ y dy)))
            (set! count (+ count 1)))))))

;; Create a deep copy of a grid
(define (copy-grid grid)
  (let* ((height (vector-length grid))
         (width (vector-length (vector-ref grid 0)))
         (new-grid (make-grid width height)))
    (do ((y 0 (+ y 1)))
        ((= y height) new-grid)
      (do ((x 0 (+ x 1)))
          ((= x width))
        (grid-set! new-grid x y (grid-ref grid x y))))))

;; Display the grid as ASCII art
(define (display-grid grid)
  (let ((height (vector-length grid))
        (width (vector-length (vector-ref grid 0))))
    (display "+")
    (do ((i 0 (+ i 1)))
        ((= i width))
      (display "-"))
    (display "+\n")
    
    (do ((y 0 (+ y 1)))
        ((= y height))
      (display "|")
      (do ((x 0 (+ x 1)))
          ((= x width))
        (display (if (grid-ref grid x y) "█" " ")))
      (display "|\n"))
    
    (display "+")
    (do ((i 0 (+ i 1)))
        ((= i width))
      (display "-"))
    (display "+\n")))

;;; Game of Life logic

;; Create a new Game of Life instance
(define (make-life width height)
  (let ((grid (make-grid width height))
        (generation 0))
    
    ;; Compute the next generation
    (define (step)
      (let ((new-grid (copy-grid grid)))
        (do ((y 0 (+ y 1)))
            ((= y height))
          (do ((x 0 (+ x 1)))
              ((= x width))
            (let ((neighbors (count-neighbors grid x y))
                  (alive? (grid-ref grid x y)))
              (cond
                ;; Rule 1: Any live cell with fewer than two live neighbors dies (underpopulation)
                ((and alive? (< neighbors 2))
                 (grid-set! new-grid x y #f))
                ;; Rule 2: Any live cell with two or three live neighbors lives on
                ((and alive? (or (= neighbors 2) (= neighbors 3)))
                 (grid-set! new-grid x y #t))
                ;; Rule 3: Any live cell with more than three live neighbors dies (overpopulation)
                ((and alive? (> neighbors 3))
                 (grid-set! new-grid x y #f))
                ;; Rule 4: Any dead cell with exactly three live neighbors becomes alive (reproduction)
                ((and (not alive?) (= neighbors 3))
                 (grid-set! new-grid x y #t))))))
        
        (set! grid new-grid)
        (set! generation (+ generation 1))))
    
    ;; Set a cell to alive or dead
    (define (set-cell x y state)
      (grid-set! grid x y state))
    
    ;; Set up a predefined pattern at the specified position
    (define (set-pattern pattern x y)
      (case pattern
        ((glider)
         (set-cell (+ x 1) y #t)
         (set-cell (+ x 2) (+ y 1) #t)
         (set-cell x (+ y 2) #t)
         (set-cell (+ x 1) (+ y 2) #t)
         (set-cell (+ x 2) (+ y 2) #t))
        
        ((blinker)
         (set-cell x y #t)
         (set-cell (+ x 1) y #t)
         (set-cell (+ x 2) y #t))
        
        ((block)
         (set-cell x y #t)
         (set-cell (+ x 1) y #t)
         (set-cell x (+ y 1) #t)
         (set-cell (+ x 1) (+ y 1) #t))
        
        ((beehive)
         (set-cell (+ x 1) y #t)
         (set-cell (+ x 2) y #t)
         (set-cell x (+ y 1) #t)
         (set-cell (+ x 3) (+ y 1) #t)
         (set-cell (+ x 1) (+ y 2) #t)
         (set-cell (+ x 2) (+ y 2) #t))
        
        ((pulsar)
         ;; Top region
         (do ((i -2 (+ i 1)))
             ((> i 2))
           (set-cell (+ x i) (- y 4) #t)
           (set-cell (+ x i) (- y 10) #t))
         
         ;; Left and right sides
         (do ((i -2 (+ i 1)))
             ((> i 2))
           (set-cell (- x 4) (+ y i) #t)
           (set-cell (- x 10) (+ y i) #t)
           (set-cell (+ x 4) (+ y i) #t)
           (set-cell (+ x 10) (+ y i) #t))
         
         ;; Bottom region
         (do ((i -2 (+ i 1)))
             ((> i 2))
           (set-cell (+ x i) (+ y 4) #t)
           (set-cell (+ x i) (+ y 10) #t)))
        
        ((random)
         (do ((i 0 (+ i 1)))
             ((= i height))
           (do ((j 0 (+ j 1)))
               ((= j width))
             (set-cell j i (zero? (random 3))))))
        
        (else (error "Unknown pattern:" pattern))))
    
    ;; Clear the grid
    (define (clear)
      (do ((y 0 (+ y 1)))
          ((= y height))
        (do ((x 0 (+ x 1)))
            ((= x width))
          (set-cell x y #f)))
      (set! generation 0))
    
    ;; Count living cells
    (define (count-living)
      (let ((count 0))
        (do ((y 0 (+ y 1)))
            ((= y height) count)
          (do ((x 0 (+ x 1)))
              ((= x width))
            (if (grid-ref grid x y)
                (set! count (+ count 1)))))))
    
    ;; Return interface functions
    (lambda (op . args)
      (case op
        ((step) (step))
        
        ((run)
         (let ((steps (if (null? args) 1 (car args))))
           (do ((i 0 (+ i 1)))
               ((= i steps))
             (step))))
        
        ((set) (apply set-cell args))
        
        ((get) (apply grid-ref (cons grid args)))
        
        ((pattern) (apply set-pattern args))
        
        ((clear) (clear))
        
        ((display) (display-grid grid))
        
        ((generation) generation)
        
        ((dimensions) (cons width height))
        
        ((living) (count-living))
        
        ((state) (list 'grid grid 'generation generation))
        
        (else (error "Unknown operation:" op))))))

;;; Example Patterns and Demonstrations

;; Demonstrate a glider pattern
(define (demo-glider)
  (let ((life (make-life 20 10)))
    (display "Game of Life: Glider Pattern\n")
    (display "============================\n")
    
    ;; Set up a glider in the top-left
    (life 'pattern 'glider 1 1)
    
    (display "Initial state (Generation 0):\n")
    (life 'display)
    
    ;; Run 5 generations
    (do ((i 1 (+ i 1)))
        ((> i 5))
      (life 'step)
      (display "\nGeneration ")
      (display i)
      (display ":\n")
      (life 'display))))

;; Demonstrate an oscillator (blinker)
(define (demo-blinker)
  (let ((life (make-life 10 10)))
    (display "\nGame of Life: Blinker Pattern (Oscillator)\n")
    (display "========================================\n")
    
    ;; Set up a blinker in the center
    (life 'pattern 'blinker 4 5)
    
    (display "Initial state (Generation 0):\n")
    (life 'display)
    
    ;; Run 4 generations to show the cycle
    (do ((i 1 (+ i 1)))
        ((> i 4))
      (life 'step)
      (display "\nGeneration ")
      (display i)
      (display ":\n")
      (life 'display))))

;; Demonstrate a still life pattern (block)
(define (demo-block)
  (let ((life (make-life 10 10)))
    (display "\nGame of Life: Block Pattern (Still Life)\n")
    (display "=======================================\n")
    
    ;; Set up a block in the center
    (life 'pattern 'block 4 4)
    
    (display "Initial state (Generation 0):\n")
    (life 'display)
    
    ;; Run 3 generations to show stability
    (do ((i 1 (+ i 1)))
        ((> i 3))
      (life 'step)
      (display "\nGeneration ")
      (display i)
      (display ":\n")
      (life 'display))))

;; Run all demos
(define (life-demo)
  (display "Conway's Game of Life in Scheme\n")
  (display "===============================\n")
  
  (demo-glider)
  (demo-blinker)
  (demo-block)
  
  (display "\nGame of Life demonstrations complete.\n"))

;; Run the demo if this file is executed directly
(life-demo)