# Computational Models in Scheme

The Lisp Dialect Showcase repository includes implementations of various computational models in Scheme, demonstrating the language's expressiveness and suitability for theoretical computer science concepts.

## Available Models

### 1. Quantum Computing Simulator

A basic quantum computing simulator that includes:
- Complex number representation and operations
- Qubit state manipulation and measurement
- Quantum gates (X, H, Z)
- Demonstrations of quantum algorithms

**File:** `src/scheme/quantum-simulator.scm`

### 2. Stack-Based Computer Simulator

A Forth-like virtual machine with:
- Comprehensive instruction set (25+ operations)
- Memory management with read/write capabilities
- Control flow operations (jumps, conditionals, subroutines)
- Example programs (factorial, Fibonacci)

**File:** `src/scheme/stack-machine.scm`

### 3. Turing Machine Simulator

A universal Turing machine implementation with:
- Expandable tape representation
- State transition function
- Example programs (binary incrementer, palindrome checker, unary adder)
- Visualization capabilities

**File:** `src/scheme/turing-machine.scm`

### 4. Cellular Automaton (Conway's Game of Life)

An implementation of Conway's Game of Life with:
- Grid-based cellular automaton
- Rule implementation for birth, survival, and death
- Predefined patterns (glider, blinker, block, etc.)
- Visualization capabilities

**File:** `src/scheme/life.scm`

## Integration Framework

All models are integrated through a common interface defined in `src/scheme/computational-models-guide.scm`. This framework provides:

- Factory function for model instantiation
- Standardized operations across models: initialize, step, run, state, reset
- Wrapper functions for each computational model
- Demonstration functions

## Running the Demonstrations

You can run the computational models with:

```bash
make computational-models
```

Or directly with the script:

```bash
sh scripts/run-computational-models.sh
```

## Educational Value

These implementations provide:

1. **Concrete Examples:** Abstract computing models as runnable code
2. **Comparative Study:** Direct comparison between computational paradigms
3. **Code Reuse:** Integration of disparate systems through common interfaces
4. **Theoretical Foundation:** Illustration of computer science concepts
5. **Scheme Showcase:** Demonstration of Scheme's expressiveness

## Future Extensions

Planned future enhancements:
1. Visualization improvements
2. Cross-model interaction demonstrations
3. Additional computational models
4. Unit tests for validation
5. Integration with other Lisp dialects