# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **Common Lisp**: `sbcl --load file.lisp`, `sbcl --script file.lisp`, `sbcl --load file.lisp --eval '(package:function)'`
- **Clojure**: `clj -M src/file.clj`, `clojure -e '(expression)'`
- **Scheme**: `guile file.scm`, `chicken-test file.scm`
- **Emacs Lisp**: `emacs --batch --load file.el --eval '(function)'`
- **Racket**: `racket file.rkt`, `raco test file.rkt` 
- **Hy**: `hy file.hy`, `hy -c 'expression'`
- **Fennel**: `fennel file.fnl`, `fennel -e 'expression'`
- **Janet**: `janet file.janet`, `janet -e 'expression'`
- **Run Scripts**: `sh scripts/run-dialect.sh` for each dialect
- **Install**: `make install` (on FreeBSD), `sh scripts/deps-simple.sh` (other systems)

## Code Style Guidelines
- **Formatting**: 2-space indentation across all dialects
- **Naming**: Use kebab-case (hyphenated-names) for functions and variables
- **Function Style**: Write pure functions when possible, minimize side effects
- **Packages/Namespaces**: Use proper namespace/package declarations for each dialect
- **Documentation**: Include docstrings for functions, explain complex algorithms
- **Error Handling**: Use appropriate error handling for each dialect (conditions in CL, exceptions in Clojure)
- **Types**: Include type declarations where applicable (SBCL declarations, Typed Racket)
- **Testing**: Create doctests or test files for new functionality
- **Parentheses**: Balance carefully, use proper indentation to make nesting clear
- **Imports**: Follow each dialect's conventions for imports and module organization