# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **Common Lisp (SBCL)**: `sbcl --load src/common-lisp/file.lisp --eval '(package:function)' --quit`
- **Clojure**: `clj -M src/clojure/file.clj`
- **Scheme**: `guile src/scheme/file.scm`
- **Emacs Lisp**: `emacs --batch --load src/emacs-lisp/file.el --eval '(function)'`
- **Racket**: `racket src/racket/file.rkt`
- **Hy**: `hy src/hy/file.hy`
- **Fennel**: `fennel src/fennel/file.fnl`
- **Janet**: `janet src/janet/file.janet`
- **Benchmark**: `sh scripts/benchmark.sh` (compares performance across dialects)
- **Run All**: `make run` or `sh scripts/run-[dialect].sh` for specific dialect examples
- **Installation**: `make install` (FreeBSD) or `sh scripts/deps-simple.sh` (other systems)
- **Org-mode**: `make tangle` (extract code from SETUP.org), `make detangle` (update SETUP.org from source)
- **Linting**: `make lint-scripts` (run shellcheck on all shell scripts)

## Code Style Guidelines
- **Formatting**: 2-space indentation across all dialects
- **Naming**: Use kebab-case (hyphenated-names) for functions and variables
- **Packages**: Proper namespace declarations (e.g., defpackage in CL, ns in Clojure)
- **Documentation**: Include docstrings for functions, explain complex algorithms
- **Error Handling**: Use appropriate dialect-specific error handling (conditions in CL, exceptions in Clojure)
- **Function Style**: Write pure functions when possible, minimize side effects
- **Implementation**: Provide multiple approaches (recursive, iterative, etc.) for learning purposes
- **Examples**: Include run-examples function for demonstration in each file
- **Parentheses**: Balance carefully, use proper indentation to make nesting clear
- **Testing**: Run dialect-specific tests through associated run scripts