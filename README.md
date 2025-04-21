# Lisp Dialect Showcase

A comparative showcase of different Lisp dialects implementing common programming problems.

## Overview

This repository demonstrates the similarities and differences between various Lisp dialects by implementing the same set of algorithms in each. It serves as both a learning resource and a reference for programmers interested in the Lisp family of languages.

## Included Dialects

- Common Lisp (SBCL) - A standardized, multi-paradigm Lisp dialect
- Clojure - A modern Lisp for the JVM
- Scheme - A minimalist Lisp dialect
- Emacs Lisp - The extension language for the Emacs editor
- Racket - A Scheme-based language focused on language-oriented programming
- Hy - A Lisp dialect embedded in Python
- Fennel - A Lisp that compiles to Lua
- Janet - A functional and imperative programming language

## Example Problems

Each dialect implements the following problems:

1. Fibonacci sequence
2. Factorial calculation
3. Prime number generation
4. FizzBuzz
5. Quicksort implementation

## Installation

This repository includes a Makefile and scripts for installing all the necessary Lisp dialects.

```
# For FreeBSD systems
make install

# For other systems
sh scripts/deps-simple.sh
```

## Makefile Targets

- `install` - Install all Lisp dialects (FreeBSD)
- `tangle` - Extract code blocks from all .org files
- `detangle` - Update .org files from source files
- `clean` - Remove tangled files and build artifacts
- `run` - Run all examples
- `lint-scripts` - Run shellcheck on all shell scripts
- `help` - Show this help message

## Running Examples

After installation, you can run all examples with:

```
make run
```

Or run examples for a specific dialect:

```
sh scripts/run-common-lisp.sh
sh scripts/run-clojure.sh
sh scripts/run-scheme.sh
sh scripts/run-emacs-lisp.sh
sh scripts/run-racket.sh
sh scripts/run-hy.sh
sh scripts/run-fennel.sh
sh scripts/run-janet.sh
```

## Benchmarks

You can compare the performance of different dialects:

```
sh scripts/benchmark.sh
```

This will benchmark Fibonacci implementations across all dialects.

## Structure

- `src/` - Source code organized by dialect
- `scripts/` - Helper scripts for running examples and benchmarks
- `SETUP.org` - Org-mode file containing all source code with literate programming
- `docs/` - Additional documentation and diagrams

## SETUP.org and Org-mode Integration

The `SETUP.org` file contains all the source code in a literate programming format. You can:

1. Modify code in `SETUP.org` and run `make tangle` to extract it to source files
2. Modify code in source files and run `make detangle` to update `SETUP.org`

This workflow is ideal for Emacs users familiar with org-mode.

## License

MIT