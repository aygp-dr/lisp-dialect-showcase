# Makefile for lisp-dialect-showcase
# Installs and configures all needed Lisp dialects on FreeBSD

.PHONY: all install clean common-lisp clojure scheme emacs-lisp racket hy fennel janet deps run help tangle detangle lint-scripts

# Use gmake on FreeBSD systems, define EMACS to point to the installed Emacs binary
EMACS ?= emacs
ORG_FILES := $(wildcard *.org)
# Extra protection to prevent interactive prompts
EMACS_BATCH := $(EMACS) --batch --no-init-file --no-site-file --no-splash
# Load org-mode and enable necessary babel languages
TANGLE_BATCH := --eval "(require 'org)" --eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (shell . t) (lisp . t) (scheme . t) (clojure . t)))"

# Make help the default target
all: help

help:
	@echo "Makefile for Lisp Dialect Showcase (FreeBSD compatible)"
	@echo ""
	@echo "Targets:"
	@echo "  install     - Install all Lisp dialects"
	@echo "  tangle      - Extract code blocks from all .org files"
	@echo "  detangle    - Update .org files from source files"
	@echo "  clean       - Remove tangled files and build artifacts"
	@echo "  run         - Run all examples"
	@echo "  lint-scripts - Run shellcheck on all shell scripts"
	@echo "  help        - Show this help message"

# Create directories if they don't exist
src:
	mkdir -p src
	
# Process each .org file and tangle its code blocks
tangle: src
	@echo "Tangling Org files..."
	@for file in $(ORG_FILES); do \
		echo "Processing $$file..."; \
		$(EMACS_BATCH) $(TANGLE_BATCH) \
			--eval "(org-babel-tangle-file \"$$file\")" \
			--kill; \
	done
	@echo "Tangling complete."

# Update .org files from their corresponding source files
detangle:
	@echo "Detangling files back to Org sources..."
	@for file in $(ORG_FILES); do \
		echo "Detangling $$file..."; \
		$(EMACS_BATCH) $(TANGLE_BATCH) \
			--eval "(org-babel-detangle \"$$file\")" \
			--kill; \
	done
	@echo "Detangling complete."

install: deps common-lisp clojure scheme emacs-lisp racket hy fennel janet
	@echo "All Lisp dialects installed successfully!"

# Install dependencies
deps:
	@echo "Installing dependencies..."
	@sh scripts/deps.sh

# Common Lisp (SBCL)
common-lisp:
	@echo "Installing Common Lisp (SBCL)..."
	@pkg install -y lang/sbcl
	@pkg install -y devel/quicklisp

# Clojure
clojure:
	@echo "Installing Clojure..."
	@pkg install -y lang/clojure
	@pkg install -y devel/leiningen

# Scheme (Chicken Scheme and Guile)
scheme:
	@echo "Installing Scheme implementations..."
	@pkg install -y lang/chicken
	@pkg install -y lang/guile

# Emacs Lisp
emacs-lisp:
	@echo "Installing Emacs..."
	@pkg install -y editors/emacs

# Racket
racket:
	@echo "Installing Racket..."
	@pkg install -y lang/racket

# Hy (Lisp embedded in Python)
hy:
	@echo "Installing Hy..."
	@pkg install -y lang/python
	@pip install --user hy

# Fennel (Lisp that compiles to Lua)
fennel:
	@echo "Installing Fennel..."
	@pkg install -y lang/lua
	@pkg install -y devel/luarocks
	@luarocks install --local fennel

# Janet
janet:
	@echo "Installing Janet..."
	@pkg install -y lang/janet

# Run all examples
run:
	@echo "Running all examples..."
	@sh scripts/run-common-lisp.sh
	@sh scripts/run-clojure.sh
	@sh scripts/run-scheme.sh
	@sh scripts/run-emacs-lisp.sh
	@sh scripts/run-racket.sh
	@sh scripts/run-hy.sh
	@sh scripts/run-fennel.sh
	@sh scripts/run-janet.sh

# Clean up
clean:
	@echo "Cleaning up..."
	@rm -rf build/
	@find . -type f -path "./src/*" -delete
	@find . -name "*.o" -delete
	@find . -name "*.fasl" -delete

# Lint shell scripts
lint-scripts:
	@echo "Linting shell scripts..."
	@find scripts -name "*.sh" -type f -print0 | xargs -0 shellcheck -e SC2086