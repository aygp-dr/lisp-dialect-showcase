# Makefile for lisp-dialect-showcase
# Installs and configures all needed Lisp dialects on FreeBSD

.PHONY: all install clean common-lisp clojure scheme emacs-lisp racket hy fennel janet deps run computational-models help tangle detangle lint-scripts tmux-start tmux-stop tmux-attach

# Project configuration from environment or defaults
PROJECT_NAME ?= lisp-dialect-showcase
PROJECT_ROOT ?= $(shell pwd)
TMUX_SESSION_NAME ?= $(PROJECT_NAME)
EMACS_PROJECT_FILE ?= $(PROJECT_ROOT)/$(PROJECT_NAME).el

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
	@echo "Project: $(PROJECT_NAME)"
	@echo "Root: $(PROJECT_ROOT)"
	@echo ""
	@echo "Targets:"
	@echo "  install     - Install all Lisp dialects"
	@echo "  tangle      - Extract code blocks from all .org files"
	@echo "  detangle    - Update .org files from source files"
	@echo "  clean       - Remove tangled files and build artifacts"
	@echo "  run         - Run all examples"
	@echo "  computational-models - Run computational models demonstrations"
	@echo "  lint-scripts - Run shellcheck on all shell scripts"
	@echo "  tmux-start  - Start tmux session with project Emacs config"
	@echo "  tmux-attach - Attach to existing tmux session"
	@echo "  tmux-stop   - Stop tmux session"
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

# Run computational models
computational-models:
	@echo "Running computational models demonstrations..."
	@sh scripts/run-computational-models.sh

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

# Tmux session management
tmux-start:
	@echo "Starting tmux session '$(TMUX_SESSION_NAME)' with project Emacs config..."
	@if tmux has-session -t "$(TMUX_SESSION_NAME)" 2>/dev/null; then \
		echo "Session '$(TMUX_SESSION_NAME)' already exists. Use 'make tmux-attach' to connect."; \
	else \
		tmux new-session -d -s "$(TMUX_SESSION_NAME)" "emacs -nw -Q -l $(EMACS_PROJECT_FILE)"; \
		echo "Started tmux session '$(TMUX_SESSION_NAME)'"; \
		echo "Session TTY: $$(tmux list-panes -t $(TMUX_SESSION_NAME) -F '#{pane_tty}')"; \
		echo "Use 'make tmux-attach' to connect to the session"; \
	fi

tmux-attach:
	@echo "Attaching to tmux session '$(TMUX_SESSION_NAME)'..."
	@if tmux has-session -t "$(TMUX_SESSION_NAME)" 2>/dev/null; then \
		tmux attach-session -t "$(TMUX_SESSION_NAME)"; \
	else \
		echo "No session named '$(TMUX_SESSION_NAME)' found. Use 'make tmux-start' to create one."; \
		exit 1; \
	fi

tmux-stop:
	@echo "Stopping tmux session '$(TMUX_SESSION_NAME)'..."
	@if tmux has-session -t "$(TMUX_SESSION_NAME)" 2>/dev/null; then \
		tmux kill-session -t "$(TMUX_SESSION_NAME)"; \
		echo "Stopped tmux session '$(TMUX_SESSION_NAME)'"; \
	else \
		echo "No session named '$(TMUX_SESSION_NAME)' found."; \
	fi