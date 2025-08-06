#!/usr/bin/env bash
# Tmux session management for Lisp Dialect Showcase

set -euo pipefail

# Load environment variables
if [ -f .envrc ]; then
  # shellcheck source=/dev/null
  source .envrc
fi

PROJECT_NAME="${PROJECT_NAME:-lisp-dialect-showcase}"
PROJECT_ROOT="${PROJECT_ROOT:-$(pwd)}"
TMUX_SESSION_NAME="${TMUX_SESSION_NAME:-$PROJECT_NAME}"
EMACS_PROJECT_FILE="${EMACS_PROJECT_FILE:-$PROJECT_ROOT/$PROJECT_NAME.el}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
  cat << EOF
Usage: $0 [COMMAND]

Commands:
  start    Start a new tmux session with project-specific Emacs
  stop     Stop the tmux session
  attach   Attach to existing tmux session
  status   Show tmux session status and TTY
  restart  Stop and start the tmux session
  help     Show this help message

Environment:
  PROJECT_NAME:     $PROJECT_NAME
  PROJECT_ROOT:     $PROJECT_ROOT
  TMUX_SESSION:     $TMUX_SESSION_NAME
  EMACS_CONFIG:     $EMACS_PROJECT_FILE

EOF
}

start_session() {
  if tmux has-session -t "$TMUX_SESSION_NAME" 2>/dev/null; then
    echo -e "${YELLOW}Session '$TMUX_SESSION_NAME' already exists${NC}"
    echo "Use '$0 attach' to connect or '$0 restart' to restart"
    show_session_info
    return 1
  fi
  
  echo -e "${GREEN}Starting tmux session '$TMUX_SESSION_NAME'...${NC}"
  
  # Create tmux session with Emacs
  tmux new-session -d -s "$TMUX_SESSION_NAME" \
    -c "$PROJECT_ROOT" \
    "emacs -nw -Q -l $EMACS_PROJECT_FILE"
  
  # Create additional windows for different purposes
  tmux new-window -t "$TMUX_SESSION_NAME:2" -n "REPL" -c "$PROJECT_ROOT"
  tmux new-window -t "$TMUX_SESSION_NAME:3" -n "Shell" -c "$PROJECT_ROOT"
  
  # Set up panes in REPL window for different Lisp REPLs
  tmux select-window -t "$TMUX_SESSION_NAME:2"
  tmux split-window -h -c "$PROJECT_ROOT"
  tmux split-window -v -c "$PROJECT_ROOT"
  
  # Return to first window (Emacs)
  tmux select-window -t "$TMUX_SESSION_NAME:1"
  
  echo -e "${GREEN}Session started successfully!${NC}"
  show_session_info
  echo ""
  echo "Quick start commands:"
  echo "  - Attach to session: tmux attach -t $TMUX_SESSION_NAME"
  echo "  - Start Clojure REPL: lein repl (in window 2)"
  echo "  - Start Scheme REPL: guile (in window 2)"
  echo "  - Start Common Lisp: sbcl (in window 2)"
}

stop_session() {
  if ! tmux has-session -t "$TMUX_SESSION_NAME" 2>/dev/null; then
    echo -e "${YELLOW}No session named '$TMUX_SESSION_NAME' found${NC}"
    return 1
  fi
  
  echo -e "${YELLOW}Stopping tmux session '$TMUX_SESSION_NAME'...${NC}"
  tmux kill-session -t "$TMUX_SESSION_NAME"
  echo -e "${GREEN}Session stopped${NC}"
}

attach_session() {
  if ! tmux has-session -t "$TMUX_SESSION_NAME" 2>/dev/null; then
    echo -e "${RED}No session named '$TMUX_SESSION_NAME' found${NC}"
    echo "Use '$0 start' to create a new session"
    return 1
  fi
  
  echo -e "${GREEN}Attaching to session '$TMUX_SESSION_NAME'...${NC}"
  tmux attach-session -t "$TMUX_SESSION_NAME"
}

show_session_info() {
  if ! tmux has-session -t "$TMUX_SESSION_NAME" 2>/dev/null; then
    echo -e "${RED}No session named '$TMUX_SESSION_NAME' found${NC}"
    return 1
  fi
  
  echo ""
  echo "Session Information:"
  echo "===================="
  echo "Session name: $TMUX_SESSION_NAME"
  echo "Windows:"
  tmux list-windows -t "$TMUX_SESSION_NAME" | sed 's/^/  /'
  echo ""
  echo "Pane TTYs:"
  tmux list-panes -t "$TMUX_SESSION_NAME" -F "  Window #{window_index}.#{pane_index}: #{pane_tty}"
}

restart_session() {
  echo -e "${YELLOW}Restarting session '$TMUX_SESSION_NAME'...${NC}"
  stop_session 2>/dev/null || true
  sleep 1
  start_session
}

# Main command dispatcher
case "${1:-help}" in
  start)
    start_session
    ;;
  stop)
    stop_session
    ;;
  attach)
    attach_session
    ;;
  status)
    show_session_info
    ;;
  restart)
    restart_session
    ;;
  help|--help|-h)
    usage
    ;;
  *)
    echo -e "${RED}Unknown command: $1${NC}"
    usage
    exit 1
    ;;
esac