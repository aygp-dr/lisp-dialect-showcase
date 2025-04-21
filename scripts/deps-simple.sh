#!/bin/sh
# Simple installation of all Lisp dialects using pkg
sudo pkg install -y \
  lang/sbcl \
  lang/clojure \
  devel/leiningen \
  lang/chicken \
  lang/guile \
  editors/emacs \
  lang/racket \
  lang/python \
  devel/py-pip \
  lang/lua \
  devel/luarocks \
  lang/janet

# Install Hy (Python-based Lisp)
sudo pip install hy

# Install Fennel (Lua-based Lisp)
sudo luarocks install fennel

# Download Quicklisp installer
curl -o ~/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
echo "To install Quicklisp, run: sbcl --load ~/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --eval '(quit)'"
