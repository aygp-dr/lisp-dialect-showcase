#!/bin/sh
# Robust installation script for Lisp dialects on FreeBSD
# Handles missing packages by using ports or direct installation

set -e  # Exit on error

echo "====== FreeBSD Lisp Dialects Installer ======"
echo "System: $(uname -a)"
echo "Date: $(date)"
echo "================================================="

# Ensure pkg is up to date
echo "Updating package database..."
pkg update

# Install common tools
echo "Installing common tools..."
pkg install -y git curl gmake gcc autoconf automake pkgconf wget

# Create directories
mkdir -p ~/lisp-install
cd ~/lisp-install

# Common Lisp (SBCL)
echo "Installing Common Lisp (SBCL)..."
pkg install -y lang/sbcl || {
    echo "Installing SBCL from ports..."
    if [ ! -d "/usr/ports/lang/sbcl" ]; then
        portsnap fetch extract
    fi
    cd /usr/ports/lang/sbcl
    make install clean
}

# Install Quicklisp
echo "Installing Quicklisp..."
curl -o ~/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
sbcl --load ~/quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql:add-to-init-file)' \
     --eval '(quit)' || {
    echo "Quicklisp installation failed. Manual installation may be required."
    echo "You can install it by running: sbcl --load ~/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --eval '(quit)'"
}

# Clojure
echo "Installing Clojure..."
pkg install -y lang/clojure devel/leiningen || {
    echo "Installing Clojure using alternative method..."
    cd ~/lisp-install
    
    # Install Clojure CLI tools
    curl -O https://download.clojure.org/install/linux-install-1.11.1.1347.sh
    chmod +x linux-install-1.11.1.1347.sh
    ./linux-install-1.11.1.1347.sh || {
        echo "Clojure installation failed. Please install manually."
    }
    
    # Install Leiningen manually
    curl -o ~/bin/lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
    chmod +x ~/bin/lein
}

# Scheme (Guile and Chicken)
echo "Installing Scheme..."
pkg install -y lang/guile lang/chicken || {
    echo "Installing Scheme from ports..."
    if [ ! -d "/usr/ports/lang/guile" ]; then
        portsnap fetch extract
    fi
    cd /usr/ports/lang/guile
    make install clean || echo "Guile installation failed."
    
    cd /usr/ports/lang/chicken
    make install clean || echo "Chicken installation failed."
}

# Emacs
echo "Installing Emacs..."
pkg install -y editors/emacs || {
    echo "Installing Emacs from ports..."
    if [ ! -d "/usr/ports/editors/emacs" ]; then
        portsnap fetch extract
    fi
    cd /usr/ports/editors/emacs
    make install clean
}

# Racket
echo "Installing Racket..."
pkg install -y lang/racket || {
    echo "Racket not available in packages, installing manually..."
    cd ~/lisp-install
    wget https://download.racket-lang.org/installers/8.12/racket-8.12-x86_64-linux-cs.sh
    chmod +x racket-8.12-x86_64-linux-cs.sh
    ./racket-8.12-x86_64-linux-cs.sh --unix-style --dest /usr/local || {
        echo "Racket manual installation failed."
        echo "Please download and install Racket from https://download.racket-lang.org/"
    }
}

# Hy (Python-based Lisp)
echo "Installing Hy..."
pkg install -y lang/python devel/py-pip
pip install --upgrade pip setuptools
pip install hy || {
    echo "Hy installation via pip failed, trying alternative method..."
    cd ~/lisp-install
    git clone https://github.com/hylang/hy.git
    cd hy
    pip install -e .
}

# Lua and Fennel
echo "Installing Lua and Fennel..."
pkg install -y lang/lua devel/luarocks || {
    echo "Lua/Luarocks not available in packages, installing from ports..."
    if [ ! -d "/usr/ports/lang/lua" ]; then
        portsnap fetch extract
    fi
    cd /usr/ports/lang/lua54
    make install clean || echo "Lua installation failed."
    
    # Install LuaRocks manually
    cd ~/lisp-install
    git clone https://github.com/luarocks/luarocks.git
    cd luarocks
    ./configure --with-lua-include=/usr/local/include/lua54
    make
    make install
}

# Install Fennel
luarocks install fennel || {
    echo "Fennel installation via LuaRocks failed, installing manually..."
    cd ~/lisp-install
    git clone https://github.com/bakpakin/Fennel.git
    cd Fennel
    make
    cp fennel /usr/local/bin/
}

# Janet
echo "Installing Janet..."
pkg install -y lang/janet || {
    echo "Janet not available in packages, installing from source..."
    cd ~/lisp-install
    git clone https://github.com/janet-lang/janet.git
    cd janet
    make
    make install || {
        echo "Janet manual installation failed."
    }

    # Create symlinks
    JANET_LIB=$(ls /usr/local/lib/libjanet.so.* 2>/dev/null | head -1)
    if [ -n "$JANET_LIB" ]; then
        JANET_VERSION=$(basename $JANET_LIB | sed 's/libjanet.so.//')
        ln -sf $JANET_LIB /usr/local/lib/libjanet.so
        mkdir -p /usr/local/include/janet
        
        # Find the correct header file
        JANET_HEADER=$(ls /usr/local/include/janet/janet_*.h 2>/dev/null | head -1)
        if [ -n "$JANET_HEADER" ]; then
            ln -sf $JANET_HEADER /usr/local/include/janet.h
            ln -sf $(basename $JANET_HEADER) /usr/local/include/janet/janet.h
            echo "Janet symlinks created successfully for version $JANET_VERSION."
        fi
    fi
}

echo "================================================="
echo "Installation completed!"
echo "Some manual steps might be required if any of the installations failed."
echo "================================================="

# Verify installations
echo "Checking installed versions:"
echo "Common Lisp: $(sbcl --version 2>/dev/null || echo 'Not installed properly')"
echo "Clojure: $(clojure -e '(println (str "Clojure " (clojure-version)))' 2>/dev/null || echo 'Not installed properly')"
echo "Guile: $(guile --version 2>/dev/null | head -1 || echo 'Not installed properly')"
echo "Chicken: $(csi -version 2>/dev/null || echo 'Not installed properly')"
echo "Emacs: $(emacs --version 2>/dev/null | head -1 || echo 'Not installed properly')"
echo "Racket: $(racket --version 2>/dev/null || echo 'Not installed properly')"
echo "Python: $(python --version 2>/dev/null || echo 'Not installed properly')"
echo "Hy: $(hy --version 2>/dev/null || echo 'Not installed properly')"
echo "Lua: $(lua -v 2>/dev/null || echo 'Not installed properly')"
echo "Fennel: $(fennel --version 2>/dev/null || echo 'Not installed properly')"
echo "Janet: $(janet -v 2>/dev/null || echo 'Not installed properly')"
