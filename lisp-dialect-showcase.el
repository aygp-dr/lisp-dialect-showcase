;;; lisp-dialect-showcase.el --- Emacs configuration for working with the Lisp dialect showcase

;;; Commentary:
;; This file provides a project-specific configuration for working with
;; the various Lisp dialects in the showcase. Load this file in Emacs
;; to set up all the necessary packages and configurations.
;;
;; Enhanced with CIDER, Geiser, Guile3, Org-mode, TRAMP, and Paredit support
;; for comprehensive Lisp development. Optimized for tmux session integration.

;;; Code:

;; Project configuration from environment
(defvar lisp-project-root
  (or (getenv "LISP_PROJECT_ROOT")
      (expand-file-name default-directory))
  "Root directory of the Lisp Dialect Showcase project.")

(defvar lisp-project-name
  (or (getenv "LISP_PROJECT_NAME")
      "lisp-dialect-showcase")
  "Name of the Lisp Dialect Showcase project.")

;; Add project root to load-path
(add-to-list 'load-path lisp-project-root)

;; Package system setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Enable better syntax highlighting
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Enable parentheses matching
(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook (prog-mode . smartparens-strict-mode))

;; Paredit for enhanced S-expression manipulation
(use-package paredit
  :hook ((lisp-mode scheme-mode clojure-mode emacs-lisp-mode) . paredit-mode))

;; Common configurations for all Lisp dialects
(use-package lisp-mode
  :ensure nil  ; Built-in
  :config
  (add-hook 'lisp-mode-hook (lambda () 
                              (setq indent-tabs-mode nil)
                              (setq tab-width 2)
                              (setq lisp-indent-offset 2))))

;; Common Lisp (SLIME)
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company))
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (setq slime-net-coding-system 'utf-8-unix))

;; Clojure (CIDER)
(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (setq indent-tabs-mode nil)
                                 (setq tab-width 2)
                                 (setq clojure-indent-style :always-align))))

(use-package cider
  :after clojure-mode
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-history-file (expand-file-name ".cider-repl-history" lisp-project-root))
  (setq cider-eval-result-prefix ";; => ")
  (setq cider-font-lock-dynamically '(macro core function var))
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; Scheme (Geiser) - Enhanced for better integration
(use-package geiser
  :config
  (setq geiser-active-implementations '(guile chicken racket))
  (setq geiser-default-implementation 'guile)
  (setq geiser-guile-binary "guile3")
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
  (setq geiser-repl-startup-time 10000))

(use-package geiser-guile
  :after geiser
  :config
  (setq geiser-guile-extra-keywords '("match" "match-lambda" "match-lambda*")))

(use-package geiser-chicken
  :after geiser)

(use-package geiser-racket
  :after geiser)

;; Racket
(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook (lambda ()
                                (setq indent-tabs-mode nil)
                                (setq tab-width 2))))

;; Fennel
(use-package fennel-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))

;; Janet
(use-package janet-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.janet\\'" . janet-mode)))

;; Hy (Python Lisp)
(use-package hy-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode)))

;; Org-mode configuration for literate programming
(use-package org
  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-edit-src-content-indentation 0)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (lisp . t)
     (scheme . t)
     (clojure . t)
     (python . t)))
  
  ;; Add support for Fennel, Janet, and Hy in org-babel
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("fennel" . fennel))
    (add-to-list 'org-src-lang-modes '("janet" . janet))
    (add-to-list 'org-src-lang-modes '("hy" . hy))))

;; TRAMP for remote file editing
(use-package tramp
  :ensure nil  ; Built-in
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 3))

;; Project management
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Helpful keybindings for this project
(global-set-key (kbd "C-c t") 'org-babel-tangle)
(global-set-key (kbd "C-c d") 'org-babel-detangle)

;; Project-specific settings
(message "Loading Lisp Dialect Showcase project settings")
(message "Project: %s" lisp-project-name)
(message "Root: %s" lisp-project-root)

;; Set up automatic org-babel associations
(with-eval-after-load 'org
  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
  (add-to-list 'org-babel-tangle-lang-exts '("fennel" . "fnl"))
  (add-to-list 'org-babel-tangle-lang-exts '("hy" . "hy"))
  (add-to-list 'org-babel-tangle-lang-exts '("janet" . "janet"))
  (add-to-list 'org-babel-tangle-lang-exts '("scheme" . "scm"))
  (add-to-list 'org-babel-tangle-lang-exts '("racket" . "rkt"))
  (add-to-list 'org-babel-tangle-lang-exts '("lisp" . "lisp"))
  (add-to-list 'org-babel-tangle-lang-exts '("emacs-lisp" . "el")))

;; Custom functions for project navigation
(defun lisp-project-find-file ()
  "Find file in the Lisp project."
  (interactive)
  (find-file (read-file-name "Find file: " 
                            (expand-file-name "src/" lisp-project-root))))

(defun lisp-project-run-dialect (dialect)
  "Run examples for a specific DIALECT."
  (interactive 
   (list (completing-read "Dialect: " 
                         '("common-lisp" "clojure" "scheme" "emacs-lisp" 
                           "racket" "hy" "fennel" "janet"))))
  (let ((script (expand-file-name (format "scripts/run-%s.sh" dialect) 
                                  lisp-project-root)))
    (if (file-exists-p script)
        (compile (format "sh %s" script))
      (message "Script not found: %s" script))))

;; Additional keybindings
(global-set-key (kbd "C-c f") 'lisp-project-find-file)
(global-set-key (kbd "C-c r") 'lisp-project-run-dialect)

;; Optional: Add a project-specific menu
(easy-menu-define lisp-showcase-menu global-map "Lisp Dialect Showcase"
  '("Lisp Showcase"
    ["Tangle All Files" (lambda () (interactive)
                          (let ((default-directory (projectile-project-root)))
                            (shell-command "make tangle"))) t]
    ["Detangle All Files" (lambda () (interactive)
                            (let ((default-directory (projectile-project-root)))
                              (shell-command "make detangle"))) t]
    ["Run All Examples" (lambda () (interactive)
                          (let ((default-directory (projectile-project-root)))
                            (shell-command "make run"))) t]
    ["Run Computational Models" (lambda () (interactive)
                                  (let ((default-directory (projectile-project-root)))
                                    (shell-command "make computational-models"))) t]
    ["Lint Shell Scripts" (lambda () (interactive)
                            (let ((default-directory (projectile-project-root)))
                              (shell-command "make lint-scripts"))) t]))

;; Startup message for tmux session
(when (getenv "TMUX")
  (message "Running in tmux session: %s" (or (getenv "TMUX_SESSION_NAME") "unknown")))

;; Display startup information
(message "Lisp Dialect Showcase environment loaded!")
(message "Use C-c f to find project files, C-c r to run dialect examples")
(message "Use C-c t to tangle org files, C-c d to detangle")

;; Open project README or SETUP.org if available
(let ((readme (expand-file-name "README.org" lisp-project-root)))
  (when (file-exists-p readme)
    (find-file readme)))

(provide 'lisp-dialect-showcase)
;;; lisp-dialect-showcase.el ends here