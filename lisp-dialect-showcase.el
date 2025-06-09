;;; lisp-dialect-showcase.el --- Emacs configuration for working with the Lisp dialect showcase

;;; Commentary:
;; This file provides a project-specific configuration for working with
;; the various Lisp dialects in the showcase. Load this file in Emacs
;; to set up all the necessary packages and configurations.

;;; Code:

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
  (setq cider-repl-history-file "~/.emacs.d/cider-repl-history")
  (add-hook 'cider-repl-mode-hook #'company-mode))

;; Scheme (Geiser)
(use-package geiser
  :config
  (setq geiser-active-implementations '(guile chicken racket)))

(use-package geiser-guile)
(use-package geiser-chicken)
(use-package geiser-racket)

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

;; Project management
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Helpful keybindings for this project
(global-set-key (kbd "C-c t") 'org-babel-tangle)
(global-set-key (kbd "C-c d") 'org-babel-detangle)

;; Project-specific settings
(when (string-match-p "lisp-dialect-showcase" default-directory)
  (message "Loading Lisp Dialect Showcase project settings")
  ;; Set up automatic org-babel associations
  (with-eval-after-load 'org
    (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
    (add-to-list 'org-babel-tangle-lang-exts '("fennel" . "fnl"))
    (add-to-list 'org-babel-tangle-lang-exts '("hy" . "hy"))
    (add-to-list 'org-babel-tangle-lang-exts '("janet" . "janet"))
    (add-to-list 'org-babel-tangle-lang-exts '("scheme" . "scm"))
    (add-to-list 'org-babel-tangle-lang-exts '("racket" . "rkt"))
    (add-to-list 'org-babel-tangle-lang-exts '("lisp" . "lisp"))
    (add-to-list 'org-babel-tangle-lang-exts '("emacs-lisp" . "el"))))

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

(provide 'lisp-dialect-showcase)
;;; lisp-dialect-showcase.el ends here