;;; init.el --- Emacs Minimal Config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(setq initial-buffer-choice t) ;;*scratch*

;; Set up custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defun font-exists-p (font)
  (not (null (x-list-fonts font))))

(when (window-system)
  (cond ((font-exists-p "Iosevka") (set-frame-font "Iosevka:spacing=110:size=13" nil t))
    ((font-exists-p "Menlo") (set-frame-font "Menlo:spacing=100:size=13" nil t))))

(when *is-a-mac*
  (setq mac-mouse-wheel-smooth-scroll nil)

  ;; Usefull
  (use-package reveal-in-osx-finder
    :defer t)

  (use-package osx-trash
    :defer t)

  ;; GNU utils
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls))))

(use-feature emacs
  :demand t
  :custom
  (enable-recursive-minibuffers t "Allow minibuffer commands in minibuffer")
  (frame-title-format '(buffer-file-name "%f" ("%b"))
                      "Make frame title current file's name.")
  (find-library-include-other-files nil)
  (indent-tabs-mode nil "Use spaces, not tabs")
  (inhibit-startup-screen t)
  (history-delete-duplicates t "Don't clutter history")
  (sentence-end-double-space nil "Double space sentence demarcation breaks sentence navigation in Evil")
  (completion-styles '(flex basic partial-completion emacs22))
  (report-emacs-bug-no-explanations t)
  (report-emacs-bug-no-confirmation t)
  (bookmark-default-file (locate-user-emacs-file ".bookmarks.el"))
  (buffers-menu-max-size 30)
  (create-lockfiles nil)
  (auto-save-default nil)
  (make-backup-files nil)
  (scroll-preserve-screen-position 'always)
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete))

(use-package organic-green-theme
  :defer t)

(use-package diminish
  :defer 10)

(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook))

(use-package sudo-edit
  :defer t)

(use-feature display-line-numbers
  :defer t
  :hook prog-mode
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 4))

(use-feature display-fill-column-indicator
  :defer 2
  :hook prog-mode
  :custom
  (display-fill-column-indicator-character
   (plist-get '( triple-pipe  ?┆
                 double-pipe  ?╎
                 double-bar   ?║
                 solid-block  ?█
                 empty-bullet ?◦)
              'triple-pipe)))

(use-feature electric
  :demand t
  :config
  (electric-pair-mode +1))

(use-feature autorevert
  :defer 2
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (auto-revert-interval 0.01 "Instantaneously revert")
  :config
  (global-auto-revert-mode t))

(use-feature recentf
  :defer 1
  :config (recentf-mode)
  :custom
  (recentf-max-menu-items 1000 "Offer more recent files in menu")
  (recentf-max-saved-items 1000 "Save more recent files"))

(use-feature winner
  :defer 5
  :config
  (winner-mode +1))

(use-feature window
  :defer 2
  :custom
  (switch-to-buffer-obey-display-actions t)
  (switch-to-prev-buffer-skip-regexp
   '("\\*Help\\*" "\\*Calendar\\*" "\\*mu4e-last-update\\*"
     "\\*Messages\\*" "\\*scratch\\*" "\\magit-.*")))

(use-feature uniquify
  :defer 2
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-feature trwhitespace
  :defer t
  :hook
  ((prog-mode-hook text-mode-hook conf-mode-hook) . my/show-trailing-whitespace)
  :init
  (setq-default show-trailing-whitespace)
  :config
  (defun my/show-trailing-whitespace ()
    "Enable display of trailing whitespace."
    (setq-local show-trailing-whitespace t)))

(use-feature vc-hooks
  :defer 2
  :custom
  (vc-follow-symlinks t "Visit real file when editing a symlink without prompting."))

(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :custom
  (evil-symbol-word-search t "search by symbol with * and #.")
  (evil-shift-width 2 "Same behavior for vim's '<' and '>' commands")
  (evil-want-C-i-jump t)
  (evil-complete-all-buffers nil)
  (evil-want-integration t)
  (evil-want-C-i-jump t)
  (evil-search-module 'evil-search "use vim-like search instead of 'isearch")
  (evil-undo-system 'undo-redo)
  :config
  ;;I want Emacs regular mouse click behavior
  (define-key evil-motion-state-map [down-mouse-1] nil)
  (evil-mode +1))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init)
  :init
  (setq evil-collection-setup-minibuffer t)
  :custom
  (evil-collection-elpaca-want-g-filters nil)
  (evil-collection-ement-want-auto-retro t))

(use-package goto-chg
  :after (evil))

(use-package vi-tilde-fringe
  :diminish vi-tilde-fringe-mode
  :defer 2
  :config
  (global-vi-tilde-fringe-mode +1))

;; Write me
(use-package evil-visual-mark-mode
  :defer t)

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :after (evil)
  :custom
  (evil-snipe-scope 'visible)
  :config
  (evil-snipe-mode +1))

(use-package evil-multiedit
  :after (evil)
  :config
  (evil-multiedit-default-keybinds)) ;; M-d and M-D

(use-package evil-lion
  :after (evil)
  :config
  (evil-lion-mode +1)) ;; gl MOTION CHAR

(use-package evil-anzu
  :after (evil anzu))

(use-package anzu
  :diminish anzu-mode
  :defer 10
  :config
  (global-anzu-mode))

(use-feature dired
  :defer 1
  :commands (dired)
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alh" "Human friendly file sizes.")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-omit-files "\\(?:\\.+[^z-a]*\\)")
  :hook (dired-mode-hook . dired-omit-mode))

(use-package which-key
  :demand t
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 0.2)
  :diminish which-key-mode)

;; Completion
(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode +1))

(use-package marginalia
  :defer 2
  :config
  (marginalia-mode +1))

;; Buffer Completion
(use-package corfu
  :defer 5
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preview-current t)
  (corfu-preselect 'valid)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-scroll-margin 5)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :config
  (global-corfu-mode +1)
  (with-eval-after-load 'evil
    (setq evil-complete-next-func (lambda (_) (completion-at-point)))))

(use-package cape
  :after (corfu)
  :commands (cape-file))

;; Snippets
(use-package tempel
  :defer 2
  :custom
  (tempel-trigger-prefix "<")
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection
  :defer t)

(use-package eglot-tempel
  :defer t)

;;; Git
(use-package magit
  :defer t
  ;;:after (general)
  :custom
  (magit-diff-refine-hunk 'all)
  :config
  (transient-bind-q-to-quit))

(use-package git-modes
  :defer t
  :config
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package gitignore-templates
  :defer t
  :custom
  (gitignore-templates-api 'github))

;;; Org
(use-package org-pomodoro
  :defer t)

;;; Programming

;; Documentation
(use-feature eldoc
  :defer t
  :hook prog-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0.75))

(use-package devdocs
  :defer t)

;; LSP
(use-package eglot
  :defer t
  :custom
  (eglot-events-buffer-size 0)
  (eglot-ignored-server-capabilities '(:hoverProvider
                                       :documentHighlightProvider))
  (eglot-autoshutdown t)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(coffee-mode . ("coffeesense-language-server" "--stdio")))))

;; Tools
(use-package projectile
  :defer 10
 ;; :after (general)
  :config
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  (projectile-mode +1))

(use-package license-templates
  :defer t)

(use-package dotenv-mode
  :defer t
  :mode ("\\.env\\..*\\'" . dotenv-mode))

(use-package ssh-config-mode
  :defer t)

(use-package editorconfig
  :diminish editorconfig-mode
  :defer 5
  :config
  (editorconfig-mode +1))

(use-package editorconfig-generate
  :defer t)

(use-package rainbow-delimiters
  :defer t
  :hook prog-mode)

(use-package highlight-numbers
  :defer t
  :hook prog-mode)

(use-package highlight-escape-sequences
  :defer t
  :hook (prog-mode . hes-mode))

(use-package highlight-indentation
  :diminish highlight-indentation-mode
  :defer t
  :hook prog-mode
  :custom
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

(use-package lorem-ipsum
  :defer t)

;; Languags

;; YAML
(use-package yaml-mode
  :defer t)

;; DATA
(use-package csv-mode
  :defer t)

;; Markdown
(use-package markdown-mode
  :defer 10
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  ;; :custom
  ;; (markdown-command "/usr/bin/pandoc")
  )

;; NGINX
(use-package nginx-mode
  :defer t)

;; APACHE
(use-package apache-mode
  :defer t)

;; GRAPHQL
(use-package graphql-mode
  :defer t)

;; RUBY
(use-package rbenv
  :defer 10
  :config
  (setq rbenv-installation-dir "/opt/homebrew/opt/rbenv")
  (global-rbenv-mode))

(use-package projectile-rails
  :defer t)

(use-package ruby-end
  :diminish ruby-end-mode
  :defer t)

(use-package rake
  :defer t)

(use-package yari
  :defer t)

(use-package bundler
  :defer t)

(use-package rubocop
  :defer t)

;; LISP
(use-package sly
  :defer t
  :commands sly
  :config
  (setq sly-protocol-version 'ignore)
  (setq sly-net-coding-system 'utf-8-unix)
  (let ((features '(sly-fancy)))
    (sly-setup features))
  (setq inferior-lisp-program "/opt/homebrew/opt/sbcl"))

(use-package highlight-quoted
  :defer t
  :hook emacs-lisp-mode-hook)

;; WEB
(use-package web-mode
  :defer t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)))

(use-package emmet-mode
  :defer t
  :hook ((sgml-mode css-mode) . emmet-mode))

(use-package coffee-mode
  :defer t
  :custom
  (coffee-tab-width 2))

(use-package sass-mode
  :defer t)

(use-package scss-mode
  :defer t)

(use-package slim-mode
  :defer t)

;; Write me
(use-package json-snatcher
  :defer t)

;; SQL
(use-package sqlup-mode
  :defer t
  :hook ((sql-mode sql-interactive-mode) . sqlup-mode))

;; C/C++
(use-package modern-cpp-font-lock
  :defer t
  :hook (c++-mode . modern-c++-font-lock-mode))

;; ASM/NASM
(use-package nasm-mode
  :defer t)

;;; MISC
;; FIX ME
(use-package speed-type
  :defer t)

(use-package eshell-toggle
  :defer t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

(use-package neotree
  :defer t
  :bind ([f8] . neotree-toggle))

;;; init.el ends here
