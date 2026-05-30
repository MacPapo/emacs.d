;;; init.el --- My Core Configuration (Emacs 30.2) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Core configuration. Acme philosophy: text is text.
;; Predictability, speed, minimalism.
;; Native features > external bloatware.

;;; Code:

;;; BOOTSTRAP & ENGINE

(prefer-coding-system 'utf-8)

;; IPC/Network throughput. Critical for LSP/Eglot speed.
(setq read-process-output-max (* 4 1024 1024))

;; GC limits: allow 16MB allocations before pausing.
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; Defer major-mode hook execution on raw buffers.
(setq initial-major-mode 'fundamental-mode)

;; Anti-stutter: halt font-lock during rapid keystrokes.
(setq redisplay-skip-fontification-on-input t)

;; Network timeout avoidance.
(setq ffap-machine-p-known 'reject)

;;; PACKAGE & MODULE LOADER

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Load external core modules & secrets
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file nil t)))

(let ((secrets-file (expand-file-name "secrets.el" user-emacs-directory)))
  (when (file-exists-p secrets-file)
    (load secrets-file nil t)))

(setq custom-safe-themes t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(load-theme 'ascetic-light t t)
(load-theme 'ascetic-dark t)

(require 'core-editing)
(require 'core-languages)

;;; UI & MONOCHROME PHILOSOPHY

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message ";; Happy Hacking!\n\n"
      use-short-answers t
      echo-keystrokes 0.1
      visible-bell nil
      ring-bell-function #'ignore
      use-dialog-box nil)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(use-package hl-line :init (global-hl-line-mode 1))

;;; CORE EDITING PRIMITIVES

;; Clipboard rules
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t)

;; Formatting boundaries
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq sentence-end-double-space nil)

;; State management
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;; Deterministic scrolling (no vscroll jumps)
(setq scroll-conservatively 101
      auto-window-vscroll nil)

;; Native minor modes
(delete-selection-mode 1)
(electric-pair-mode 1)
(global-subword-mode 1)
(use-package so-long :init (global-so-long-mode 1))

;; Enable disabled native commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package repeat
  :init (repeat-mode 1)
  :custom
  (repeat-exit-timeout 3)
  (repeat-exit-key (kbd "RET")))

;;; FILESYSTEM & I/O

(defvar core-state-dir (expand-file-name "var/" user-emacs-directory))
(unless (file-exists-p core-state-dir)
  (make-directory core-state-dir t))

(setq save-place-file (expand-file-name "places" core-state-dir)
      recentf-save-file (expand-file-name "recentf" core-state-dir)
      savehist-file (expand-file-name "history" core-state-dir)
      bookmark-default-file (expand-file-name "bookmarks" core-state-dir)
      project-list-file (expand-file-name "projects" core-state-dir)
      tramp-persistency-file-name (expand-file-name "tramp" core-state-dir))

;; Disable symlink lockfiles (.#file) - fatal for web bundlers
(setq create-lockfiles nil)

(setq backup-directory-alist `(("." . ,(concat core-state-dir "backups")))
      auto-save-file-name-transforms `((".*" ,(concat core-state-dir "auto-save-list/") t)))

;; History tracking
(savehist-mode 1)
(setq history-length 100)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-keep '(file-remote-p file-readable-p))

(use-package saveplace
  :init (save-place-mode 1)
  :custom (save-place-limit 500))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(keymap-global-set "C-c r" #'rename-visited-file)

(use-package autorevert
  :init (global-auto-revert-mode 1)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package tramp
  :defer t
  :custom
  (tramp-default-method "ssh")
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
  :config
  (setq tramp-auto-save-directory (concat user-emacs-directory "tramp-auto-save/")))

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;;; buffer completion

(setq completion-styles '(basic substring partial-completion flex)
      completion-auto-help nil
      completions-detailed nil
      completion-cycle-threshold nil)

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-extended-command-predicate #'command-completion-default-include-p)

;; Shield minibuffer prompt from cursor.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package ascetic-read
  :config
  (ascetic-read-mode 1))

(use-package ascetic-plumber
  :after ascetic-read)

;;; in-buffer completion

(use-package completion-preview
  :init (global-completion-preview-mode 1)
  :custom
  (completion-preview-minimum-symbol-length 2)
  (completion-preview-idle-delay 0.15)
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate)
              ("M-i" . completion-preview-complete)))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name)))

;;; WINDOWS & WORKSPACES

(windmove-default-keybindings)
(use-package winner :config (winner-mode 1))

(setq switch-to-buffer-obey-display-actions t
      window-combination-resize t
      help-window-select t)

(use-package window
  :custom
  (display-buffer-alist
   '(("\\*\\(Help\\|Apropos\\|info\\|Messages\\|Warnings\\|Compile-Log\\)\\*"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (window-height . 0.3)
      (reusable-frames . visible))
     ;; Cattura *compilation*, ma anche *Plumber Stream* e *Plumber: ...*
     ("\\*\\(compilation\\|Plumber.*\\)\\*"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (window-height . 0.3)
      (reusable-frames . visible))
     ("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (window-height . 0.2)))))

(use-package tab-bar
  :init
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  :custom
  (tab-bar-show 1)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-hints t)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-format-align-right tab-bar-format-global))
  :bind
  (("M-[" . tab-bar-history-back)
   ("M-]" . tab-bar-history-forward)))

(use-package ediff
  :defer t
  :custom
  (ediff-diff-options "-w")
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil))

;;; DEV TOOLS & WORKFLOW

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom (display-line-numbers-width 3) (display-line-numbers-grow-only t))

(use-package isearch
  :custom
  (search-highlight t)
  (isearch-lax-whitespace t)
  (isearch-lazy-count t)
  (lazy-count-prefix-format "[%s of %s] "))

(use-package dired
  :custom
  (dired-listing-switches "-AFlbhv --group-directories-first")
  (dired-omit-files "^\\.?#\\|^\\.[a-zA-Z0-9]+\\|\\.DS_Store$\\|\\.class$")
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        dired-dwim-target t))

(use-package which-key
  :init (which-key-mode 1)
  :custom (which-key-idle-delay 0.5))

(use-package project
  :custom (project-mode-line t)
  :config (add-to-list 'project-vc-extra-root-markers "go.mod"))

(use-package compile
  :custom
  (compilation-scroll-output t)
  (compilation-always-kill t)
  (compilation-skip-threshold 2)
  (compilation-ask-about-save nil)
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :bind
  (("C-c c" . compile)
   ("C-c C" . project-compile)))

(use-package vc
  :bind (("C-x g" . vc-dir))
  :config
  (setq vc-follow-symlinks t
        vc-git-diff-switches '("--histogram")))

;; Loaded last to ensure system modifications don't override manual config.
(setq custom-file (expand-file-name "custom.el" core-state-dir))
(when (file-exists-p custom-file)
  (load custom-file nil t))

;;; init.el ends here
