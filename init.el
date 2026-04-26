;;; init.el --- My Core Configuration (Emacs 30.2) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Core configuration. Acme philosophy: text is text.
;; Predictability, speed, minimalism.
;; Native features > external bloatware.

;;; Code:

;; ==========================================
;; 01. BOOTSTRAP & ENGINE
;; ==========================================

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

;; ==========================================
;; 02. ENVIRONMENT & MACOS INTEGRATION
;; ==========================================

(set-face-attribute 'default nil :family "Atkinson Hyperlegible Mono" :height 110)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none
        ns-use-native-fullscreen t)

  ;; Direct PATH injection. Avoids spawning slow sub-shells.
  (let ((my-paths '("/opt/homebrew/bin"
                    "/opt/homebrew/sbin"
                    "~/.rbenv/shims"
                    "~/go/bin"
		    "~/.nvm/versions/node/v24.14.1/bin"
                    "/usr/local/bin"
                    "/usr/bin"
                    "/bin")))
    (setq exec-path (append (mapcar #'expand-file-name my-paths) exec-path))
    (setenv "PATH" (mapconcat #'identity exec-path ":")))

  (defun core/apply-theme (appearance)
    "Dynamic theme switching based on OS appearance."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'ascetic-light t))
      ('dark (load-theme 'ascetic-dark t))))

  (add-hook 'ns-system-appearance-change-functions #'core/apply-theme)
  (set-face-attribute 'default nil :family "Atkinson Hyperlegible Mono" :height 125)

  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls))))

;; ==========================================
;; 03. PACKAGE & MODULE LOADER
;; ==========================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Load external core modules & secrets
(let ((secrets-file (expand-file-name "secrets.el" user-emacs-directory)))
  (when (file-exists-p secrets-file)
    (load secrets-file nil t)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(require 'core-editing)
(require 'core-languages)

(unless (and (eq system-type 'darwin) (display-graphic-p))
  (load-theme 'ascetic-dark t))

;; ==========================================
;; 04. UI & MONOCHROME PHILOSOPHY
;; ==========================================

;; Strip chrome
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message ";; Happy Hacking!\n\n"
      use-short-answers t
      echo-keystrokes 0.1
      visible-bell nil
      ring-bell-function #'ignore
      use-dialog-box nil)

;; Unclutter inactive elements
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(use-package hl-line :init (global-hl-line-mode 1))

;; ==========================================
;; 05. CORE EDITING PRIMITIVES
;; ==========================================

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

;; ==========================================
;; 06. FILESYSTEM & I/O
;; ==========================================

;; Disable symlink lockfiles (.#file) - fatal for web bundlers
(setq create-lockfiles nil)

;; Isolate backups to ~/.emacs.d
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save-list/") t)))

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

;; ==========================================
;; 07. MINIBUFFER & NATIVE COMPLETION
;; ==========================================

(setq completion-styles '(basic partial-completion substring)
      completion-auto-help nil
      completions-detailed nil
      completion-cycle-threshold nil)

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-extended-command-predicate #'command-completion-default-include-p)

(use-package ascetic-read
  :ensure nil
  :config
  (ascetic-read-mode 1))

(use-package ascetic-plumber
  :ensure nil
  :after ascetic-read
  :bind (:map ascetic-minibuffer-map
              ("RET" . ascetic-plumber-commit)
              ("C-j" . ascetic-plumber-commit)))

(use-package consult
  :ensure t
  :config
  (add-hook 'consult--completion-refresh-hook #'ascetic-read-refresh))

;; Shield minibuffer prompt from cursor.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

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

;; ==========================================
;; 08. WINDOWS & WORKSPACES
;; ==========================================

(windmove-default-keybindings)
(use-package winner :config (winner-mode 1))

(setq switch-to-buffer-obey-display-actions t
      window-combination-resize t
      help-window-select t)

;; Deterministic bottom-drawer placements for system buffers.
(use-package window
  :custom
  (display-buffer-alist
   '(("\\*\\(Help\\|Apropos\\|info\\|Messages\\|Warnings\\|Compile-Log\\)\\*"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (window-height . 0.3)
      (reusable-frames . visible))
     ("\\*compilation\\*"
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
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil))

;; ==========================================
;; 09. DEV TOOLS & WORKFLOW
;; ==========================================

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

;; ==========================================
;; 10. EMACS CUSTOM GENERATED
;; ==========================================

;; Loaded last to ensure system modifications don't override manual config.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil t))

;;; init.el ends here
