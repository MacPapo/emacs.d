;;; init.el --- My Core Configuration (Emacs 30.2) -*- lexical-binding: t; -*-

;;; Commentary:
;; Core configuration emphasizing predictability, speed, and minimalism.
;; Adheres to the Acme philosophy: text is text. Syntax highlighting is
;; reduced to minimize cognitive overhead, and native features are preferred
;; over external bloatware.

;;; Code:

(prefer-coding-system 'utf-8)

;; Increase the amount of data read from processes in a single chunk.
;; Crucial for fast LSP (Eglot) responses.
(setq read-process-output-max (* 2 1024 1024))

;; --- Boot Restoration ---
;; Revert the GC threshold to a sensible modern default (16MB) post-startup.
(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; Load `fundamental-mode` initially to bypass expensive major-mode hooks.
(setq initial-major-mode 'fundamental-mode)

;; Bypass fontification during rapid typing to prevent input micro-stutters.
(setq redisplay-skip-fontification-on-input t)

;; --- macOS & Environment Integration ---
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'none
	ns-use-native-fullscreen t)

  ;; Inject essential binary paths directly without spawning expensive shells
  ;; via packages like `exec-path-from-shell`.
  (let ((my-paths '("/opt/homebrew/bin"
		    "/opt/homebrew/sbin"
		    "~/.rbenv/shims"
		    "/usr/local/bin"
		    "/usr/bin"
		    "/bin")))
    (setq exec-path (append (mapcar #'expand-file-name my-paths) exec-path))
    (setenv "PATH" (mapconcat #'identity exec-path ":")))

  (defun core/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi-tinted t))
      ('dark (load-theme 'modus-vivendi t))))

  (add-hook 'ns-system-appearance-change-functions #'core/apply-theme)
  (set-face-attribute 'default nil :family "Atkinson Hyperlegible Mono" :height 125)

  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls))))

;; --- Package Management ---
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; --- Identity & Custom Modules ---
(let ((secrets-file (expand-file-name "secrets.el" user-emacs-directory)))
  (when (file-exists-p secrets-file)
    (load secrets-file nil t)))

;; Load custom editing semantics (Verbs & Nouns)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'core-editing)
(require 'core-languages)

;; --- UI & Monochrome Philosophy ---
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message ";; Happy Hacking!\n\n"
      use-short-answers t
      echo-keystrokes 0.1
      visible-bell nil
      ring-bell-function 'ignore
      use-dialog-box nil)

;; --- Eye Comfort & Themes (Modus) ---
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t
      modus-themes-prompts '(intense bold)
      modus-themes-completions '(intense)
      modus-themes-headings
      '((1 . (semibold variable-pitch 1.3))
        (2 . (semibold variable-pitch 1.1))
        (t . (semibold variable-pitch))))

(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi))
(unless (and (eq system-type 'darwin) (display-graphic-p))
  (load-theme 'modus-vivendi t))

;; Enforce minimalist syntax highlighting (Acme style).
;; Focus on structure and text rather than a "neon" color palette.
(setq font-lock-maximum-decoration 1)

(setq-default fill-column 80)
(add-hook 'prog-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; --- Core Editor Behavior ---
(setq sentence-end-double-space nil)
(setq switch-to-buffer-obey-display-actions t)

;; Substantially increase undo history limits to prevent accidental data loss.
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;; Enforce strict, predictable scrolling.
;; Disable implicit vscroll to prevent sudden half-page jumps.
(setq auto-window-vscroll nil)
(setq scroll-conservatively 101)

(delete-selection-mode 1)
(electric-pair-mode 1)
(global-subword-mode 1)

;; Enable advanced native commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; --- Minibuffer & Native Completion ---
(fido-vertical-mode 1)
;; Set compute delay to 0 for instantaneous fido-mode feedback.
(setq icomplete-compute-delay 0)

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-extended-command-predicate #'command-completion-default-include-p)

;; Protect the minibuffer prompt from accidental cursor entry/modification.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package completion-preview
  :init (global-completion-preview-mode 1)
  :bind (:map completion-preview-active-mode-map
	      ("M-n" . completion-preview-next-candidate)
	      ("M-p" . completion-preview-prev-candidate)))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

;; --- Filesystem, Navigation & Backup ---
(savehist-mode 1)
(setq history-length 100)

(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-keep '(file-remote-p file-readable-p))

;; Disable symbolic lockfiles (.#filename) as they clobber modern dev tools (e.g., Vite/Webpack).
(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save-list/") t)))

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

(use-package so-long :init (global-so-long-mode 1))
(use-package executable :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; --- Window Management ---
(windmove-default-keybindings)
(use-package winner :config (winner-mode 1))

;; Deterministic window placements for ephemeral buffers.
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

;; --- Development Tools ---
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
  :custom (dired-listing-switches "-AFlbhv --group-directories-first")
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	delete-by-moving-to-trash t
	dired-dwim-target t))

(use-package which-key :init (which-key-mode 1) :custom (which-key-idle-delay 0.5))

(use-package project
  :custom (project-mode-line t)
  :config (add-to-list 'project-vc-extra-root-markers "go.mod"))

(use-package compile
  :custom
  (compilation-scroll-output t)
  (compilation-always-kill t)
  (compilation-skip-threshold 2)
  (compilation-ask-about-save nil)
  :bind
  (("C-c c" . compile)
   ("C-c C" . project-compile)))

;; Version Control (Native)
(use-package vc
  :bind (("C-x g" . vc-dir))
  :config
  (setq vc-follow-symlinks t
	;; Utilize a modern diff algorithm for more readable hunk boundaries.
	vc-git-diff-switches '("--histogram")))

;; --- Autogenerated Customizations ---
;; Load custom file at the very end to guarantee variable priority.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil t))

;;; init.el ends here
