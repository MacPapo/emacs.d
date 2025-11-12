;;; init.el --- My Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Simple yet powerful configuration

;;; Code:

(use-package emacs
  :init
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (defvar site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
    "Directory containing manually installed Emacs Lisp packages.")
  (setopt custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error-if-file-is-missing)
  :custom
  (mode-line-compact t)
  (save-interprogram-paste-before-kill t)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)
  (use-short-answers t)
  (confirm-kill-emacs 'yes-or-no-p)
  (switch-to-buffer-obey-display-actions t)
  (history-delete-duplicates t)
  (enable-recursive-minibuffers t)
  (create-lockfiles nil)
  (x-underline-at-descent-line t)
  (set-mark-command-repeat-pop t)
  (sentence-end-double-space nil)
  (custom-safe-themes t)
  (kill-do-not-save-duplicates t)
  (mark-ring-max 60)
  (global-mark-ring-max 200)
  (delete-by-moving-to-trash t)
  (load-prefer-newer t)
  (user-full-name "Jacopo Costantini")
  (user-mail-address "jacopocostantini32@gmail.com")
  (context-menu-mode t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; BACKUP
  (backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups"))))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  
  ;; AUTOSAVE
  (auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups") t)))
  (auto-save-interval 2400)
  (auto-save-timeout 300)
  :config
  (let ((mono-spaced-font "Monospace")	; Set fonts
	(proportionately-spaced-font "Sans"))
    (set-face-attribute 'default nil :family mono-spaced-font :height 100)
    (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
    (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

  (when (file-directory-p site-lisp-dir) ; Add site-lisp and subdirs to load-path
    (add-to-list 'load-path site-lisp-dir)
    (dolist (dir (directory-files site-lisp-dir t "^[^\\.]"))
      (when (file-directory-p dir)
	(add-to-list 'load-path dir))))
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
		(lambda (args)
                  (cons (format "[CRM%s] %s"
				(string-replace "[ \t]*" "" crm-separator)
				(car args))
			(cdr args))))))

(use-package exec-path-from-shell
  :demand t
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package system-packages :ensure t)
(use-package system-packages-ext :after exec-path-from-shell)

(use-package emacs
  :after exec-path-from-shell
  :if (eq system-type 'darwin)
  :custom
  (ns-use-srgb-colorspace nil)
  (ns-use-proxy-icon nil)
  (mac-command-modifier 'meta)
  (mac-option-modifier 'none)
  (mac-function-modifier 'hyper)
  :config
  
  (set-frame-parameter nil 'ns-transparent-titlebar t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  
  (let ((mono-spaced-font "Go Mono")
	(proportionately-spaced-font "Go"))
    (set-face-attribute 'default nil :family mono-spaced-font :height 130)
    (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
    (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

  (use-package menu-bar
    :config
    (menu-bar-mode +1))

  (let ((gls (executable-find "gls")))
    (when gls (progn
		(setq insert-directory-program gls)))))

(use-package diminish :ensure t)

;;; Built-in

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

(use-package repeat
  :config
  (repeat-mode +1))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 300)
  :config
  (define-advice recentf-cleanup (:around (fun) silently)
    (let ((inhibit-message t)
          (message-log-max nil))
      (funcall fun))))

(use-package savehist
  :hook (after-init . savehist-mode)
  :custom
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-save-minibuffer-history t))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package desktop
  :defer
  :custom
  (desktop-auto-save-timeout 300)
  (desktop-base-file-name "desktop")
  (desktop-restore-forces-onscreen nil)
  (desktop-globals-to-clear nil)
  (desktop-load-locked-desktop t)
  (desktop-missing-file-warning nil)
  (desktop-restore-eager 20)
  (desktop-restore-frames t)
  (desktop-save 'ask-if-new))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(use-package electric
  :hook (after-init . electric-indent-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t)
  (auto-revert-interval 5)
  (global-auto-revert-non-file-buffers t))

(use-package help
  :custom
  (help-window-select t "Always select the help window"))

(use-package copyright
  :hook (before-save . copyright-update))

(use-package calendar
  :custom
  (calendar-week-start-day 1))

(use-package winner
  :hook (after-init . winner-mode))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3)
  (display-line-numbers-grow-only t))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :custom
  (indicate-buffer-boundaries 'left)
  (display-fill-column-indicator-character ?┊))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package isearch
  :custom
  (search-highlight t)
  (search-whitespace-regexp ".*?")
  (search-ring-max 26)
  (regexp-search-ring-max 26)
  (isearch-lax-whitespace t)
  (isearch-repeat-on-direct-change t)
  (isearch-wrap-pause nil)
  (isearch-lazy-count t)
  (lazy-highlight-initial-delay 0.5)
  (lazy-highlight-no-delay-length 4)
  (lazy-count-prefix-format "[%s of %s] ")
  (isearch-forward-thing-at-point '(region url email symbol sexp))
  (isearch-allow-prefix t))

(use-package dired
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :custom
  (ibuffer-expert t))

(use-package find-func
  :custom
  (find-library-include-other-files nil))

(use-package which-func
  :config
  (which-function-mode +1))

(use-package imenu
  :bind ("M-s i" . imenu)
  :custom
  (imenu-use-markers t)
  (imenu-auto-rescan t)
  (imenu-max-item-length 100)
  (imenu-use-popup-menu nil)
  (imenu-eager-completion-buffer t)
  (imenu-space-replacement " ")
  (imenu-level-separator "/"))

(use-package compile
  :defer t
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (global-set-key [(f9)] 'compile))

(use-package project
  :custom (project-mode-line t))

(use-package eldoc
  :diminish eldoc-mode
  :hook (ielm-mode . eldoc-mode)
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t "Visit real file when editing a symlink no prompting."))

(use-package ediff
  :hook (ediff-quit . winner-undo)
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package tramp
  :custom
  (tramp-inline-compress-start-size 1000000)
  (tramp-default-method "ssh"))

(use-package flymake
  :custom
  (flymake-mode-line-lighter "f"))

(use-package completion-preview
  :hook ((shell-mode eshell-mode eat-mode) . completion-preview-mode))

(use-package editorconfig
  :hook (after-init . editorconfig-mode))

;;; Packages

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-prompts '(italic bold))
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-disable-other-themes t)
  (modus-themes-mixed-fonts t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-completions '((matches . (extrabold))
  			      (selection . (semibold italic text-also))))
  (modus-themes-headings '((1 . (variable-pitch 1.5))
			   (2 . (1.3))
			   (agenda-date . (1.3))
			   (agenda-structure . (variable-pitch light 1.8))
			   (t . (1.1))))
  :config
  (modus-themes-load-theme 'modus-vivendi-tinted))

(use-package modus-themes
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (defun mac/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (modus-themes-load-theme 'modus-operandi-tinted))
      ('dark (modus-themes-load-theme 'modus-vivendi-tinted))))

  (add-hook 'ns-system-appearance-change-functions #'mac/apply-theme))

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
	 ("C-o" . crux-smart-open-line)
	 ("C-S-o" . crux-smart-open-line-above)
	 ("C-c o" . crux-open-with)
	 ("C-c r" . crux-rename-file-and-buffer)
	 ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
	 ("C-c k" . crux-kill-other-buffers)
	 ("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
	 ("C-x 4 t" . crux-transpose-windows)
	 ("M-o" . crux-other-window-or-switch-buffer)
	 ("C-<backspace>" . crux-kill-line-backwards)
	 ([remap kill-whole-line] . crux-kill-whole-line)
	 ([remap keyboard-quit] . crux-keyboard-quit-dwim))
  :config
  (crux-reopen-as-root-mode))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-line)
	 ("C-c C-j" . avy-resume)
	 ("C-'" . avy-goto-char-timer))
  :config
  (avy-setup-default))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-count 12)
  :config
  (keymap-set vertico-map "?" #'minibuffer-completion-help)
  (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
  (keymap-set vertico-map "M-TAB" #'minibuffer-complete))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  ;;(completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring
  )

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key "M-."
	consult-narrow-key "<")
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (context-menu-mode 1)
  (add-hook 'context-menu-functions #'embark-context-menu 100)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . nil)
              ("M-RET" . corfu-insert)
	      ("M-SPC" . corfu-insert-separator)
              ("M-." . corfu-show-location)
              ("M-h" . nil)
              ([remap next-line] . nil)
              ([remap previous-line] . nil)
              ("M-." . corfu-info-location)
	      ([remap corfu-info-documentation] . corfu-popupinfo-toggle)
	      ("'" . corfu-quick-complete))
  :custom
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.07)
  (corfu-count 8)
  (corfu-auto  t)
  (corfu-cycle t)
  (corfu-min-width 20)
  (corfu-preview-current 'insert)
  (corfu-quit-no-match 'separator)
  (corfu-preselect 'prompt)
  (corfu-scroll-margin 5)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-quick1 "asdfghjkl;")
  (global-corfu-modes '(not shell-mode eshell-mode eat-mode))
  :config
  (corfu-popupinfo-mode 1)
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode))

(use-package eglot
  :ensure t
  :pin gnu
  :bind (:map
	 eglot-mode-map
	 ("C-c c a" . eglot-code-actions)
	 ("C-c c o" . eglot-code-actions-organize-imports)
	 ("C-c c r" . eglot-rename)
	 ("C-c c f" . eglot-format)
	 ("C-c h" . eldoc))
  :custom
  (eglot-sync-connect nil)
  (eglot-events-buffer-config 0)
  (eglot-report-progress nil)
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package tempel
  :ensure t
  :bind (("M-*" . tempel-insert)
         ("M-+" . tempel-complete)
         :map tempel-map
         ("C-c RET" . tempel-done)
         ("C-<down>" . tempel-next)
         ("C-<up>" . tempel-previous)
         ("M-<down>" . tempel-next)
         ("M-<up>" . tempel-previous))
  :init
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection
  :ensure t
  :after tempel)

(use-package eglot-tempel
  :ensure t
  :hook (eglot-server-initialized . eglot-tempel-mode))

(use-package mise
  :defer t
  :ensure t
  :ensure-system-package mise
  :hook (after-init . global-mise-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-modes
  :ensure t)

(use-package diff-hl
  :ensure t
  :config
  (use-package dired
    :config
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote))

  (use-package magit
    :ensure t
    :config
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  (global-diff-hl-mode)
  (global-diff-hl-show-hunk-mouse-mode))

(require 'data-langs)
(require 'elisp-lang)
(require 'c-cpp-lang)
(require 'web-lang)
(require 'ruby-lang)
(require 'go-lang)

(require 'misc)

(provide 'init)

;;; init.el ends here
