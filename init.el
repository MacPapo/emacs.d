;;; init.el --- My Emacs configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jacopo Costantini

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
  "Check if FONT exists."
  (not (null (x-list-fonts font))))

(when (window-system)
  (cond ((font-exists-p "Iosevka")
         (set-frame-font "Iosevka:spacing=110:size=13" nil t))
        ((font-exists-p "Menlo")
         (set-frame-font "Menlo:spacing=100:size=13" nil t))))

(when *is-a-mac*
  (setq mac-mouse-wheel-smooth-scroll nil)
  (setq-default locate-command "mdfind")

  (use-package reveal-in-osx-finder
    :defer 10)

  (use-package osx-trash
    :defer 10
    :config
    (osx-trash-setup))

  ;; GNU utils
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls))))


(use-feature emacs
  :demand t
  :bind (("C-<return>" . save-buffer)
         ("M-<left>"   . beginning-of-buffer)
         ("M-<right>"  . end-of-buffer))
  :custom
  (blink-cursor-interval 0.4)
  (confirm-kill-emacs 'yes-or-no-p)
  (enable-recursive-minibuffers t "Allow minibuffer commands in minibuffer")
  (find-library-include-other-files nil)
  (indent-tabs-mode nil "Use spaces, not tabs")
  (history-delete-duplicates t "Don't clutter history")
  (kill-do-not-save-duplicates t)
  (completion-styles '(hotfuzz basic emacs22))
  (report-emacs-bug-no-explanations t)
  (report-emacs-bug-no-confirmation t)
  (bookmark-default-file (locate-user-emacs-file ".bookmarks.el"))
  (buffers-menu-max-size 30)
  (case-fold-search t)
  (create-lockfiles nil)
  (column-number-mode t)
  (line-number-mode t)
  (size-indication-mode t)
  (tooltip-delay 1.5)
  (truncate-lines nil)
  (truncate-partial-width-windows nil)
  (save-interprogram-paste-before-kill t)
  (scroll-preserve-screen-position 'always)
  (completion-cycle-threshold 4)
  (tab-always-indent 'complete)
  (max-lisp-eval-depth 10000)
  (frame-title-format
   '(buffer-file-name "%f" ("%b")) "Make frame title current file's name.")
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-feature whitespace
  :demand t
  :diminish (whitespace-mode)
  :hook (prog-mode)
  :custom
  (whitespace-action
   '(cleanup auto-cleanup))
  (whitespace-style
   '(face trailing tabs spaces newline missing-newline-at-eof empty indentation
          space-after-tab space-before-tab space-mark tab-mark newline-mark)))

(use-package hotfuzz
  :after (emacs))

;; MISC
(use-package diminish
  :defer 10)

(use-package dashboard
  :demand t
  :custom
  (dashboard-center-content t)
  (dashboard-navigation-cycle t)
  :config
  (dashboard-setup-startup-hook))

(use-package sudo-edit
  :defer t)

(use-package mode-line-bell
  :after (emacs)
  :config
  (mode-line-bell-mode +1))

(use-feature delsel
  :after (emacs)
  :config
  (delete-selection-mode +1))

(use-feature display-line-numbers
  :defer t
  :hook prog-mode
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width 4))

(use-feature display-fill-column-indicator
  :defer 2
  :hook prog-mode
  :custom
  (fill-column 80)
  (display-fill-column-indicator-character
   (plist-get '( triple-pipe  ?┆
                 double-pipe  ?╎
                 double-bar   ?║
                 solid-block  ?█
                 empty-bullet ?◦)
              'triple-pipe)))

(use-feature paren
  :after (emacs)
  :config
  (show-paren-mode +1))

(use-feature elec-pair
  :after (emacs)
  :config
  (electric-pair-mode +1))

(use-feature electric
  :after (emacs)
  :config
  (electric-indent-mode +1))

(use-package aggressive-indent
  :after (emacs)
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode) . aggressive-indent-mode))

(use-feature savehist
  :after (emacs)
  :custom
  (history-length 100)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :init
  (savehist-mode +1))

(use-feature saveplace
  :after (emacs)
  :config
  (save-place-mode +1))

(use-feature autorevert
  :defer 2
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (auto-revert-interval 1 "One sec revert")
  :config
  (global-auto-revert-mode t))

(use-feature help
  :after (emacs)
  :custom
  (help-window-select t "Always select the help window"))

(use-feature copyright
  :defer t
  :hook (before-save . copyright-update))

(use-feature files
  :after (emacs)
  :custom
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (auto-save-default t)
  (auto-save-timeout 20)
  (auto-save-interval 200)
  ;; (auto-save-file-name-transforms
  ;;  `(("." . ,(concat user-emacs-directory "backups"))))
  (make-backup-files t)
  (backup-by-copying t)
  (backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backups")))))

(use-feature holidays
  :after (emacs)
  :commands (org-agenda)
  :custom
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-oriental-holidays nil))

(use-feature timeclock
  :after (emacs)
  :custom
  (timeclock-mode-line-display nil)
  :bind (("C-c T i" . timeclock-in)
         ("C-c T o" . timeclock-out)
         ("C-c T c" . timeclock-change)
         ("C-c T r" . timeclock-reread-log)
         ("C-c T u" . timeclock-update-mode-line)
         ("C-c T w" . timeclock-when-to-leave-string)))

(use-feature compile
  :after (emacs)
  :commands (compile recompile)
  :custom (compilation-scroll-output 'first-error)
  :config
  (defun +compilation-colorize ()
    "Colorize from `compilation-filter-start' to `point'."
    (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook #'+compilation-colorize))

(use-feature recentf
  :after (emacs)
  :custom
  (recentf-max-menu-items 1000 "Offer more recent files in menu")
  (recentf-max-saved-items 1000 "Save more recent files")
  :config
  (recentf-mode +1))

(use-feature time
  :after (emacs)
  :custom
  (display-time-day-and-date t "Show date, day, and time")
  (display-time-24hr-format t "Show time as 24H format")
  (display-time-default-load-average nil "Dont show load avg")
  :config
  (display-time))

(use-feature tramp
  :defer 4
  :custom
  (tramp-inline-compress-start-size 1000000)
  (tramp-default-method "ssh"))

(use-feature winner
  :defer 5
  :bind (("M-N" . winner-undo)
         ("M-P" . winner-redo))
  :config
  (winner-mode +1))

(use-feature window
  :defer 2
  :custom
  (switch-to-buffer-obey-display-actions t)
  (switch-to-prev-buffer-skip-regexp
   '("\\*Help\\*" "\\*Calendar\\*" "\\*mu4e-last-update\\*"
     "\\*Messages\\*" "\\*scratch\\*" "\\magit-.*")))

(use-package buffer-move
  :defer 2
  :custom
  (buffer-move-behavior 'move)
  :bind
  (("C-x <up>"    . buf-move-up)
   ("C-x <down>"  . buf-move-down)
   ("C-x <left>"  . buf-move-left)
   ("C-x <right>" . buf-move-right)))

(use-feature uniquify
  :defer 2
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-feature tab-bar
  :defer 2
  :custom
  (tab-bar-close-button-show nil "Dont show the x button on tabs")
  (tab-bar-new-button-show nil)
  (tab-bar-show 1 "only show tab bar when more than one tab"))

(use-feature tab-line
  :defer 2
  :custom
  (tab-line-close-button-show nil)
  (tab-line-new-button-show   nil))

(use-feature ediff
  :defer t
  :hook (ediff-quit . winner-undo)
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-feature vc-hooks
  :defer 2
  :custom
  (vc-follow-symlinks t
                      "Visit real file when editing a symlink no prompting."))

(use-feature simple
  :after (emacs)
  :bind (("M-z"     . zap-to-char)
         ("M-Z"     . zap-up-to-char)
         ("C-."     . set-mark-command)
         ("C-x C-." . pop-global-mark)
         ("M-j"     . join-line)))

(use-package page-break-lines
  ;; C-q C-l for page break
  :after (emacs)
  :diminish (page-break-lines-mode)
  :config
  (global-page-break-lines-mode +1))

(use-package whole-line-or-region
  :after (emacs)
  :diminish (whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode +1))

(use-feature autoinsert
  :after (emacs)
  :config
  (auto-insert-mode +1))

(use-feature remember
  :after (emacs)
  :bind ("C-x M-r" . remember))

(use-feature hideshow
  :defer t
  :hook (prog-mode . hs-minor-mode))

(use-package anzu
  :diminish anzu-mode
  :defer 5
  :custom
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 50)
  (anzu-replace-to-string-separator " => ")
  :bind (([remap query-replace]        . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         ("C-c M-%" . anzu-query-replace-at-cursor-thing)))

(use-feature isearch
  :bind
  (:map isearch-mode-map
        ([remap isearch-query-replace] . anzu-isearch-query-replace)
        ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-rege))
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

(use-feature dired
  :after (emacs)
  :commands (dired)
  :custom
  (dired-dwim-target t)
  ;; (dired-listing-switches "-alh --group-directories-first" "Human friendly file sizes.")
  ;; (dired-use-ls-dired nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-omit-files "\\(?:\\.+[^z-a]*\\)")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-isearch-filenames 'dwim)
  :hook (dired-mode-hook . dired-omit-mode))

(use-feature grep
  :defer 2
  :custom
  (grep-highlight-matches 'auto)
  (grep-scroll-output t))

(use-package rg
  :defer t)

(use-package wgrep
  :after (grep)
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t)
  :bind (:map grep-mode-map
              ("w" . wgrep-change-to-wgrep-mode)))

(use-package which-key
  :diminish (which-key-mode)
  :after (emacs)
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 0.2))

(use-package avy
  :after (emacs)
  :bind (("C-'"   . avy-goto-char-timer)
         ("M-g f" . avy-goto-line)))

(use-package ace-window
  :after (emacs)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . ace-window)))

(use-package pulsar
  :after (emacs)
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  :config
  (pulsar-global-mode +1))

(use-package crux
  :after (emacs)
  :bind (("C-o"     . crux-smart-open-line)
         ("C-S-o"   . crux-smart-open-line-above)
         ("C-c d"   . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c k"   . crux-kill-other-buffers)
         ("C-c t"   . crux-visit-term-buffer)
         ("C-c e"   . crux-visit-shell-buffer)))

(use-package goto-chg
  :defer 5
  :bind (("C-M-'" . goto-last-change)))

(use-package expand-region
  :defer 2
  :bind ("C-=" . er/expand-region))

(use-package hl-todo
  :defer 5
  :config
  (global-hl-todo-mode +1))

(use-feature eshell
  :defer t
  :config
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-hist-mode-map
                            (kbd "C-c C-l")
                            'helm-eshell-history))))

;; Input Completion
(use-package helm
  :demand t
  :diminish (helm-mode)
  :bind (("M-x"     . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-buffers-list)
         ("C-x C-b" . helm-mini)
         ("M-y"     . helm-show-kill-ring)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-x C-d" . helm-browse-project)
         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history))
  :config
  (helm-mode +1))

(use-package helm-descbinds
  :after (helm))

(use-feature ibuffer
  :after (emacs)
  :bind (("C-x B" . ibuffer)))

(use-package embark
  :after (helm)
  :bind (("C-," . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-," . embark-act)
         ("C-c C-," . embark-export)
         ("C-c C-l" . embark-collect)))

;; Buffer Completion
(use-package company
  :after (emacs)
  :diminish (company-mode)
  :custom
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-idle-delay 0.25)
  (company-global-modes '(not erc-mode message-mode eshell-mode))
  (company-tooltip-align-annotations t)
  (company-tooltip-annotation-padding 1)
  (company-tooltip-limit 10)
  (company-tooltip-minimum 4)
  (company-format-margin-function 'company-text-icons-margin)
  (company-text-icons-add-background t)
  (company-files-exclusions '(".git/" ".DS_Store" "node_modules/"))
  (company-dabbrev-other-buffers 'all)
  (company-dabbrev-downcase nil)
  (company-dabbrev-minimum-length 2)
  (company-transformers '(delete-consecutive-dups
                          company-sort-by-occurrence))
  :config
  (global-company-mode +1))

(use-package slime-company
  :after (company slime)
  :custom
  (slime-company-after-completion 'slime-company-just-one-space)
  :config
  (slime-company-maybe-enable))

;;; Git
(use-package magit
  :defer t
  :custom
  (magit-diff-refine-hunk 'all)
  :bind (([(meta f12)] . magit-status)
         ("C-x g"      . magit-status)
         ("C-x M-g"    . magit-dispatch)
         :map magit-status-mode-map
         ("C-M-<up>"   . magit-section-up))
  :config
  (transient-bind-q-to-quit))

(use-package magit-todos
  :after (magit))

(use-package git-modes
  :defer t
  :config
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package git-timemachine
  :defer t)

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
  :hook (prog-mode)
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-idle-delay 0.75))

(use-package devdocs
  :defer t)

;; LSP
(use-package eglot
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  ;; (eglot-ignored-server-capabilities
  ;;  '(:hoverProvider :documentHighlightProvider))
  :config
  (with-eval-after-load 'eglot
    (advice-add 'jsonrpc--log-event :override #'ignore)
    (add-to-list 'eglot-server-programs
                 '(coffee-mode . ("coffeesense-language-server" "--stdio")))))

;; Tools
(use-package projectile
  :defer 2
  :bind ("C-c p" . projectile-command-map)
  :custom
  (projectile-per-project-compilation-buffer t)
  (projectile-mode-line-function '(lambda ()
                                    (format " P[%s]" (projectile-project-name))))
  :config
  (projectile-mode +1))

(use-package license-templates
  :defer t)

(use-package dotenv-mode
  :defer t
  :mode ("\\.env\\..*\\'" . dotenv-mode))

(use-package ssh-config-mode
  :defer t)

(use-package editorconfig
  :diminish (editorconfig-mode)
  :defer 5
  :config
  (editorconfig-mode +1))

(use-package editorconfig-generate
  :defer t)

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode))

(use-package symbol-overlay
  :defer t
  :diminish (symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev))
  :hook ((prog-mode conf-mode) . symbol-overlay-mode))

(use-package highlight-numbers
  :defer t
  :hook (prog-mode))

(use-package highlight-escape-sequences
  :defer t
  :hook (prog-mode . hes-mode))

(use-package highlight-indentation
  :diminish (highlight-indentation-mode)
  :defer t
  :hook ((ruby-mode
          ruby-ts-mode
          python-mode
          python-ts-mode
          coffee-mode
          haml-mode
          yaml-mode
          yaml-ts-mode) . highlight-indentation-mode)
  :custom
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

(use-feature repeat
  :after (emacs)
  :config
  (repeat-mode +1))

(use-package lorem-ipsum
  :defer t)


;; Languags

;; YAML
(use-package yaml-mode
  :defer t)

;; DATA
(use-package csv-mode
  :defer t
  :custom
  (csv-separators '("," ";" "|" " "))
  :hook (csv-mode . csv-guess-set-separator))

;; DOCKER
(use-package dockerfile-mode
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

(use-package inf-ruby
  :defer t
  :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode))

(use-package projectile-rails
  :after (projectile rbenv))

(use-package robe
  :defer t
  :hook ((ruby-mode ruby-ts-mode) . robe-mode))

(use-package ruby-end
  :diminish (ruby-end-mode)
  :defer t)

(use-package rspec-mode
  :hook ((ruby-mode ruby-ts-mode) . rspec-mode))

(use-package rake
  :defer t)

(use-package yari
  :defer t)

(use-package bundler
  :defer t)

(use-package rubocop
  :defer t)

;; LISP
(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
  (slime-setup '(slime-fancy
                 slime-company
                 slime-repl-ansi-color)))

(use-package slime-repl-ansi-color
  :after (slime))

(use-package elisp-slime-nav
  :defer t
  :diminish (elisp-slime-nav-mode)
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(use-package highlight-quoted
  :defer t
  :hook (emacs-lisp-mode))

(use-package paredit
  ;; Paredit Cheat Sheet

  ;; Navigation
  ;; Go to the opening parenthesis: M-C-u (paredit-backward-up)
  ;; Go to the closing parenthesis: M-C-d (paredit-forward-down)

  ;; Parentheses and Quotes Manipulation
  ;; Insert balanced parentheses: M-( (paredit-wrap-round)
  ;; Slurp a parenthesis forward: C-) (paredit-forward-slurp-sexp)
  ;; Slurp a parenthesis backward: C-( (paredit-backward-slurp-sexp)
  ;; Barf a parenthesis forward: C-} (paredit-forward-barf-sexp)
  ;; Barf a parenthesis backward: C-{ (paredit-backward-barf-sexp)
  ;; Surround with quotes: M-\" (paredit-meta-doublequote)

  ;; Deletion and Killing
  ;; Delete a character forward: C-d (paredit-forward-delete)
  ;; Delete a character backward: DEL (paredit-backward-delete)
  ;; Kill a line (keeping parentheses balanced): C-k (paredit-kill)

  ;; Splitting and Joining
  ;; Split an s-expression: M-S (paredit-split-sexp)
  ;; Join two s-expressions: M-J (paredit-join-sexps)
  :diminish (paredit-mode)
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode) . enable-paredit-mode))

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
  :hook ((web-mode css-mode) . emmet-mode))

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

;; ELM
(use-package elm-mode
  :defer t)

(use-package elm-test-runner
  :defer t)

;; ERLANG
(use-package erlang
  :defer t
  :config
  (require 'erlang-start))

;; Write me
(use-package json-snatcher
  :defer t)

;; SQL
(use-package sqlup-mode
  :defer t
  :hook ((sql-mode sql-interactive-mode) . sqlup-mode))

(use-package sqlformat
  :defer t
  :bind (:map sql-mode-map
              ("C-c C-f" . sqlformat))
  ;; :hook (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g")))

;; JAVA
(use-feature glasses
  :defer t
  :hook (java-mode . glasses-mode))

;; C/C++
(use-feature cwarn
  :defer t
  :hook ((c-mode c++-mode) . cwarn-mode))

(use-package disaster
  :defer t)

(use-package cmake-mode
  :defer t)

(use-package modern-cpp-font-lock
  :defer t
  :hook (c++-mode . modern-c++-font-lock-mode))

;; ASM/NASM
(use-package nasm-mode
  :defer t)

;; DART
(use-package dart-mode
  :defer t
  :mode "\\.dart\\'"
  :bind (:map dart-mode-map
              ("C-c C-o" . dart-server-format)
              ("C-M-x"   . flutter-run-or-hot-reload)))

(use-package dart-server
  :after (dart-mode)
  :hook (dart-server . flycheck-mode))

(use-package flutter
  :after (dart-mode)
  :config
  (setq flutter-sdk-path "~/FlutterDev/flutter/"))

;; GO
(use-package go-mode
  :defer t
  :mode ("\\.go\\'" . go-mode))


;;; MISC
;; FIX ME
(use-package speed-type
  :defer t)

(use-package eshell-toggle
  :after (eshell)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

(use-package neotree
  :defer t
  :bind ([f8] . neotree-toggle))

(provide 'init)
;;; init.el ends here
