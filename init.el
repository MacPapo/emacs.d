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

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-expand-minimally t
        use-package-always-defer t))

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(defun +project/root-dir (&optional dir)
  (let ((project (project-current nil dir)))
    (unless project (user-error "Not in a project"))
    (project-root project)))

(defun +project/search ()
  (interactive)
  (let ((dir (+project/root-dir)))
    (funcall-interactively #'consult-ripgrep dir)))

(defun +project/search-for-symbol-at-point ()
  (interactive)
  (let ((dir (+project/root-dir))
        (initial (thing-at-point 'symbol t)))
    (funcall-interactively #'consult-ripgrep dir initial)))

(setq initial-buffer-choice t) ;;*scratch*

(defun font-exists-p (font)
  "Check if FONT exists."
  (not (null (x-list-fonts font))))

(defmacro with-window-system (&rest args)
  `(when (window-system)
     ,@args))

(defmacro set-font-if-exists (name spacing size
                                   &optional keep-size frames inhibit)
  `(with-window-system
    (if (font-exists-p ,name)
        (progn
          (set-frame-font
           (format "%s:spacing=%d:size=%d" ,name ,spacing ,size)
           ,keep-size ,frames ,inhibit))
      (set-frame-font
       (format "Menlo:spacing=%d:size=%d" ,spacing ,size)
       ,keep-size ,frames ,inhibit))))

(set-font-if-exists "Iosevka" 110 13)

(when *is-a-mac*
  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  (setq mac-mouse-wheel-smooth-scroll nil)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq ns-function-modifier 'hyper)
  (setq-default locate-command "mdfind")

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))

  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil)

  (use-package exec-path-from-shell
    :demand t
    :config
    (exec-path-from-shell-initialize))

  (use-package reveal-in-osx-finder)

  (use-package osx-trash
    :config
    (osx-trash-setup))

  ;; GNU utils
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls))))


(use-feature cus-edit
  :demand t
  :custom
  (custom-file null-device "Don't store customizations"))

(use-feature emacs
  :demand t
  :bind (("C-<return>" . save-buffer)
         ("M-<left>"   . beginning-of-buffer)
         ("M-<right>"  . end-of-buffer))
  :custom
  (auto-save-list-file-prefix nil)
  (tags-revert-without-query t)
  (font-lock-maximum-decoration t)
  (use-short-answers t)
  (case-fold-search t)
  (create-lockfiles nil)
  (truncate-lines nil)
  (truncate-partial-width-windows nil)
  (max-lisp-eval-depth 10000)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (locale-coding-system 'utf-8)
  (coding-system-for-read 'utf-8)
  (coding-system-for-write 'utf-8)
  (default-process-coding-system '(utf-8-unix . utf-8-unix))
  :init
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(use-package acme-theme
  :demand t
  :custom
  (acme-theme-black-fg t)
  :config
  (load-theme 'acme t))

(use-feature frame
  :defer 3
  :custom
  (blink-cursor-interval 0.4))

(use-feature mb-depth
  :defer 3
  :custom
  (enable-recursive-minibuffers t)
  :config
  (minibuffer-depth-indicate-mode +1))

(use-feature minibuffer
  :defer 3
  :custom
  (completion-cycle-threshold 4))

(use-feature whitespace
  :defer 2
  :diminish (global-whitespace-mode)
  :custom
  (whitespace-line-column 80)
  (whitespace-global-modes
   '(not shell-mode
         eshell-mode
         help-mode
         magit-mode
         magit-diff-mode
         ibuffer-mode
         dired-mode
         occur-mode))
  (whitespace-action
   '(cleanup auto-cleanup))
  (whitespace-style
   '(face trailing tabs spaces newline missing-newline-at-eof empty indentation
          space-after-tab space-before-tab space-mark tab-mark newline-mark))
  :config
  (global-whitespace-mode +1))

(use-feature executable
  :defer 5
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package hotfuzz
  :defer 1
  :custom
  (completion-styles '(hotfuzz basic partial-completion emacs22)))

;; MISC
(use-package diminish)

(use-package sudo-edit)

(use-package mode-line-bell
  :defer 3
  :config
  (mode-line-bell-mode +1))

(use-feature delsel
  :defer 2
  :config
  (delete-selection-mode +1))

(use-feature display-line-numbers
  :hook (prog-mode)
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width 4))

(use-feature display-fill-column-indicator
  :hook (prog-mode)
  :custom
  (fill-column 80)
  (display-fill-column-indicator-character
   (plist-get '( triple-pipe  ?┆
                 double-pipe  ?╎
                 double-bar   ?║
                 solid-block  ?█
                 empty-bullet ?◦)
              'triple-pipe))
  :config
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#717C7C" ; katana-gray
                      :background "transparent"))

(use-feature paren
  :defer 2
  :custom
  (show-paren-ring-bell-on-mismatch t)
  :config
  (show-paren-mode +1))

(use-feature elec-pair
  :defer 2
  :config
  (electric-pair-mode +1))

(use-feature electric
  :defer 2
  :config
  (electric-indent-mode +1))

(use-package aggressive-indent
  :diminish (aggressive-indent-mode)
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode) . aggressive-indent-mode))

(use-feature savehist
  :defer 2
  :custom
  (history-length 100)
  (history-delete-duplicates t)
  (savehist-autosave-interval 120)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :config
  (savehist-mode +1))

(use-feature saveplace
  :defer 2
  :config
  (save-place-mode +1))

(use-feature autorevert
  :defer 2
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (auto-revert-interval 1 "One sec revert")
  :config
  (global-auto-revert-mode +1))

(use-feature help
  :defer 2
  :custom
  (help-window-select t "Always select the help window"))

(use-feature copyright
  :defer 5
  :hook (before-save . copyright-update))

(use-feature menu-bar
  :defer 3
  :custom
  (buffers-menu-max-size 30))

(use-feature bookmark
  :defer 2
  :custom
  (bookmark-default-file (locate-user-emacs-file ".bookmarks.el")))

(use-feature files
  :demand t
  :custom
  (require-final-newline t)
  (confirm-kill-emacs 'yes-or-no-p)
  (version-control t)
  (vc-make-backup-files nil)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (auto-save-default t)
  (auto-save-timeout 20)
  (auto-save-interval 200)
  (make-backup-files t)
  (backup-by-copying t)
  ;; Replace default directories
  (backup-directory-alist
   `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t))))

(use-feature holidays
  :defer 2
  :commands (org-agenda)
  :custom
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-oriental-holidays nil))

(use-feature timeclock
  :custom
  (timeclock-mode-line-display nil)
  :bind (("C-c T i" . timeclock-in)
         ("C-c T o" . timeclock-out)
         ("C-c T c" . timeclock-change)
         ("C-c T r" . timeclock-reread-log)
         ("C-c T u" . timeclock-update-mode-line)
         ("C-c T w" . timeclock-when-to-leave-string)))

(use-feature compile
  :commands (compile recompile)
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-window-height 15)
  :bind ([f5] . recompile)
  :config
  (defun +compilation-colorize ()
    "Colorize from `compilation-filter-start' to `point'."
    (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook #'+compilation-colorize))

(use-feature windmove
  :defer 2
  :config
  (windmove-default-keybindings))

(use-feature hl-line
  :defer 3
  :config
  (global-hl-line-mode +1))

(use-feature recentf
  :defer 1
  :custom
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 500)
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode +1))

(use-feature time
  :defer 2
  :custom
  (display-time-day-and-date t "Show date, day, and time")
  (display-time-24hr-format t "Show time as 24H format")
  (display-time-default-load-average nil "Dont show load avg")
  :config
  (display-time))

(use-feature tramp
  :defer 10
  :custom
  (tramp-inline-compress-start-size 1000000)
  (tramp-default-method "ssh")
  (tramp-backup-directory-alist backup-directory-alist))

(use-feature winner
  :defer 4
  :bind (("M-N" . winner-undo)
         ("M-P" . winner-redo))
  :custom
  (winner-boring-buffers '("*Completions*"
                           "*Compile-Log*"
                           "*inferior-lisp*"
                           "*Fuzzy Completions*"
                           "*Apropos*"
                           "*Help*"
                           "*cvs*"
                           "*Buffer List*"
                           "*Ibuffer*"
                           "*mu4e-loading*"
                           ))
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

(use-feature tab-bar
  ;; TAB NEXT -> C-TAB
  ;; TAB PREV -> C-S-TAB
  :defer 3
  :bind (("C-c TAB n" . tab-new)
         ("C-c TAB k" . tab-close)
         ("C-c TAB /" . tab-undo)
         ("C-c TAB ?" . tab-switch))
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-show 1))

(use-feature tab-line
  :defer 3
  :custom
  (tab-line-close-button-show nil)
  (tab-line-new-button-show   nil))

(use-feature ediff
  :hook (ediff-quit . winner-undo)
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-feature project
  :defer 3
  :bind (:map project-prefix-map
              ("C-s" . +project/search))
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (deadgrep "Find regexp" "r")
     (project-find-dir "Find directory")
     (project-dired "Root dired")
     (project-eshell "Eshell")
     (magit-project-status "Git" "g")
     (+project/search "Search project" "s")
     (+project/search-for-symbol-at-point "Search project with symbol" "S"))))

(use-feature vc-hooks
  :defer 1
  :custom
  (vc-follow-symlinks t "Visit real file when editing a symlink no prompting."))

(use-feature tooltip
  :defer 2
  :custom
  (tooltip-delay 1.5))

(use-feature simple
  :defer 2
  :bind (("M-z"     . zap-to-char)
         ("M-Z"     . zap-up-to-char)
         ("C-."     . set-mark-command)
         ("C-x C-." . pop-global-mark)
         ("M-j"     . join-line))
  :custom
  (save-interprogram-paste-before-kill t)
  (column-number-mode t)
  (line-number-mode t)
  (size-indication-mode t)
  (kill-do-not-save-duplicates t)
  (indent-tabs-mode nil "Use spaces, not tabs")
  (mark-ring-max 60)
  (global-mark-ring-max 200))

(use-package page-break-lines
  ;; C-q C-l for page break
  :defer 2
  :diminish (page-break-lines-mode)
  :config
  (global-page-break-lines-mode +1))

(use-package whole-line-or-region
  :defer 4
  :diminish (whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode +1))

(use-feature autoinsert
  :defer 2
  :config
  (auto-insert-mode +1))

(use-feature remember
  :bind ("C-x M-r" . remember))

(use-feature hideshow
  :diminish (hs-minor-mode)
  :hook (prog-mode . hs-minor-mode))

(use-feature isearch
  :defer 3
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
  :defer 2
  :custom
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  ;; (dired-listing-switches "-alh --group-directories-first" "Human friendly file sizes.")
  ;; (dired-use-ls-dired nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-omit-files "\\(?:\\.+[^z-a]*\\)")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask)
  :hook (dired-mode-hook . dired-omit-mode))

(use-feature dired-async-mode
  :after (dired)
  :config
  (dired-async-mode +1))

(use-feature dired-x
  :after (dired))

(use-feature grep
  :defer 5
  :custom
  (grep-highlight-matches 'auto)
  (grep-scroll-output t))

(use-package wgrep
  :after (grep)
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t)
  (wgrep-enable-key "\C-x\C-q")
  :bind (:map grep-mode-map
              ("w" . wgrep-change-to-wgrep-mode)))

(use-package deadgrep)

(use-package which-key
  :defer 5
  :diminish (which-key-mode)
  :custom
  (which-key-enable-extended-define-key t)
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 0.2)
  :config
  (which-key-mode +1))

(use-package avy
  :custom
  (avy-background t)
  (avy-style 'at-full)
  :bind (("C-'"   . avy-goto-char-timer)
         ("M-g g" . avy-goto-line)
         :map isearch-mode-map
         ("M-j" . avy-isearch)))

(use-package winum
  :defer 3
  :bind (:map winum-keymap
              ("M-0" . winum-select-window-0-or-10)
              ("M-1" . winum-select-window-1)
              ("M-2" . winum-select-window-2)
              ("M-3" . winum-select-window-3)
              ("M-4" . winum-select-window-4)
              ("M-5" . winum-select-window-5)
              ("M-6" . winum-select-window-6)
              ("M-7" . winum-select-window-7)
              ("M-8" . winum-select-window-8)
              ("M-9" . winum-select-window-9))
  :config
  (winum-mode +1))

(use-package ace-window
  ;; x - delete window
  ;; m - swap windows
  ;; M - move window
  ;; c - copy window
  ;; j - select buffer
  ;; n - select the previous window
  ;; u - select buffer in the other window
  ;; c - split window fairly, either vertically or horizontally
  ;; v - split window vertically
  ;; b - split window horizontally
  ;; o - maximize current window
  ;; ? - show these command bindings
  :custom
  (aw-dispatch-always t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . ace-window)))

(use-package crux
  :defer 5
  :bind (("C-o"     . crux-smart-open-line)
         ("C-S-o"   . crux-smart-open-line-above)
         ("C-c d"   . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c k"   . crux-kill-other-buffers)
         ("C-c t"   . crux-visit-term-buffer)
         ("C-c e"   . crux-visit-shell-buffer)))

(use-package goto-chg
  :bind (("C-M-'" . goto-last-change)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package hl-todo
  :defer 5
  :custom
  (hl-todo-highlight-punctuation ":")
  :config
  (global-hl-todo-mode +1))

;; Input Completion
(use-feature ido
  :demand t
  :bind (
         ;; Find Files
         ("C-x C-f"   . ido-find-file)
         ("C-x C-v"   . ido-find-alternate-file)
         ("C-x 4 C-v" . ido-find-alternate-file-other-window)

         ;; Find Buffers
         ("C-x b" . ido-switch-buffer)

         ;; Find Directories
         ("C-x d" . ido-dired)

         ;; Display Buffer (No selection)
         ("C-x 4 C-o" . ido-display-buffer))
  :custom
  (ido-create-new-buffer 'always)
  (ido-use-filename-at-point 'guess)
  (ido-max-prospects 10)
  (ido-use-faces t)
  (ido-default-file-method 'selected-window)
  (ido-auto-merge-work-directories-length -1)
  (ido-file-extensions-order
   '(".org" ".el" ".c" ".cpp" ".rb" ".java" ".lisp" ".md" ".dart"))
  :config
  (ido-mode +1)
  (add-to-list 'ido-ignore-directories "target")
  (add-to-list 'ido-ignore-directories "node_modules"))

(use-feature icomplete
  ;; Remember if you want to submit your current prompt hit M-j
  :defer 2
  :config
  (fido-vertical-mode +1))

(use-package consult
  :defer 4
  :bind (("C-x C-b" . consult-buffer)
         ("M-y"     . consult-yank-from-kill-ring)))

(use-package embark
  :after (consult)
  :bind (("C-," . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-," . embark-act)
         ("C-c C-," . embark-export)
         ("C-c C-l" . embark-collect)))

(use-package embark-consult)

(use-feature ibuffer
  :bind (("C-x B" . ibuffer))
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :custom
  (ibuffer-expert t))

;; NEW

(use-feature find-func
  :defer 3
  :custom
  (find-library-include-other-files nil))

(use-feature which-func
  :defer 2
  :config
  (which-function-mode +1))

(use-feature subword
  :hook ((ruby-mode
          ruby-ts-mode
          python-mode
          python-ts-mode) . subword-mode))

;; Buffer Completion
(use-package company
  :defer 2
  :diminish (company-mode)
  :preface
  (setq tab-always-indent 'complete
        completion-cycle-threshold 3)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 7)
  (company-tooltip-minimum-width 60)
  (company-tooltip-maximum-width 60)
  (company-global-modes '(not erc-mode message-mode eshell-mode))
  (company-format-margin-function 'company-text-icons-margin)
  (company-text-icons-add-background t)
  (company-files-exclusions '(".git/" ".DS_Store" "node_modules/"))
  (company-transformers '(delete-consecutive-dups
                          company-sort-by-occurrence))
  :config
  (global-company-mode +1))

(use-package company-posframe
  :defer 1
  :diminish (company-posframe-mode)
  :config
  (company-posframe-mode +1))

(use-package yasnippet
  :defer 5
  :config
  (yas-global-mode +1))

(use-package yasnippet-snippets)

(use-package slime-company
  :after (slime)
  :custom
  (slime-company-after-completion 'slime-company-just-one-space)
  :config
  ;; (slime-company-maybe-enable)
  )

;;; Git
(use-package magit
  :defer 6
  :custom
  (magit-bury-buffer-function #'quit-window)
  (magit-diff-refine-hunk t)
  :bind (([(meta f12)] . magit-status)
         ("C-x g"      . magit-status)
         ("C-x M-g"    . magit-dispatch)
         :map magit-status-mode-map
         ("C-M-<up>"   . magit-section-up))
  :config
  (transient-bind-q-to-quit))

(use-package transient)

(use-package magit-todos)

(use-package git-modes
  :defer 6
  :config
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package git-timemachine)

(use-package gitignore-templates
  :custom
  (gitignore-templates-api 'github))

;;; Org
(use-package org
  :defer 9)

(use-package org-pomodoro)

;;; Programming

;; Documentation
(use-feature eldoc
  :defer 3
  :custom
  (eldoc-idle-delay 0.2)
  :config
  (global-eldoc-mode +1))

(use-package devdocs)

;; Linting
(use-feature flyspell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  :hook ((text-mode org-mode) . flyspell-mode))

;; LSP
(use-package eglot
  :defer 9
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-ignored-server-capabilities
   '(:hoverProvider :documentHighlightProvider))
  :config
  (with-eval-after-load 'eglot
    (advice-add 'jsonrpc--log-event :override #'ignore)
    (add-to-list 'eglot-server-programs
                 '(coffee-mode . ("coffeesense-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs
                 '((c-mode c++-mode) . ("clangd-mp-18"
                                        "-j=8"
                                        "--clang-tidy"
                                        "--background-index"
                                        "--pch-storage=memory")))))

;; Debugger
(use-feature gdb-mi
  :defer 8
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

;; Tools
(use-package dotenv-mode
  :mode ("\\.env\\..*\\'" . dotenv-mode))

(use-package ssh-config-mode)

(use-package editorconfig
  :defer 10
  :diminish (editorconfig-mode)
  :config
  (editorconfig-mode +1))

(use-package editorconfig-generate)

(use-package dtrt-indent
  :after (editorconfig)
  :config
  (add-hook
   'editorconfig-after-apply-functions
   (lambda (props)
     "Adjust indentation if `editorconfig' hasn't changed it"
     (unless (and (gethash 'indent_style props)
                  (gethash 'indent_size props))
       (message "No EditorConfig properties found, falling back to dtrt-indent")
       (dtrt-indent-mode 1)))))

(use-package rainbow-delimiters
  :hook (prog-mode))

(use-package symbol-overlay
  :diminish (symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-I" . symbol-overlay-remove-all)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev))
  :hook ((prog-mode conf-mode) . symbol-overlay-mode))

(use-package highlight-numbers
  :hook (prog-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package highlight-indentation
  :diminish (highlight-indentation-mode)
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
  :defer 4
  :config
  (repeat-mode +1))

(use-package lorem-ipsum)


;; Languages

;; (use-feature treesit
;;   ;; Experiment (C-TS-MODE, JAVA-TS-MODE crash)
;;   :defer 4
;;   :config
;;   (setq treesit-language-source-alist
;;         '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
;;           (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
;;           (make       . ("https://github.com/alemuller/tree-sitter-make"))
;;           (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
;;           (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
;;           (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
;;           (scss       . ("https://github.com/serenadeai/tree-sitter-scss"))
;;           (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
;;           (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
;;           (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
;;           (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
;;           (java       . ("https://github.com/tree-sitter/tree-sitter-java"))
;;           (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
;;           (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown"))
;;           (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
;;           (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
;;           (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml")))))

;; YAML
(use-package yaml-mode)

;; DATA
(use-package csv-mode
  :custom
  (csv-separators '("," ";" "|" " "))
  :hook (csv-mode . csv-guess-set-separator))

;; DOCKER
(use-package dockerfile-mode)

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  ;; :custom
  ;; (markdown-command "/usr/bin/pandoc")
  )

;; NGINX
(use-package nginx-mode)

;; RUBY
(use-package rvm
  :defer 20
  :config
  (rvm-use-default))

(use-package inf-ruby
  :hook ((ruby-mode ruby-ts-mode) . inf-ruby-minor-mode))

(use-package robe
  :hook ((ruby-mode ruby-ts-mode) . robe-mode))

(use-package ruby-end
  :diminish (ruby-end-mode))

(use-package rspec-mode
  :hook ((ruby-mode ruby-ts-mode) . rspec-mode))

(use-package rake)

(use-package yari)

(use-package bundler)

(use-package rubocop)

;; LISP
(use-package slime
  :hook (lisp-mode)
  :custom
  (slime-lisp-implementations
   '((ccl ("ccl"))
     (clisp ("clisp" "-q"))
     (cmucl ("cmucl" "-quiet"))
     (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
  (inferior-lisp-program "/usr/local/bin/sbcl")
  (slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (slime-fuzzy-completion-in-place t)
  (slime-enable-evaluate-in-emacs t)
  (slime-autodoc-use-multiline-p t)
  :config
  ;; the SBCL configuration file is in Common Lisp
  (add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

  ;; Open files with .cl extension in lisp-mode
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))
  (slime-setup '(slime-fancy
                 slime-cl-indent
                 slime-company
                 slime-repl-ansi-color)))

(use-package slime-repl-ansi-color
  :after (slime))

(use-package elisp-slime-nav
  :diminish (elisp-slime-nav-mode)
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("M-RET" . macrostep-expand)))

(use-package highlight-quoted
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
(use-package php-mode)

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)))

(use-package emmet-mode
  :hook ((web-mode css-mode) . emmet-mode))

(use-package coffee-mode
  :custom
  (coffee-tab-width 2)
  :config
  (add-to-list 'coffee-args-compile "--no-header"))

(use-feature css-mode
  :custom
  (css-indent-offset 2))

(use-package sass-mode)

(use-package scss-mode)

;; SQL
(use-package sqlup-mode
  :hook ((sql-mode sql-interactive-mode) . sqlup-mode))

;; JAVA
(use-feature glasses
  :hook (java-mode . glasses-mode))

;; C/C++
(use-feature cwarn
  :hook ((c-mode c++-mode) . cwarn-mode))

(use-package disaster)

(use-package cmake-mode)

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;; DART
(use-package dart-mode
  :mode "\\.dart\\'"
  :bind (:map dart-mode-map
              ("C-c C-o" . dart-server-format)
              ("C-M-x"   . flutter-run-or-hot-reload)))

(use-package dart-server
  :hook (dart-server . flycheck-mode))

(use-package flutter
  :custom
  (flutter-sdk-path "~/FlutterDev/flutter/"))

;; GO
(use-package go-mode
  :mode ("\\.go\\'" . go-mode))


;;; MISC

(use-package neotree
  :bind ([f8] . neotree-toggle))

(use-feature gnus
  :defer 20
  :bind (:map gnus-group-mode-map
              ("o" . my-gnus-group-list-subscribed-groups))
  :hook ((message-mode . (lambda ()
                           (flyspell-mode t))))
  :config
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-date)
          (not gnus-article-sort-by-number))
        ;; Patch article
        gnus-article-patch-conditions
        '( "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" )
        ;; Specify the send mail function
        send-mail-function         'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)

  (defun my-gnus-group-list-subscribed-groups ()
    "List all subscribed groups with or without un-read messages"
    (interactive)
    (gnus-group-list-all-groups 5)))

(provide 'init)
;;; init.el ends here
