;;; init-defaults.el --- Better Emacs Defaults ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :ensure nil
  :demand t
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  (electric-pair-mode +1)
  (show-paren-mode +1)
  (global-hl-line-mode +1)
  (delete-selection-mode +1)
  (subword-mode +1)
  (save-place-mode +1)

  (setq display-time-day-and-date t
        display-time-24hr-format  t
        display-time-default-load-average nil)
  (display-time-mode +1)

  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        auto-revert-use-notify nil)
  (global-auto-revert-mode +1)

  (setq-default
   line-number-mode t
   column-number-mode t
   size-indication-mode t
   fill-column 80
   blink-cursor-interval 0.4
   bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
   buffers-menu-max-size 30
   case-fold-search t
   load-prefer-newer t
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain
   indent-tabs-mode nil
   tab-width 4
   auto-save-default nil
   mouse-yank-at-point t
   read-buffer-completion-ignore-case t
   read-file-name-completion-ignore-case t
   save-interprogram-paste-before-kill t
   scroll-preserve-screen-position 'always
   set-mark-command-repeat-pop t
   tooltip-delay 1.5
   truncate-lines nil
   visible-bell t
   kill-do-not-save-duplicates t
   echo-keystrokes 0.02
   truncate-partial-width-windows nil)

  ;; Make native compilation silent and prune its cache.
  (when (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
    (setq native-compile-prune-cache t)) ; Emacs 29

  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
  ;; (setq auto-save-file-name-transforms
  ;;     `((".*" ,(concat user-emacs-directory "backups/") t)))

  (setq x-selection-timeout 100
        select-enable-clipboard t
        x-select-enable-clipboard-manager nil)

  ;; (setq scroll-margin 1
  ;;       scroll-conservatively 10000
  ;;       scroll-preserve-screen-position 1)

  ;; EMACS C
  (setq auto-hscroll-mode 'current-line
        auto-save-interval 64
        auto-save-timeout 2
        enable-recursive-minibuffers t
        history-delete-duplicates t
        history-length 200)

  (setq backup-by-copying t
        delete-old-versions t
        version-control t
        compilation-scroll-output 'first-error
        create-lockfiles nil
        redisplay-dont-pause t
        confirm-kill-emacs 'y-or-n-p
        undo-limit 800000
        max-lisp-eval-depth 10000
        x-stretch-cursor t)

  ;; enable features
  (mapc (lambda (x) (put x 'disabled nil))
        '(erase-buffer upcase-region downcase-region dired-find-alternate-file narrow-to-region)))

(use-package savehist
  :ensure nil
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring))
  (setq savehist-autosave-interval 60)
  (savehist-mode +1))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 600
        recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                          "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                          ".*png$" ".*cache$"))
  (recentf-mode 1))

(use-package eldoc
  :ensure nil
  :config
  (remove-hook 'after-change-major-mode-hook
               'global-eldoc-mode-enable-in-buffers)
  (global-eldoc-mode -1))

(use-package winner
  :ensure nil
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
  (winner-mode 1))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-ignore-buffers-re "^\\*"))

(use-package display-line-numbers
  :ensure nil
  :hook prog-mode
  :config
  (setq-default display-line-numbers-width 3))

(use-package display-fill-column-indicator
    :ensure nil
    :hook prog-mode
    :config
    (setq-default
     indicate-buffer-boundaries 'left
     display-fill-column-indicator-character ?\u254e))

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-laGh1v"
        list-directory-brief-switches "-CFh"
        list-directory-verbose-switches "-lhG"
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t))

(use-package mac-config
  :ensure nil
  :if (eq *is-a-mac* t)
  :config
  (setq ns-alternate-modifier 'alt
        ns-command-modifier 'meta
        ns-function-modifier 'hyper
        ns-right-alternate-modifier 'alt)

  ;; Settings for the Emacs Mac-port
  (setq mac-command-modifier 'meta
        mac-option-modifier 'alt
        mac-pass-command-to-system nil))

(use-package isearch
  :ensure nil
  :config
  (setq isearch-repeat-on-direction-change t
        isearch-wrap-pause nil
        isearch-lazy-count t
        lazy-count-prefix-format "[%s of %s] "
        isearch-forward-thing-at-point '(region url email symbol sexp)
        isearch-allow-prefix t))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package avy
  :bind ("C-j" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.25))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package crux
  :bind (
         ("C-o" . crux-smart-open-line)
         ("C-S-o" . crux-smart-open-line-above)
         ("C-k" . crux-smart-kill-line)
         ("C-<backspace>". crux-kill-line-backwards)
         ("M-<down>" . crux-duplicate-current-line-or-region)
         ("M-S-<down>" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c K" . crux-kill-other-buffers)
         ("M-j" . crux-top-join-line)

         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package rainbow-delimiters
  :hook prog-mode
  ;; :diminish rainbow-mode
  )

;; Smartparens
;; (require 'smartparens-config)
;; (setq show-paren-delay 0)
;; (setq sp-highlight-pair-overlay nil)
;; (add-hook 'prog-mode-hook #'smartparens-mode)
;; (add-hook 'text-mode-hook #'smartparens-mode)

;; Yasnippet
;; (require 'yasnippet)
;; (yas-reload-all)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)

(provide 'init-defaults)
;;; init-defaults.el ends here
