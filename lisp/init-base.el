;;; init-base.el -- Base Emacs Tweaks ;; -*- lexical-binding: t; -*-
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

  (when *is-a-mac*
    (setq ns-alternate-modifier 'alt
          ns-command-modifier 'meta
          ns-function-modifier 'hyper
          ns-right-alternate-modifier 'none)
    ;; Settings for the Emacs Mac-port
    (setq mac-command-modifier 'meta
          mac-option-modifier 'alt
          mac-pass-command-to-system nil))

  ;; enable features
  (mapc (lambda (x) (put x 'disabled nil))
        '(erase-buffer upcase-region downcase-region dired-find-alternate-file narrow-to-region)))

(provide 'init-base)
;;; init-base.el ends here
