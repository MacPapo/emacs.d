;;; init-defaults.el --- Better Emacs Defaults ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'savehist)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default
 line-number-mode t
 column-number-mode t
 size-indication-mode t)

(setq-default
 fill-column 80)

(electric-pair-mode +1)
(global-hl-line-mode +1)
(delete-selection-mode t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq savehist-additional-variables '(search-ring regexp-search-ring))
(setq savehist-autosave-interval 60)
(savehist-mode +1)

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 visible-bell t
 use-short-answers t
 kill-do-not-save-duplicates t
 echo-keystrokes 0.02
 truncate-partial-width-windows nil)

;; isearch config
(setq isearch-repeat-on-direction-change t
      isearch-wrap-pause nil
      isearch-forward-thing-at-point '(region url email symbol sexp)
      isearch-allow-prefix t)

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Avy config
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char-timer)

;; Expand Region config
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Which key integration
(require 'which-key)
(which-key-mode)

;; Highlight indent guides config
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; CRUX
(require 'crux)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-o") #'crux-smart-open-line)
(global-set-key (kbd "C-S-o") #'crux-smart-open-line-above)
(global-set-key (kbd "C-k") #'crux-smart-kill-line)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
(global-set-key (kbd "M-<down>") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "M-S-<down>") #'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c K") #'crux-kill-other-buffers)
(global-set-key (kbd "M-o") #'crux-other-window-or-switch-buffer)

;; Smartparens
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'text-mode-hook #'smartparens-mode)

(provide 'init-defaults)
;;; init-defaults.el ends here
