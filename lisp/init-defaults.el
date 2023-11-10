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

(setq x-selection-timeout 100)

(setq-default
 fill-column 80)

(electric-pair-mode +1)
(global-hl-line-mode +1)
(delete-selection-mode t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

(winner-mode 1)
(global-set-key (kbd "M-N") #'winner-redo)
(global-set-key (kbd "M-P") #'winner-undo)
(remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 600
      recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                        "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                        ".*png$" ".*cache$"))

(remove-hook 'after-change-major-mode-hook
             'global-eldoc-mode-enable-in-buffers)

(global-eldoc-mode -1)

;; EMACS C
(setq auto-hscroll-mode 'current-line)
(setq auto-save-interval 64)
(setq auto-save-timeout 2)
(setq enable-recursive-minibuffers t)
(setq history-delete-duplicates t)
(setq history-length 200)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-ignore-buffers-re "^\\*")

(setq dired-listing-switches "-laGh1v --group-directories-first")

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq auto-revert-use-notify nil)

(setq savehist-additional-variables '(search-ring regexp-search-ring))
(setq savehist-autosave-interval 60)
(savehist-mode +1)

(setq list-directory-brief-switches "-CFh"
      list-directory-verbose-switches "-lh")

(setq backup-by-copying t)
(setq delete-old-versions t)
(setq version-control t)
(setq create-lockfiles nil)
(setq redisplay-dont-pause t)
(setq undo-limit 800000)
(setq x-stretch-cursor t)

(when *is-a-mac*
   ;; Settings for the Cocoa port
  (setq ns-alternate-modifier 'alt)
  (setq ns-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (setq ns-right-alternate-modifier 'alt)

  ;; Settings for the Emacs Mac-port
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  (setq mac-pass-command-to-system nil))

;;** enable features
(mapc (lambda (x) (put x 'disabled nil))
      '(erase-buffer upcase-region downcase-region
        dired-find-alternate-file narrow-to-region))

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 load-prefer-newer t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
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
 use-short-answers t
 kill-do-not-save-duplicates t
 echo-keystrokes 0.02
 truncate-partial-width-windows nil)

;; isearch config
(setq isearch-repeat-on-direction-change t
      isearch-wrap-pause nil
      isearch-lazy-count t
      lazy-count-prefix-format "[%s of %s] "
      isearch-forward-thing-at-point '(region url email symbol sexp)
      isearch-allow-prefix t)

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Avy config
(require 'avy)
(setq avy-timeout-seconds 0.25)
(global-set-key (kbd "C-j") 'avy-goto-char-timer)

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
(setq show-paren-delay 0)
(setq sp-highlight-pair-overlay nil)
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'text-mode-hook #'smartparens-mode)

;; Yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(provide 'init-defaults)
;;; init-defaults.el ends here
