;;; init.el -- Main config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Require Emacs' package functionality
(require 'package)

;; Add the Melpa repository to the list of package sources
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Initialise the package system.
(package-initialize)

(unless (package-installed-p 'graphene)
  (package-install 'graphene))

(unless (package-installed-p 'ido-vertical-mode)
  (package-install 'ido-vertical-mode))

(unless (package-installed-p 'project-persist)
  (package-install 'project-persist))

(unless (package-installed-p 'avy)
  (package-install 'avy))

(unless (package-installed-p 'ace-window)
  (package-install 'ace-window))

(unless (package-installed-p 'expand-region)
  (package-install 'expand-region))

(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))

(unless (package-installed-p 'which-key)
  (package-install 'which-key))

(unless (package-installed-p 'rvm)
  (package-install 'rvm))

(unless (package-installed-p 'robe)
  (package-install 'robe))

(require 'ido-vertical-mode)
(setq ido-vertical-show-count t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-vertical-mode 1)

(require 'icomplete)
(icomplete-vertical-mode 1)

(require 'graphene)
(setq graphene-default-font "Iosevka-10")
(setq graphene-variable-pitch-font "Iosevka-10")
(setq graphene-fixed-pitch-font "Iosevka-10")

(defun graphene-linum ()
  "Set Line Number Mode."
  (setq display-line-numbers-width 4)
  (display-line-numbers-mode t))

(setq isearch-repeat-on-direction-change t
      isearch-wrap-pause nil
      isearch-lazy-count t
      lazy-count-prefix-format "[%s of %s] "
      isearch-forward-thing-at-point '(region url email symbol sexp)
      isearch-allow-prefix t)

(require 'avy)
(avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume)
(global-set-key (kbd "C-j") 'avy-goto-char)
(global-set-key (kbd "C-S-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)

(require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(global-set-key (kbd "M-o") 'ace-window)

(require 'which-key)
(which-key-mode)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby)
(require 'robe)
(eval-after-load 'company
  '(push 'company-robe company-backends))
(add-hook 'ruby-mode-hook 'robe-mode)

(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 100000000
                                     gc-cons-percentage 0.1)))

(provide 'init)
;;; init.el ends here
