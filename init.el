;; -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load Theme
(load-theme 'modus-operandi t)

;; Load Better defaults
(require 'better-defaults)

;; Initialize the package manager
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install all the needed packages, or activate it
(dolist (package (quote (smex
                         ido-completing-read+
                         pulsar
                         ace-window
                         diminish
                         crux
                         aggressive-indent
                         highlight-thing
                         smart-hungry-delete
                         multiple-cursors
                         easy-kill
                         browse-kill-ring
                         magit
                         typescript-mode
                         diff-hl
                         which-key)))
  (if (package-installed-p package)
      (require (intern (concat (symbol-name package)
                               "-config")))
    (package-install package)))
