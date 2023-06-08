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

(when (member "IBM Plex Mono" (font-family-list))
  (custom-set-faces
   '(default ((t (:family "IBM Plex Mono" :height 125))))))


;; Install all the needed packages, or activate it
(dolist (package (quote (
                         ;; IDO PACKAGES
                         smex
                         ido-completing-read+
                         ido-grid-mode
                         flx-ido

                         neotree
                         corfu
                         pulsar
                         ace-window
                         diminish
                         crux
                         aggressive-indent
                         highlight-thing
                         smart-hungry-delete
                         multiple-cursors
                         browse-kill-ring
                         magit
                         
                         diff-hl
                         which-key
                         perspective

                         ;; EGLOT
                         eglot

                         ;; LANG PACKAGES
                         typescript-mode

                         )))
  (if (package-installed-p package)
      (require (intern (concat (symbol-name package)
                               "-config")))
    (package-install package)))
