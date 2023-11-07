;;; init.el --- Emacs Minimal Config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load Theme
;; (load-theme 'modus-vivendi t)

;; Initialize the package manager
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Minimal Selection
(setq package-selected-packages
      '(
        xah-fly-keys
        
        ;; Corfu Completion
        corfu
        orderless

        ;; Project
        projectile

        ;; Git
        magit
        ))

;; Other Packages
;; Add Themes packages
(require 'pre-themes)
;; Add Editing Utils Packages
(require 'pre-defaults)
;; Add Vertico Packages
;;(require 'pre-vertico)

;; Add Lang specific packages
(require 'pre-ruby)
(require 'pre-dart)

(package-install-selected-packages t)

;; Enable Xah Fly Keys
(require 'init-xah-fly-keys)

;; Theme
(require 'init-themes)

;; Editing Utils
(require 'init-defaults)

;; Completion
(require 'init-corfu)

;; Git
(require 'init-git)

;; Project
(require 'init-project)

;; Eglot LSP
(require 'init-eglot)

;;; LANGS
;; Ruby Lang
(require 'init-ruby)
(require 'init-dart)

;;; init.el ends here
