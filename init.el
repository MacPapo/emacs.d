;;; init.el --- Emacs Minimal Config ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t
      use-package-always-defer t)

;; Set up custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Completions
(require 'init-ido)
(require 'init-company)

;; UI & UX
(require 'init-defaults)
(require 'init-themes)

;; Projects related
(require 'init-projectile)
(require 'init-git)

;; Org stuff
(require 'init-org)

;; LANGS
(require 'init-langs)

;; MISC
(require 'init-misc)

(provide 'init)
;;; init.el ends here
