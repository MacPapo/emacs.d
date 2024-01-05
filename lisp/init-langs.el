;;; init-langs.el --- Programming Languages Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/languages" user-emacs-directory))

(require 'init-c-cpp)
(require 'init-ruby)
(require 'init-dart)
(require 'init-data)
(require 'init-lisp)
(require 'init-sql)
(require 'init-markdown)

(use-package eglot
  :ensure nil
  :init
  (setq eglot-stay-out-of '(flymake flycheck)))

(use-package flycheck
  :ensure nil
  :hook (prog-mode . flycheck-mode))

(use-package eldoc
  :demand t
  :ensure nil
  :init
  (global-eldoc-mode +1))

(provide 'init-langs)
;;; init-langs.el ends here
