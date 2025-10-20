;;; ruby-lang.el --- RUBY configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; 


;;; Code:

(use-package rubocop
  :ensure t)

(use-package minitest
  :ensure t
  :hook (ruby-mode . minitest-mode)
  :custom
  (minitest-use-rails t))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
	       '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(provide 'ruby-lang)

;;; ruby-lang.el ends here
