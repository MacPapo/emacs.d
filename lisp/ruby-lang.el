;;; ruby-lang.el --- RUBY configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; 


;;; Code:

(use-package ruby-mode
  :defer t
  :ensure-system-package ((ruby :version "3.4") . "mise use -g ruby@3.4"))

(use-package rubocop
  :defer t
  :ensure t
  :ensure-system-package (rubocop . "gem install rubocop"))

(use-package minitest
  :ensure t
  :diminish " mini"
  :hook ((ruby-mode ruby-ts-mode) . minitest-mode)
  :custom
  (minitest-use-rails t))

(use-package eglot
  :defer t
  :ensure t
  :ensure-system-package (ruby-lsp . "gem install ruby-lsp")
  :pin gnu
  :hook ((ruby-mode ruby-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(provide 'ruby-lang)

;;; ruby-lang.el ends here
