;;; init-yasnippet.el --- Yasnippet snippets ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  (push '(company-semantic :with company-yasnippet) company-backends)
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
