;;; go-lang.el --- GO configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; 


;;; Code:

(use-package go-mode
  :defer t
  :ensure t
  :ensure-system-package ((go :version "1.25" :version-flag "version") . "mise use -g go@1.25"))

(use-package project
  :defer t
  :config
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module))

(use-package eglot
  :defer t
  :ensure t
  :ensure-system-package (gopls . "go install golang.org/x/tools/gopls@latest")
  :pin gnu
  :hook ((go-mode . eglot-ensure)
	 (go-mode . eglot-format-buffer-before-save))
  :config
  (setq-default eglot-workspace-configuration
		'((:gopls .
			  ((staticcheck . t)
			   (matcher . "CaseSensitive")))))
  (add-hook 'before-save-hook
	    (lambda ()
              (call-interactively 'eglot-code-action-organize-imports))
	    nil t)
  (defun eglot-format-buffer-before-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)))

(provide 'go-lang)

;;; go-lang.el ends here
