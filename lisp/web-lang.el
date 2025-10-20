;;; web-lang.el --- WEB specific tools  -*- lexical-binding: t; -*-

;;; Commentary:
;; 


;;; Code:

(use-package web-mode
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :custom
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-block-face t)
  (web-mode-enable-part-face t)
  (web-mode-enable-comment-interpolation t)
  (web-mode-enable-heredoc-fontification t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-extra-auto-pairs
   '(("erb"  . (("beg" "end")))
     ("php"  . (("beg" "end")
                ("beg" "end")))
     )))

(use-package eglot
  :ensure t
  :pin gnu
  :config
  (add-to-list 'eglot-server-programs
	       '((web-mode :language-id "erb") . ("lspx"
						  "--lsp" "herb-language-server --stdio"
						  "--lsp" "stimulus-language-server --stdio"))))

(provide 'web-lang)

;;; web-lang.el ends here
