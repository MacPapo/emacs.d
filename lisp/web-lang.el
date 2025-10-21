;;; web-lang.el --- WEB specific tools  -*- lexical-binding: t; -*-

;;; Commentary:
;; 


;;; Code:

(use-package js
  :defer t
  :ensure-system-package
  ((node :version "25") . "mise use -g node@25")
  ((deno :version "2")  . "mise use -g deno@2")
  (typescript-language-server . "npm install -g typescript-language-server typescript"))

(use-package web-mode
  :defer t
  :ensure t
  :ensure-system-package (herb-language-server . "npm install -g herb-language-server")
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
  :defer t
  :ensure t
  :ensure-system-package (stimulus-language-server . "npm install -g stimulus-language-server")
  :pin gnu
  :hook ((js-mode . eglot-ensure)
	 (web-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
	       '((web-mode :language-id "erb") . ("lspx"
						  "--lsp" "herb-language-server --stdio"
						  "--lsp" "stimulus-language-server --stdio"))))

;; (add-to-list 'eglot-server-programs
;;              '(templ-ts-mode . ("lspx" "--lsp" "templ lsp" "--lsp" "tailwindcss-language-server --stdio")))

;; (setq-default eglot-workspace-configuration
;;               (list (cons :tailwindCSS
;;                           (list :includeLanguages (list :templ "html")
;;                                 ))))

(provide 'web-lang)

;;; web-lang.el ends here
