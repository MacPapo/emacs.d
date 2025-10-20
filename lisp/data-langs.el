;;; data-langs.el --- TEXT oriented modes  -*- lexical-binding: t; -*-

;;; Commentary:
;; 


;;; Code:

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode))

(use-package csv-mode
  :ensure t
  :custom
  (csv-separators '("," ";" "|" " "))
  :hook (csv-mode . csv-guess-set-separator))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(provide 'data-langs)

;;; data-langs.el ends here
