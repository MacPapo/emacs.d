;;; elisp-lang.el --- Emacs Lisp config

;;; Commentary:
;; 


;;; Code:

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
	      ("C-c e" . macrostep-expand)))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(provide 'elisp-lang)

;;; elisp-lang.el ends here
