;;; misc.el --- Miscellaneous packages

;;; Commentary:
;; 


;;; Code:

(use-package guru-mode
  :ensure t
  :hook (after-init . guru-global-mode))

(use-package crontab-mode
  :ensure t)

(use-package lorem-ipsum
  :ensure t)

(use-package string-inflection
  :ensure t
  :config
  (global-unset-key (kbd "C-q"))
  (global-set-key (kbd "C-q C-u") 'my-string-inflection-cycle-auto)

  (defun my-string-inflection-cycle-auto ()
    "switching by major-mode"
    (interactive)
    (cond
     ;; for emacs-lisp-mode
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ;; for python
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ;; for java
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     (t
      ;; default
      (string-inflection-ruby-style-cycle)))))

(provide 'misc)

;;; misc.el ends here
