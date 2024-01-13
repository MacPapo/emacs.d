;;; init-ivy.el --- Ivy Configuration ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ivy
  :demand t
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
          ("C-j" . ivy-immediate-done)
          ("TAB" . ivy-partial)
          ("RET" . ivy-alt-done))
  :config
  (setq ivy-use-virtual-buffers t
    enable-recursive-minibuffers t
    ivy-count-format "[%d/%d] ")
  (ivy-mode +1))

(use-package counsel
  :after ivy
  :diminish
  :config
  (counsel-mode +1))

(use-package amx
  :after ivy
  :config
  (amx-mode +1))

(provide 'init-ivy)
;;; init-ivy.el ends here
