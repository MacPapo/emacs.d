;;; init-projectile.el --- Projectile Packages ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-per-project-compilation-buffer t
        projectile-mode-line-function '(lambda ()
                                         (format " Proj[%s]"
                                                 (projectile-project-name))))
  :init
  (projectile-mode +1))

(provide 'init-projectile)
;;; init-projectile.el ends here
