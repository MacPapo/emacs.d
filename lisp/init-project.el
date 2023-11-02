;;; init-project.el --- Project Packages ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'projectile)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(provide 'init-project)
;;; init-project.el ends here
