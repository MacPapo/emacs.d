;;; init-docker.el --- Better Emacs Docker ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package docker)
(use-package docker-compose-mode)
(use-package dockerfile-mode
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))

(provide 'init-docker)
;;; init-docker.el ends here
