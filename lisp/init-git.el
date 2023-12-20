;;; init-git.el --- Git Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :config
  (setq magit-completing-read-function #'magit-ido-completing-read))

(provide 'init-git)
;;; init-git.el ends here
