;;; init-ido.el --- Ido Configuration ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ido
  :config
  (setq ido-everywhere t
	ido-virtual-buffers t
        ido-use-virtual-buffers t
        ido-max-prospects 10
	ido-use-faces t
        ido-create-new-buffer 'always
	ido-default-file-method 'selected-window
	ido-default-buffer-method 'selected-window
        ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido")
	ido-enable-flex-matching t)
  (ido-mode))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

;; Smex
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; This is your old M-x.
         ("C-c C-c M-x" . execute-extended-command)))

(provide 'init-ido)
;;; init-ido.el ends here
