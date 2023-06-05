;; -*- lexical-binding: t; -*-

(require 'perspective)
(global-set-key (kbd "M-1") (lambda () (interactive) (persp-switch-by-number 1)))
(global-set-key (kbd "M-2") (lambda () (interactive) (persp-switch-by-number 2)))
(global-set-key (kbd "M-3") (lambda () (interactive) (persp-switch-by-number 3)))
(global-set-key (kbd "M-4") (lambda () (interactive) (persp-switch-by-number 4)))
(global-set-key (kbd "M-5") (lambda () (interactive) (persp-switch-by-number 5)))
(global-set-key (kbd "M-6") (lambda () (interactive) (persp-switch-by-number 6)))
(global-set-key (kbd "M-7") (lambda () (interactive) (persp-switch-by-number 7)))
(global-set-key (kbd "M-8") (lambda () (interactive) (persp-switch-by-number 8)))
(global-set-key (kbd "M-9") (lambda () (interactive) (persp-switch-by-number 9)))
(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
(persp-mode)

(provide 'perspective-config)
