;; -*- lexical-binding: t; -*-

(require 'ido-at-point)

(ido-at-point-mode 1)
(global-set-key (kbd "C-<return>") 'completion-at-point)

(provide 'ido-at-point-config)
