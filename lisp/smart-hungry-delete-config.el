;; -*- lexical-binding: t; -*-

;; Smart Hungry Delete Config
(require 'smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "<delete>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

(provide 'smart-hungry-delete-config)
