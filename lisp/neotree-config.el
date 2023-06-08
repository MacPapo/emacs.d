;; -*- lexical-binding: t; -*-

;; Enable Neotree
(require 'neotree)

(global-set-key [f8] 'neotree-toggle) ;; Assign a keybinding to toggle Neotree
(global-set-key (kbd "C-c o p") 'neotree-toggle)

;; Set Neotree to automatically follow the current file
(setq-default neo-smart-open t)
(add-hook 'neo-after-create-hook
          (lambda (&rest _) (display-line-numbers-mode -1)))

;; Set Neotree to be project-wise
(setq neo-vc-integration '(face))

;; Set the default directory to the current project's root
(setq neo-smart-open t)

(provide 'neotree-config)
