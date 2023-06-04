;; -*- lexical-binding: t; -*-

;; Magit Config
(setq magit-completing-read-function 'magit-ido-completing-read)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(provide 'magit-config)
