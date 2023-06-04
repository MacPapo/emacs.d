;; -*- lexical-binding: t; -*-

;; Highlight Thing Config
(require 'highlight-thing)
(add-hook 'prog-mode-hook 'highlight-thing-mode)
(setq highlight-thing-what-thing 'symbol)
(setq highlight-thing-exclude-thing-under-point t)
(setq highlight-thing-limit-to-region-in-large-buffers-p nil
      highlight-thing-narrow-region-lines 15
      highlight-thing-large-buffer-limit 5000)
(diminish 'highlight-thing-mode)
(diminish 'hi-lock-mode)

(provide 'highlight-thing-config)
