;; -*- lexical-binding: t; -*-

(require 'ido-grid-mode)

;; Configure ido-grid-mode appearance
(setq ido-grid-mode-max-columns 10
      ido-grid-mode-min-rows 1
      ido-grid-mode-prefix-scrolls t
      ido-grid-mode-prefix-display 'always
      ido-grid-mode-scroll-up-char "▲"
      ido-grid-mode-scroll-down-char "▼"
      ido-grid-mode-toggle-entry-char "✓")

(ido-grid-mode 1)

(provide 'ido-grid-mode-config)
