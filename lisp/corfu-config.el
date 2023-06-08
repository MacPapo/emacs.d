;; -*- lexical-binding: t; -*-

(require 'corfu)

;; Customize Corfu behavior for specific modes
(setq corfu-cycle t) ;; Enable cycling of candidates
(setq corfu-auto t) ;; Enable automatic completion
(setq corfu-cache t) ;; Enable Caching 
(setq corfu-minimum-prefix-length 2) ;; Adjust the value as per your preference
(setq corfu-sort-order 'corfu-sort-by-usage)

(global-corfu-mode)

(provide 'corfu-config)
