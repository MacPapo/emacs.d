;; -*- lexical-binding: t; -*-

(unless (package-installed-p 'orderless)
  (package-refresh-contents)
  (package-install 'orderless))

(require 'corfu)

(setq completion-styles '(orderless)
      completion-category-overrides '((file (styles partial-completion)))
      completion-at-point-functions '(capf)
      )

(define-key corfu-map (kbd "<tab>") 'corfu-complete)
(define-key corfu-map (kbd "TAB") 'corfu-complete)

;; Customize Corfu behavior for specific modes
(setq corfu-cycle t ;; Enable cycling of candidates
      corfu-auto t ;; Enable automatic completion
      corfu-quit-at-boundary t
      corfu-quit-no-match t  ;; Only show completions when prefix input matches
      corfu-auto-prefix 1)  ;; Add delay for automatic prefix completion

(global-corfu-mode)

(provide 'corfu-config)
