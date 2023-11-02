;;; init-corfu.el --- Corfu Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'corfu)

;; Settings
(setq corfu-cycle t
      corfu-auto t
      corfu-commit-predicate nil
      corfu-quit-no-match t
      corfu-auto-delay 0
      corfu-auto-prefix 1)

;; Bindings
(define-key corfu-map (kbd "<tab>") 'corfu-complete)
(define-key corfu-map (kbd "TAB") 'corfu-complete)

;; Hooks
(add-hook 'eshell-mode-hook #'(lambda () (setq-local corfu-auto nil)))
(add-hook 'shell-mode-hook  #'(lambda () (setq-local corfu-auto nil)))
(add-hook 'term-mode-hook   #'(lambda () (setq-local corfu-auto nil)))

(global-corfu-mode)

(provide 'init-corfu)
;;; init-corfu.el ends here
