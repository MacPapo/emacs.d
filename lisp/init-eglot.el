;;; init-eglot.el --- LSP Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'eglot)

;; Configuration
(advice-add 'jsonrpc--log-event :override #'ignore)

;; Hooks
;;(add-hook 'ruby-mode-hook #'eglot-ensure)

(provide 'init-eglot)
;;; init-eglot.el ends here
