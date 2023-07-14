;; -*- lexical-binding: t; -*-

(require 'eglot)

;; Required
(dolist (package (quote (dash s)))
  (if (package-installed-p package)
      (require package)
    (package-install package)))

(setq eglot-max-num-diagnostics 100
      eglot-sync-method 'incremental
      eglot-menu-string "LSP"
      )

(setq-default flymake-no-changes-timeout nil)
(setq-default flymake-start-syntax-check-on-newline nil)

(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'dart-mode-hook 'eglot-ensure)

(provide 'eglot-config)
