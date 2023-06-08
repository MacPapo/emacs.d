;; -*- lexical-binding: t; -*-

(require 'eglot)

;; Required
(dolist (package (quote (dash s)))
  (if (package-installed-p package)
      (require package)
    (package-install package)))

(setq eglot-ignored-server-capabilites '(:documentHighlightProvider)
      eglot-stay-out-of '(:hover)
      eglot-max-num-diagnostics 100
      eglot-sync-method 'incremental)

(provide 'eglot-config)
