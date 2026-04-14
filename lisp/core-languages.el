;;; core-languages.el --- Syntax, LSP and Grammars -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jacopo Costantini
;;
;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;;
;; Consolidates programming language behaviors, LSP (Eglot) settings,
;; and Tree-sitter grammar management.

;;; Code:

;;; Tree-sitter

(setq treesit-language-source-alist
      '((go         "https://github.com/tree-sitter/tree-sitter-go"         "v0.23.4")
        (ruby       "https://github.com/tree-sitter/tree-sitter-ruby"       "v0.23.1")
        (json       "https://github.com/tree-sitter/tree-sitter-json"       "v0.24.8")
        (css        "https://github.com/tree-sitter/tree-sitter-css"        "v0.23.2")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml"          "v0.5.0")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0")
        (c          "https://github.com/tree-sitter/tree-sitter-c"          "v0.23.6")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp"        "v0.23.4")))

(defun core-treesit-auto-install ()
  "Compile and install missing Tree-sitter grammars."
  (interactive)
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (message "Compiling grammar: %s" lang)
      (treesit-install-language-grammar lang))))

(core-treesit-auto-install)

(setq major-mode-remap-alist
      '((ruby-mode    . ruby-ts-mode)
        (js-json-mode . json-ts-mode)
        (css-mode     . css-ts-mode)
        (js-mode      . js-ts-mode)
        (c-mode       . c-ts-mode)
        (c++-mode     . c++-ts-mode)))

;;; LSP (Eglot)

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 0)
  (eglot-extend-to-xref t)
  (eglot-send-changes-idle-time 0.1)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-stay-out-of 'font-lock)
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode 1))))

;;; Languages

(use-package go-ts-mode
  :defer t
  :mode ("\\.go\\'" "/go\\.mod\\'" "/go\\.work\\'")
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . (lambda ()
                         (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
                         (add-hook 'before-save-hook
                                   (lambda () (call-interactively 'eglot-code-action-organize-imports))
                                   nil t))))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((go-ts-mode go-mod-ts-mode go-work-ts-mode) .
                   ("gopls" :initializationOptions
                    (:staticcheck t :gofumpt t :usePlaceholders t
                     :hints (:assignVariableTypes t :compositeLiteralFields t
                             :constantValues t :functionTypeParameters t
                             :parameterNames t :rangeVariableTypes t)))))))

(use-package ruby-ts-mode
  :defer t
  :hook (ruby-ts-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))))

(use-package c-ts-mode
  :defer t
  :hook ((c-ts-mode c++-ts-mode) . eglot-ensure)) ; Copre entrambe le varianti native

(use-package yaml-ts-mode :defer t :mode "\\.ya?ml\\'")
(use-package json-ts-mode :defer t :mode "\\.json\\'")
(use-package dockerfile-ts-mode :defer t :mode "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'")
(use-package js-ts-mode :defer t :mode "\\.m?js\\'" :hook (js-ts-mode . eglot-ensure))

;;; Web (Fallback)

(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'"  . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.[jt]sx\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-current-element-highlight nil))

(provide 'core-languages)
;;; core-languages.el ends here
