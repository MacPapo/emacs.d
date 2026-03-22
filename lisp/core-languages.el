;;; core-languages.el --- Programming languages and LSP -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidates all programming language behaviors, LSP (Eglot) settings,
;; and Tree-sitter grammar management.
;; Automatically compiles Tree-sitter grammars locally to ensure perfect
;; compatibility across macOS (Homebrew) and Linux (Debian) environments.

;;; Code:

;; ==========================================
;; SECTION 1: TREE-SITTER AUTO-COMPILATION
;; ==========================================

;; Define the canonical sources for the grammars.
(setq treesit-language-source-alist
      '((go         "https://github.com/tree-sitter/tree-sitter-go")
	(gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
	(ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
	(yaml       "https://github.com/ikatyang/tree-sitter-yaml")
	(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	(json       "https://github.com/tree-sitter/tree-sitter-json")
	(css        "https://github.com/tree-sitter/tree-sitter-css")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

(defun core-treesit-auto-install ()
  "Compile and install missing Tree-sitter grammars automatically.
Relies on the system's C compiler (clang on macOS, gcc on Linux)."
  (interactive)
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (message "Core: Compiling Tree-sitter grammar for %s..." lang)
      (treesit-install-language-grammar lang))))

(core-treesit-auto-install)

;; ==========================================
;; SECTION 1.5: TREE-SITTER MINIMALISM (ACME)
;; ==========================================
;;
;; We force level 1 (or 2) to maintain a zero-distraction, monochrome editing
;; environment consistent with the Acme philosophy.
(setq treesit-font-lock-level 1)

;; ==========================================
;; SECTION 2: NATIVE MODE REMAPPING
;; ==========================================

;; Transparently redirect traditional regex-based modes to their modern
;; C-powered Tree-sitter equivalents.
(setq major-mode-remap-alist
      '((ruby-mode       . ruby-ts-mode)
	(yaml-mode       . yaml-ts-mode)
	(dockerfile-mode . dockerfile-ts-mode)
	(json-mode       . json-ts-mode)
	(css-mode        . css-ts-mode)
	(js-mode         . js-ts-mode)
	(typescript-mode . typescript-ts-mode)))

;; ==========================================
;; SECTION 3: CORE LSP (EGLOT) ENGINE
;; ==========================================

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  ;; Establish connection asynchronously to prevent initial lock-ups.
  (eglot-sync-connect 0)
  (eglot-extend-to-xref t)
  ;; Send changes to LSP server before saving to ensure accurate formatting.
  (eglot-send-changes-idle-time 0.1)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  ;; Prevent Eglot from overriding our minimalist font-lock choices.
  (add-to-list 'eglot-stay-out-of 'font-lock)

  (defun core-eglot-format-on-save ()
    "Run LSP formatting and import organization safely before save."
    (when (eglot-managed-p)
      ;; Use ignore-errors so a crashed LSP doesn't prevent file saving
      (ignore-errors (call-interactively #'eglot-code-action-organize-imports))
      (ignore-errors (eglot-format-buffer))))

  (defun core-eglot-setup-buffer ()
    "Activate inlay hints and save hooks."
    (eglot-inlay-hints-mode 1)
    (add-hook 'before-save-hook #'core-eglot-format-on-save nil t))

  (add-hook 'eglot-managed-mode-hook #'core-eglot-setup-buffer))


;; ==========================================
;; SECTION 4: LANGUAGE SPECIFICS
;; ==========================================

;; --- Golang ---
(use-package go-ts-mode
  :defer t
  :mode "\\.go\\'"
  :hook (go-ts-mode . eglot-ensure)
  :config
  ;; Optimize Gopls: enable static checks and case-sensitive matching.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '((go-ts-mode go-mod-ts-mode) .
		   ("gopls" :initializationOptions
		    (:staticcheck t :matcher "CaseSensitive"))))))

(use-package go-mod-ts-mode :defer t :mode "/go\\.mod\\'")

;; --- Ruby ---
(use-package ruby-ts-mode
  :defer t
  :hook (ruby-ts-mode . eglot-ensure)
  :config
  ;; Ensure Eglot knows to use ruby-lsp for the Tree-sitter mode.
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))))

;; --- Data & Config Formats ---
(use-package yaml-ts-mode :defer t)
(use-package dockerfile-ts-mode :defer t)
(use-package json-ts-mode :defer t)

;; --- Frontend (JS/TS) ---
(use-package js-ts-mode :defer t :hook (js-ts-mode . eglot-ensure))
(use-package typescript-ts-mode :defer t :hook (typescript-ts-mode . eglot-ensure))


;; ==========================================
;; SECTION 5: WEB & TEMPLATES (FALLBACK)
;; ==========================================

;; Rely on regex-based web-mode for complex template interpolation (ERB)
;; where tree-sitter injection currently falls short.
(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)
	 ("\\.tsx?\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  :config
  ;; Prevent web-mode from drawing invasive column highlights
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight nil))

(provide 'core-languages)
;;; core-languages.el ends here
