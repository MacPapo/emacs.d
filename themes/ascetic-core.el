;;; ascetic-core.el --- Core engine for Ascetic themes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 by Jacopo Costantini
;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Macro engine for Ascetic themes. Enforces DRY principles.

;;; Code:

(defmacro ascetic-build-theme (theme-name docstring palette)
  "Build an Ascetic theme dynamically.
THEME-NAME is the theme symbol, DOCSTRING is the description,
PALETTE is a let-binding list of color definitions."
  `(progn
     (deftheme ,theme-name ,docstring)
     (let ,palette
       (custom-theme-set-faces
        ',theme-name

        ;; --- Core UI ---
        `(default ((t (:background ,bg-main :foreground ,fg-main))))
        `(cursor ((t (:background ,fg-main :foreground ,bg-main))))
        `(fringe ((t (:background ,bg-main :foreground ,comment))))
        `(vertical-border ((t (:foreground ,border))))
        `(hl-line ((t (:background ,bg-line))))
        `(region ((t (:background ,region))))

        ;; --- Modeline & Tabs ---
        `(mode-line-buffer-id ((t (:weight bold))))
        `(mode-line ((t (:background ,bg-modeline :foreground ,fg-main :box (:line-width -1 :color ,border :style nil)))))
        `(mode-line-inactive ((t (:background ,bg-modeline-inactive :foreground ,comment :box (:line-width -1 :color ,border :style nil)))))
        `(tab-bar ((t (:background ,bg-modeline-inactive :foreground ,comment))))
        `(tab-bar-tab ((t (:background ,bg-modeline :foreground ,fg-main :weight bold :box (:line-width -1 :color ,border :style nil)))))
        `(tab-bar-tab-inactive ((t (:background ,bg-modeline-inactive :foreground ,comment :box (:line-width -1 :color ,border :style nil)))))
        `(tab-bar-tab-group-current ((t (:weight bold :foreground ,prompt))))
        `(tab-bar-tab-group-inactive ((t (:slant italic :foreground ,comment))))
        `(tab-bar-tab-ungrouped ((t (:foreground ,comment))))

        ;; --- Search & Buffers ---
        `(isearch ((t (:background ,prompt :foreground ,bg-main :weight bold))))
        `(lazy-highlight ((t (:background ,bg-line :foreground ,prompt :weight bold))))
        `(match ((t (:background ,bg-line :foreground ,prompt :weight bold))))
        `(occur-match-face ((t (:inherit match))))
        `(occur-target-face ((t (:foreground ,constant :weight bold))))
        `(compilation-info ((t (:foreground ,constant :weight bold))))
        `(compilation-line-number ((t (:foreground ,comment))))
        `(compilation-column-number ((t (:foreground ,comment))))

        ;; --- Prompts & Completion ---
        `(minibuffer-prompt ((t (:weight bold :foreground ,prompt))))
        `(eshell-prompt ((t (:weight bold :foreground ,prompt))))
        `(comint-highlight-prompt ((t (:weight bold :foreground ,prompt))))
        `(completions-common-part ((t (:foreground ,string))))
        `(completions-first-difference ((t (:weight bold :foreground ,error))))
        `(icomplete-first-match ((t (:weight bold))))
        `(icomplete-selected-match ((t (:background ,bg-line :foreground ,prompt :weight bold))))
        `(icomplete-section ((t (:slant italic :foreground ,comment))))
	`(completion-preview ((t (:slant italic :foreground ,comment))))
        `(completion-preview-common ((t (:weight bold :foreground ,comment))))
        `(completion-preview-exact ((t (:slant italic :foreground ,prompt))))

        ;; --- Core Syntax & Web Mode ---
        `(font-lock-keyword-face ((t (:weight bold :foreground unspecified))))
        `(font-lock-type-face ((t (:weight bold :foreground unspecified))))
        `(font-lock-function-name-face ((t (:weight bold :foreground unspecified))))
        `(font-lock-variable-name-face ((t (:foreground unspecified))))
        `(font-lock-builtin-face ((t (:slant italic :foreground unspecified))))
        `(font-lock-string-face ((t (:foreground ,string))))
        `(font-lock-constant-face ((t (:foreground ,constant))))
        `(font-lock-comment-face ((t (:slant italic :foreground ,comment))))
        `(font-lock-doc-face ((t (:slant italic :foreground ,comment))))

        `(web-mode-html-tag-face ((t (:weight bold :foreground ,fg-main))))
        `(web-mode-html-tag-bracket-face ((t (:foreground ,comment))))
        `(web-mode-html-attr-name-face ((t (:foreground ,fg-main))))
        `(web-mode-html-attr-value-face ((t (:foreground ,string))))
        `(web-mode-jsx-tag-face ((t (:weight bold :foreground ,fg-main))))
        `(web-mode-jsx-attr-name-face ((t (:foreground ,fg-main))))
        `(web-mode-block-delimiter-face ((t (:weight bold :foreground ,prompt))))
        `(web-mode-block-control-face ((t (:weight bold :foreground ,fg-main))))
        `(web-mode-variable-name-face ((t (:foreground ,fg-main))))
        `(web-mode-symbol-face ((t (:foreground ,constant))))
        `(web-mode-part-face ((t (:background unspecified))))
        `(web-mode-block-face ((t (:background unspecified))))
        `(web-mode-current-element-highlight-face ((t (:background ,bg-line))))

        ;; --- Diagnostics & LSP ---
        `(error ((t (:foreground ,error :weight bold))))
        `(warning ((t (:foreground ,warning))))
        `(success ((t (:foreground ,prompt))))
        `(eglot-mode-line ((t (:weight bold :foreground ,error))))
        `(compilation-error ((t (:weight bold :foreground ,error))))
        `(compilation-warning ((t (:weight bold :foreground ,warning))))))

     ;; Theme load-path setup
     (when load-file-name
       (add-to-list 'custom-theme-load-path
                    (file-name-as-directory (file-name-directory load-file-name))))
     (provide-theme ',theme-name)))

(provide 'ascetic-core)
;;; ascetic-core.el ends here
