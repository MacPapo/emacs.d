;;; ascetic-light-theme.el --- Ascetic Light Theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026 by Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Ascetic: Clarity through renunciation.
;; Structure is defined by form. Color is reserved strictly for signal.
;; The digital vellum for high-contrast, low-noise environments.

;;; Code:

(deftheme ascetic-light
  "Clarity through renunciation. Syntax is structure; color is signal.")

(let ((bg-main     "#F4F4F1") ; Base background
      (fg-main     "#333333") ; Base foreground
      (bg-line     "#E8E8E3") ; Current line highlight
      (bg-modeline "#E0E0D8") ; Active modeline
      (comment     "#888888") ; Muted comments
      (string      "#4B6A3A") ; Forest green
      (constant    "#4A658A") ; Slate blue
      (error       "#B34D4C") ; Brick red
      (warning     "#9B7018") ; Dark gold
      (prompt      "#2F5B5B") ; Dark teal
      (region      "#D3DFE8") ; Ice blue
      (border      "#CCCCCC")) ; Window dividers

  (custom-theme-set-faces
   'ascetic-light

   ;; Core UI
   `(default ((t (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((t (:background ,fg-main :foreground ,bg-main))))
   `(fringe ((t (:background ,bg-main :foreground ,comment))))
   `(vertical-border ((t (:foreground ,border))))
   `(hl-line ((t (:background ,bg-line))))
   `(region ((t (:background ,region))))

   ;; Modeline
   `(mode-line ((t (:background ,bg-modeline :foreground ,fg-main :box nil))))
   `(mode-line-inactive ((t (:background ,bg-main :foreground ,comment :box nil))))

   ;; Minibuffer & Completion
   `(minibuffer-prompt ((t (:weight bold :foreground ,prompt))))
   `(completions-common-part ((t (:foreground ,string))))
   `(completions-first-difference ((t (:weight bold :foreground ,error))))

   ;; Syntax: Structure (Monochrome / Typography)
   `(font-lock-keyword-face ((t (:weight bold :foreground "unspecified"))))
   `(font-lock-type-face ((t (:weight bold :foreground "unspecified"))))
   `(font-lock-function-name-face ((t (:weight bold :foreground "unspecified"))))
   `(font-lock-variable-name-face ((t (:foreground "unspecified"))))
   `(font-lock-builtin-face ((t (:slant italic :foreground "unspecified"))))

   ;; Syntax: Signal (Color)
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-constant-face ((t (:foreground ,constant))))
   `(font-lock-comment-face ((t (:slant italic :foreground ,comment))))
   `(font-lock-doc-face ((t (:slant italic :foreground ,comment))))

   ;; Diagnostics & LSP
   `(error ((t (:foreground ,error :weight bold))))
   `(warning ((t (:foreground ,warning))))
   `(success ((t (:foreground ,prompt))))
   `(eglot-mode-line ((t (:weight bold :foreground ,error))))
   `(compilation-error ((t (:weight bold :foreground ,error))))
   `(compilation-warning ((t (:weight bold :foreground ,warning))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ascetic-light)
;;; ascetic-light-theme.el ends here
