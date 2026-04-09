;;; ascetic-dark-theme.el --- Ascetic Dark Theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026 by Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Ascetic: Clarity through renunciation.
;; Structure is defined by form. Color is reserved strictly for signal.
;; A high-contrast, low-noise environment for deep cognition.

;;; Code:

(deftheme ascetic-dark
  "Clarity through renunciation. Syntax is structure; color is signal.")

(let ((bg-main     "#1E1E1E") ; Base background
      (fg-main     "#CCCCCC") ; Base foreground
      (bg-line     "#2A2A2A") ; Current line highlight
      (bg-modeline "#323232") ; Active modeline
      (comment     "#7A7A7A") ; Muted comments
      (string      "#8BA59B") ; Desaturated sage
      (constant    "#9AA8C7") ; Dusty blue
      (error       "#E46876") ; Coral red
      (warning     "#D8A657") ; Dark mustard
      (prompt      "#A9B665") ; Olive green
      (region      "#304259") ; Midnight blue
      (border      "#444444")) ; Window dividers

  (custom-theme-set-faces
   'ascetic-dark

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
   `(eglot-mode-line ((t (:weight bold :foreground ,warning))))
   `(compilation-error ((t (:weight bold :foreground ,error))))
   `(compilation-warning ((t (:weight bold :foreground ,warning))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ascetic-dark)
;;; ascetic-dark-theme.el ends here
