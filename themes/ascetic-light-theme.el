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

(let (
      ;; --- Core Architecture ---
      (bg-main              "#F0EAD6")	; main
      (fg-main              "#242424")	; lc(main): 90, lc(mod): 85.8
      (bg-line              "#E6DFCA")

      ;; --- UI Blocks ---
      (bg-modeline          "#E3EBF2")	; mod
      (bg-modeline-inactive "#E5E0D5")
      (border               "#C4C4BC")

      ;; --- Typography & Signals ---
      (comment              "#585858")	; lc 72.2
      (warning              "#8B6012")	; lc 68.3

      (string               "#3A592D")	; lc 75
      (constant             "#36507A")	; lc 75.5
      (error                "#9E3636")	; lc 70.6
      (prompt               "#254A4A")	; lc 80

      (region               "#D5E1ED")
      )

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
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line ((t (:background ,bg-modeline :foreground ,fg-main
				:box (:line-width -1 :color ,border :style nil)))))
   `(mode-line-inactive ((t (:background ,bg-modeline-inactive :foreground ,comment
					 :box (:line-width -1 :color ,border :style nil)))))
   `(vertical-border ((t (:foreground ,border))))

   ;; Minibuffer & Completion
   `(minibuffer-prompt ((t (:weight bold :foreground ,prompt))))
   `(completions-common-part ((t (:foreground ,string))))
   `(completions-first-difference ((t (:weight bold :foreground ,error))))

   ;; Fido / Icomplete
   `(icomplete-first-match ((t (:weight bold))))
   `(icomplete-selected-match ((t (:background ,bg-line :foreground ,prompt :weight bold))))
   `(icomplete-section ((t (:slant italic :foreground ,comment))))

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

   ;; Shell & Terminals
   `(eshell-prompt ((t (:weight bold :foreground ,prompt))))
   `(comint-highlight-prompt ((t (:weight bold :foreground ,prompt))))

   ;; Tab Bar
   `(tab-bar ((t (:background ,bg-modeline-inactive :foreground ,comment))))
   `(tab-bar-tab ((t (:background ,bg-modeline :foreground ,fg-main :weight bold
				  :box (:line-width -1 :color ,border :style nil)))))
   `(tab-bar-tab-inactive ((t (:background ,bg-modeline-inactive :foreground ,comment
					   :box (:line-width -1 :color ,border :style nil)))))
   `(tab-bar-tab-group-current ((t (:weight bold :foreground ,prompt))))
   `(tab-bar-tab-group-inactive ((t (:slant italic :foreground ,comment))))
   `(tab-bar-tab-ungrouped ((t (:foreground ,comment))))

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
