;;; early-init.el --- Early Startup Optimization -*- lexical-binding: t; -*-

;;; Commentary:
;; Low-level Emacs boot optimizations.
;; Modifies the environment before GUI initialization or package loading
;; to maximize startup speed and prevent visual artifacts.

;;; Code:

;; ==========================================
;; 1. PACKAGE MANAGER & GARBAGE COLLECTION
;; ==========================================

;; Defer package initialization to init.el
(setq package-enable-at-startup nil)

;; Temporarily maximize garbage collection thresholds during boot.
;; This prevents GC pauses from slowing down the initialization process.
;; Will be restored to sensible defaults in `emacs-startup-hook`.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

;; ==========================================
;; 2. I/O OPTIMIZATION
;; ==========================================

;; Temporarily disable file name handlers (e.g., regex checks for compressed
;; or remote files) during startup to significantly boost I/O performance.
(defvar core--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist
		  (delete-dups (append file-name-handler-alist core--file-name-handler-alist)))))

;; ==========================================
;; 3. PRE-RENDERING GUI & FLASH PREVENTION
;; ==========================================

;; Strip down UI elements before the frame is drawn to prevent
;; unstyled flashes and save rendering cycles.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tooltip-mode nil
      use-dialog-box nil
      use-file-dialog nil)

;; Inhibit Emacs from resizing the frame based on character grid geometry.
;; This prevents awkward gaps when using tiling window managers or macOS native fullscreen.
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;; ==========================================
;; 4. CORE TEXT PERFORMANCE
;; ==========================================

;; Prevent resource-intensive font cache compaction.
(setq inhibit-compacting-font-caches t)

;; Disable bidirectional (BIDI) text scanning.
;; Since we assume left-to-right (LTR) languages, disabling this yields
;; one of the most significant rendering performance boosts available in Emacs.
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; ==========================================
;; 5. NATIVE COMPILER NOISE
;; ==========================================

;; Suppress async native-compilation warnings from popping up during boot.
(setq native-comp-async-report-warnings-errors 'silent)

;;; early-init.el ends here
