;;; early-init.el --- Early Startup Optimization -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Low-level boot optimizations. Executes before GUI initialization
;; to maximize startup speed, prevent UI flashes, and tweak GC.

;;; Code:

;; ==========================================
;; 1. BOOTSTRAP & GARBAGE COLLECTION
;; ==========================================

(setq package-enable-at-startup nil)

(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 1.0)

;; ==========================================
;; 2. I/O OPTIMIZATION
;; ==========================================

(defvar core--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist
                  (delete-dups (append file-name-handler-alist
                                       core--file-name-handler-alist)))))

;; ==========================================
;; 3. GUI STRIPPING & FLASH PREVENTION
;; ==========================================

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq menu-bar-mode    nil
      tool-bar-mode    nil
      scroll-bar-mode  nil
      tooltip-mode     nil
      use-dialog-box   nil
      use-file-dialog  nil)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise       t
      window-resize-pixelwise      t)

;; ==========================================
;; 4. CORE TEXT PERFORMANCE
;; ==========================================

(setq inhibit-compacting-font-caches t)

(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t)

;; ==========================================
;; 5. NATIVE COMPILER NOISE
;; ==========================================

(setq native-comp-async-report-warnings-errors 'silent)

;;; early-init.el ends here
