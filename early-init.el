;;; early-init.el --- My Early Init file             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My personal Early Init file

;;; Code:

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (or (eq system-type 'gnu)
                           (eq system-type 'gnu/linux)))

(setq package-enable-at-startup t)
(setq inhibit-default-init nil)

(setq native-comp-deferred-compilation t
      native-comp-async-query-on-exit t
      native-comp-async-jobs-number 4
      native-comp-async-report-warnings-errors 'silent)

(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;;; Disable UI elements early
;; PERF,UI: Doom strives to be keyboard-centric, so I consider these UI elements
;;   clutter. Initializing them also costs a morsel of startup time. Whats more,
;;   the menu bar exposes functionality that Doom doesn't endorse. Perhaps one
;;   day Doom will support these, but today is not that day.
;;
;; HACK: I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because they do extra work to manipulate frame variables
;;   that isn't necessary this early in the startup process.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq server-client-instructions nil)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; FIX: On MacOS, disabling the menu bar makes MacOS treat Emacs as a
;;   non-application window -- which means it doesn't automatically capture
;;   focus when it is started, among other things, so enable the menu-bar for
;;   GUI frames, but keep it disabled in terminal frames because there it
;;   activates an ugly, in-frame menu bar.
(when *is-a-mac*
  (add-hook 'window-setup-hook 'restore-menu-bar-in-gui-frames-h)
  (add-hook 'after-make-frame-functions 'restore-menu-bar-in-gui-frames-h)
  (defun restore-menu-bar-in-gui-frames-h (&optional frame)
    (let ((use-frame (or frame (selected-frame))))
      (when (display-graphic-p use-frame)
        (set-frame-parameter use-frame 'menu-bar-lines 1)))))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(setq user-full-name    "Jacopo Costantini"
      user-mail-address "jacopocostantini32@gmail.com")

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(advice-add #'x-apply-session-resources :override #'ignore)

(setq desktop-restore-forces-onscreen nil)

(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

(setq default-input-method nil)
(global-so-long-mode 1)

(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))

(setq window-resize-pixelwise t
      initial-major-mode 'fundamental-mode
      initial-scratch-message ";; Happy Hacking\n\n")

(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Reduce the frequency of garbage collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 8000000)
     (when (boundp 'after-focus-change-function)
       (add-function :after
                     after-focus-change-function
                     #'+gc-after-focus-change)))))

(add-hook 'after-init-hook '+reset-init-values)

(provide 'early-init)
;;; early-init.el ends here
