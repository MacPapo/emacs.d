;;; early-init.el --- Early Init Config  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025  Jacopo Costantini

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

(setq-local bk-gc-th gc-cons-threshold)
(setq-local bk-gc-pe gc-cons-percentage)

(setopt gc-cons-threshold most-positive-fixnum
	gc-cons-percentage 1.0)
(add-hook 'after-init-hook #'(lambda () (setopt gc-cons-threshold bk-gc-th
					        gc-cons-percentage bk-gc-pe)))

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setopt frame-resize-pixelwise t)
(setopt frame-inhibit-implied-resize t)
(setopt inhibit-startup-message t)
(setopt visible-bell t)
(setopt use-dialog-box nil)

(setopt read-process-output-max (* 4 1024 1024))
(setopt process-adaptive-read-buffering nil)
(setopt jit-lock-defer-time 0)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Give up some bidirectional functionality for slightly faster re-display.
(setopt bidi-inhibit-bpa t)

(setopt use-package-expand-minimally t)
(setopt use-package-enable-imenu-support t)


;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'early-init)

;;; early-init.el ends here
