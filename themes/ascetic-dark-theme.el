;;; ascetic-dark-theme.el --- Ascetic Dark Theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026 by Jacopo Costantini
;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Ascetic: Clarity through renunciation.
;; A high-contrast, low-noise environment for deep cognition.

;;; Code:

;; Inject current directory into load-path to find ascetic-core
(eval-and-compile
  (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name))))

(require 'ascetic-core)

(ascetic-build-theme ascetic-dark
  "Clarity through renunciation. A high-contrast, warm low-noise environment."
  ((bg-main              "#1D1B19")
   (fg-main              "#F0E4D7")
   (bg-line              "#282522")
   (bg-modeline          "#2C333B")
   (bg-modeline-inactive "#252321")
   (border               "#4A4540")
   (comment              "#857D74")
   (string               "#9BB096")
   (constant             "#95A3BF")
   (error                "#D67C7C")
   (warning              "#D1A86B")
   (prompt               "#72A69A")
   (region               "#3B424D")))

;;; ascetic-dark-theme.el ends here
