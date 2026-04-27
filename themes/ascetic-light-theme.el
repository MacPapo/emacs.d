;;; ascetic-light-theme.el --- Ascetic Light Theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026 by Jacopo Costantini
;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Ascetic: Clarity through renunciation.
;; Structure is defined by form. Color is reserved strictly for signal.
;; The digital vellum for high-contrast, low-noise environments.

;;; Code:

;; Inject current directory into load-path to find ascetic-core
(eval-and-compile
  (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name))))

(require 'ascetic-core)

(ascetic-build-theme ascetic-light
  "Clarity through renunciation. Syntax is structure; color is signal."
  ((bg-main              "#F0EAD6")
   (fg-main              "#242424")
   (bg-line              "#E6DFCA")
   (bg-modeline          "#E8E4D8")
   (bg-modeline-inactive "#D4CDB8")
   (border               "#C4C4BC")
   (comment              "#585858")
   (string               "#3A592D")
   (constant             "#4A5068")
   (error                "#9E3636")
   (warning              "#7A5010")
   (prompt               "#3A4A48")
   (region               "#D5E1ED")))

;;; ascetic-light-theme.el ends here
