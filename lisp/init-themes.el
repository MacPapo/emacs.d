;;; init-themes.el --- Theme Config ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-vivendi t)
  (set-face-attribute 'default nil :font "Iosevka" :height 130))

(provide 'init-themes)
;;; init-themes.el ends here
