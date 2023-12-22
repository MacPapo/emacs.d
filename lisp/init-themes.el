;;; init-themes.el --- Theme Config ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package modus-themes)
(use-package base16-theme)

(load-theme 'base16-ashes t)

(set-face-attribute 'default nil :font "Iosevka" :height 110)

(provide 'init-themes)
;;; init-themes.el ends here
