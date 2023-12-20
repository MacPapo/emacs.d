;;; init-themes.el --- Theme Config ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package modus-themes)
(use-package base16-theme)

(load-theme 'base16-ashes t) ; bello
;; (load-theme 'base16-ayu-dark t) ; non male
;; (load-theme 'base16-ayu-mirage t) ; non male
;; (load-theme 'base16-black-metal-bathory t) ; bianco e nero non male
;; (load-theme 'base16-black-metal-burzum t) ; bianco e nero non male
;; (load-theme 'base16-black-metal-dark-funeral t) ; bianco e nero non male
;; (load-theme 'base16-black-metal-gorgoroth t) ; bianco e nero non male ma spento
;; (load-theme 'base16-black-metal-immortal t) ; bianco e nero non male ma spento
;; (load-theme 'base16-black-metal-khold t) ; bianco e nero non male, mette in risalto i tipi di dato (in rosso)
;; (load-theme 'base16-black-metal-marduk t) ; bianco e nero non male troppo spento, non si capisce nulla
;; (load-theme 'base16-black-metal-mayhem t) ; bianco e nero non male troppo spento, mette in risalto i tipi di dato (in giallo)
;; (load-theme 'base16-black-metal-nile t) ; bianco e nero non male troppo spento, mette in risalto i tipi di dato piu' o meno
;; (load-theme 'base16-black-metal t) ; non male
;; (load-theme 'base16-black-metal-venom t) ; cosi cosi
;; (load-theme 'base16-catppuccin t) ; niente male
;; (load-theme 'base16-catppuccin-frappe t) ; niente male!
;; (load-theme 'base16-catppuccin-macchiato t) ; niente male simile a doom!
;; (load-theme 'base16-catppuccin-mocha t) ; niente male!
;; (load-theme 'base16-chalk t) ; niente male!
;; (load-theme 'base16-classic-dark t) ; niente male!
;; (load-theme 'base16-circus t) ; niente male!!
;; (load-theme 'base16-da-one-black t) ; niente male, ottimo!!
;; (load-theme 'base16-da-one-ocean t) ; niente male
;; (load-theme 'base16-da-one-sea t) ; niente male
;; (load-theme 'base16-danqing t) ; carino
;; (load-theme 'base16-darcula t) ; carino
;; (load-theme 'base16-darkmoss t) ; niente male
;; (load-theme 'base16-decaf t) ; niente male
;; (load-theme 'base16-eighties t) ; niente male
;; (load-theme 'base16-embers t) ; cosi cosi
;; (load-theme 'base16-espresso t) ; cosi cosi
;; (load-theme 'base16-evenok-dark t) ; non male
;; (load-theme 'base16-everforest-dark-hard t) ; non male
;; (load-theme 'base16-framer t) ; non male
;; (load-theme 'base16-gigavolt t) ; cosi cosi
;; (load-theme 'base16-gruber t) ; nulla di che
;; (load-theme 'base16-gruvbox-dark-soft t) ; non male
;; (load-theme 'base16-gruvbox-dark-hard t) ; non male!!
;; (load-theme 'base16-gruvbox-material-dark-hard t) ; non male
;; (load-theme 'base16-helios t) ; non male
;; (load-theme 'base16-horizon-dark t) ; non male!!
;; (load-theme 'base16-horizon-terminal-dark t) ; non male!!!
;; (load-theme 'base16-ia-dark t) ; non male!!!
;; (load-theme 'base16-irblack t) ; non male!
;; (load-theme 'base16-materia t)

(set-face-attribute 'default nil :font "Iosevka" :height 110)

(provide 'init-themes)
;;; init-themes.el ends here
