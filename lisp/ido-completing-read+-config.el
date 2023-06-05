;; -*- lexical-binding: t; -*-
;; Ido Completing Read+ Config

(require 'ido-completing-read+)

;; Enable IDO mode
(ido-mode 1)

;; Enhance IDO with additional features
(ido-everywhere)
(ido-ubiquitous-mode 1)

;; Customize IDO
(setq ido-enable-flex-matching t
      ido-use-faces nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-enable-prefix nil
      ido-default-buffer-method 'selected-window)

(provide 'ido-completing-read+-config)
