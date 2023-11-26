;;; early-init.el -- Pre INIT ;; -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq read-process-output-max (* 2 1024 1024)) ; 2 MiB

(provide 'early-init)
;;; early-init.el ends here
