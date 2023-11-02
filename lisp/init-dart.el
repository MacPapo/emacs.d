;;; init-dart.el --- Dart Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'dart-mode)
(require 'dart-server)
(require 'flutter)

;; Flutter config
(setq flutter-sdk-path "~/FlutterDev/flutter/")
(global-set-key (kbd "C-M-x") #'flutter-run-or-hot-reload)

;; Flutter Hooks
(add-hook 'dart-mode-hook #'flutter-test-mode)

;; Dart config
(with-eval-after-load "dart-mode"
  (define-key dart-mode-map (kbd "C-c C-o") 'dart-server-format))

;; Dart Server config
(setq dart-server-enable-analysis-server t)

(provide 'init-dart)
;;; init-dart.el ends here
