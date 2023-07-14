;; -*- lexical-binding: t; -*-

(require 'dart-mode)

(unless (package-installed-p 'dart-server)
  (package-refresh-contents)
  (package-install 'dart-server))

(unless (package-installed-p 'flutter)
  (package-refresh-contents)
  (package-install 'flutter))

(setq dart-sdk-path (concat (getenv "HOME") "FlutterDev/flutter/bin/cache/dark-sdk/")
      dart-server-sdk-path (concat (getenv "HOME") "FlutterDev/flutter/bin/cache/dark-sdk/")
      dart-format-on-save t
      flutter-sdk-path "~/FlutterDev/flutter/"
      dart-server-enable-analysis-server t
      )

(define-key dart-mode-map (kbd "C-M-x") 'flutter-run-or-hot-reload)
(add-hook 'dart-server-hook 'flycheck-mode)

(provide 'dart-mode-config)
