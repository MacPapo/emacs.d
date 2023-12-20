;;; init-dart.el --- Dart Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dart-mode)
(use-package dart-server
  :bind (:map dart-mode-map
              ("C-c C-o" . dart-server-format))
  :config
  (setq dart-server-enable-analysis-server t))

(use-package flutter
  :hook dart-mode
  :bind ("C-M-x" . flutter-run-or-hot-reload)
  :config
  (setq flutter-sdk-path "~/FlutterDev/flutter/"))

(provide 'init-dart)
;;; init-dart.el ends here
