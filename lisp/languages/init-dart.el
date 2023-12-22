;;; init-dart.el --- Dart Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dart-mode
  :mode "\\.dart\\'"
  :bind (:map dart-mode-map
              ("C-c C-o" . dart-server-format)
              ("C-M-x"   . flutter-run-or-hot-reload)))

(use-package dart-server
  :after dart-mode
  :hook (dart-server . flycheck-mode))

(use-package flutter
  :after dart-mode
  :config
  (setq flutter-sdk-path "~/FlutterDev/flutter/"))

(provide 'init-dart)
;;; init-dart.el ends here
