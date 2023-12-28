;;; init-data.el --- Data Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package csv-mode
  :hook (csv-mode . csv-guess-set-separator))

(provide 'init-data)
;;; init-data.el ends here
