;;; init-misc.el --- Miscellaneous Packages Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/miscellaneous" user-emacs-directory))

(require 'init-docker)

(provide 'init-misc)
;;; init-misc.el ends here
