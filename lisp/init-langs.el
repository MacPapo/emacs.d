;;; init-langs.el --- Programming Languages Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/languages" user-emacs-directory))

(require 'init-ruby)
(require 'init-dart)
(require 'init-sql)

(provide 'init-langs)
;;; init-langs.el ends here
