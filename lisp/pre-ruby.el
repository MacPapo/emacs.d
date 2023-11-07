;;; pre-ruby.el --- Add packages for Ruby Config ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when *rvm-installed* (add-to-list 'package-selected-packages 'rvm))
(add-to-list 'package-selected-packages 'inf-ruby)
(add-to-list 'package-selected-packages 'enh-ruby-mode)
(add-to-list 'package-selected-packages 'robe)
(add-to-list 'package-selected-packages 'rubocop)

(provide 'pre-ruby)
;;; pre-ruby.el ends here
