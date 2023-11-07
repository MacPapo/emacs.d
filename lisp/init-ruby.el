;;; init-ruby.el --- Ruby Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'enh-ruby-mode)

;; Hooks for Enhanced Ruby Mode
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

;; Inferior Ruby Mode
(require 'inf-ruby)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

;; Robe Mode
(require 'robe)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

;; Rubocop
(require 'rubocop)
(add-hook 'enh-ruby-mode-hook #'rubocop-mode)

;; If RVM is installed then
(when *rvm-installed*
  (require 'rvm)
  (advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby))

(provide 'init-ruby)
;;; init-ruby.el ends here
