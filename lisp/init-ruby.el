;;; init-ruby.el --- Ruby Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package inf-ruby
  :hook ((ruby-mode    . inf-ruby-minor-mode)
         (ruby-ts-mode . inf-ruby-minor-mode)))

(use-package rvm
  :if (eq *rvm-installed* t)
  :after inf-ruby
  :config
  (advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby))

(use-package robe
  :hook ((ruby-mode    . robe-mode)
         (ruby-ts-mode . robe-mode))
  :config
  ;; (eval-after-load 'company
  ;;   '(push 'company-robe company-backends))
  )

(use-package bundler
  :after ruby-mode)

(use-package rspec-mode
  ;; :diminish rspec-mode
  :hook ruby-mode
  :config
  (setq rspec-use-rake-when-possible nil))

(use-package yari
  :after ruby-mode
  :bind (:map ruby-mode-map
              ("C-c k" . yari)))

(use-package rubocop
  :hook ((ruby-mode    . rubocop-mode)
         (ruby-ts-mode . rubocop-mode)))

(provide 'init-ruby)
;;; init-ruby.el ends here
