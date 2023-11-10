;;; init-corfu.el --- Corfu Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'corfu)
(require 'cape)

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold nil) 

;; Settings
(setq corfu-cycle t
      corfu-auto t
      corfu-commit-predicate nil
      corfu-quit-no-match t
      corfu-auto-delay 0
      corfu-auto-prefix 1)

;; Bindings
(define-key corfu-map (kbd "<tab>") 'corfu-complete)
(define-key corfu-map (kbd "TAB") 'corfu-complete)

;; Hooks
(add-hook 'eshell-mode-hook #'(lambda () (setq-local corfu-auto nil)))
(add-hook 'shell-mode-hook  #'(lambda () (setq-local corfu-auto nil)))
(add-hook 'term-mode-hook   #'(lambda () (setq-local corfu-auto nil)))

(global-corfu-mode)

;; CAPE
(global-set-key (kbd "C-c . p") #'completion-at-point)
(global-set-key (kbd "C-c . t") #'complete-tag)
(global-set-key (kbd "C-c . d") #'cape-dabbrev)
(global-set-key (kbd "C-c . h") #'cape-history)
(global-set-key (kbd "C-c . f") #'cape-file)
(global-set-key (kbd "C-c . k") #'cape-keyword)
(global-set-key (kbd "C-c . s") #'cape-symbol)
(global-set-key (kbd "C-c . a") #'cape-abbrev)
(global-set-key (kbd "C-c . l") #'cape-line)
(global-set-key (kbd "C-c . w") #'cape-dict)
(global-set-key (kbd "C-c . \\") #'cape-tex)
(global-set-key (kbd "C-c . _") #'cape-tex)
(global-set-key (kbd "C-c . ^") #'cape-tex)
(global-set-key (kbd "C-c . &") #'cape-sgml)
(global-set-key (kbd "C-c . r") #'cape-rfc1345)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-abbrev)
(add-to-list 'completion-at-point-functions #'yasnippet-capf)


(provide 'init-corfu)
;;; init-corfu.el ends here
