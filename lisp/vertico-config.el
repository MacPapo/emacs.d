;; -*- lexical-binding: t; -*-

(require 'vertico)
(require 'vertico-directory)
(require 'vertico-quick)

(setq vertico-cycle t)

(keymap-set vertico-map "?" #'minibuffer-completion-help)
(keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
(keymap-set vertico-map "S-TAB" #'minibuffer-complete)

;; More convenient directory navigation commands
(define-key vertico-map (kbd "RET") 'vertico-directory-enter)
(define-key vertico-map (kbd "DEL") 'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word)

;; Tidy shadowed file names
(add-hook 'rfn-eshadow-update-overlay-hook 'vertico-directory-tidy)

(keymap-set vertico-map "M-q" #'vertico-quick-insert)
(keymap-set vertico-map "C-q" #'vertico-quick-exit)


(vertico-mode)

(provide 'vertico-config)
