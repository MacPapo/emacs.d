;;; init-vertico.el --- Vertico Configuration ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'vertico)
(require 'vertico-directory)
(require 'orderless)
(require 'savehist)
(require 'consult)
(require 'marginalia)

;; Vertico Hooks
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(add-hook 'minibuffer-setup-hook           #'vertico-repeat-save)
(add-hook 'minibuffer-setup-hook           #'cursor-intangible-mode)

;; Vertico settings
(setq vertico-resize nil
      vertico-count 12
      vertico-cycle t)
(add-to-list 'savehist-additional-variables 'vertico-repeat-history)

(setq-default completion-in-region-function
              (lambda (&rest args)
                (apply (if vertico-mode
                           #'consult-completion-in-region
                         #'completion--in-region)
                       args)))

;; Vertico bindings
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "RET") 'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") 'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word))

;; Orderless settings
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; Consult Config
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; Consult bindings
(global-set-key (kbd "M-y") 'consult-yank-from-kill-ring)
(global-set-key (kbd "M-Y") 'consult-yank-pop)

(global-set-key (kbd "M-#") 'consult-register-load)
(global-set-key (kbd "M-'") 'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
(global-set-key (kbd "C-M-#") 'consult-register)

;;; PREFIX: C-c
(define-key mode-specific-map (kbd "M-x") 'consult-mode-command)
(define-key mode-specific-map (kbd "h")   'consult-history)
(define-key mode-specific-map (kbd "k")   'consult-kmacro)
(define-key mode-specific-map (kbd "m")   'consult-man)
(define-key mode-specific-map (kbd "i")   'consult-info)
(define-key mode-specific-map [remap Info-search] 'consult-info)

;;; PREFIX: C-x
(define-key ctl-x-map (kbd "M-:") 'consult-complex-command)     ;; orig. repeat-complex-command
(define-key ctl-x-map (kbd "b") 'consult-buffer)                ;; orig. switch-to-buffer
(define-key ctl-x-map (kbd "4 b") 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(define-key ctl-x-map (kbd "5 b") 'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
(define-key ctl-x-map (kbd "t b") 'consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
(define-key ctl-x-map (kbd "r b") 'consult-bookmark)            ;; orig. bookmark-jump
(define-key ctl-x-map (kbd "p b") 'consult-project-buffer)      ;; orig. project-switch-to-buffer

;;; PREFIX: M-g
(define-key goto-map (kbd "e")  'consult-compile-error)
(define-key goto-map (kbd "f")  'consult-flymake)               ;; Alternative: consult-flycheck
(define-key goto-map (kbd "g")  'consult-goto-line)             ;; orig. goto-line
(define-key goto-map (kbd "M-g")  'consult-goto-line)           ;; orig. goto-line
(define-key goto-map (kbd "o")  'consult-outline)               ;; Alternative: consult-org-heading
(define-key goto-map (kbd "m")  'consult-mark)
(define-key goto-map (kbd "k")  'consult-global-mark)
(define-key goto-map (kbd "i")  'consult-imenu)
(define-key goto-map (kbd "I")  'consult-imenu-multi)

;;; PREFIX: M-s
(define-key search-map (kbd "d") 'consult-find)                  ;; Alternative: consult-fd
(define-key search-map (kbd "c") 'consult-locate)
(define-key search-map (kbd "g") 'consult-grep)
(define-key search-map (kbd "G") 'consult-git-grep)
(define-key search-map (kbd "r") 'consult-ripgrep)
(define-key search-map (kbd "l") 'consult-line)
(define-key search-map (kbd "L") 'consult-line-multi)
(define-key search-map (kbd "k") 'consult-keep-lines)
(define-key search-map (kbd "u") 'consult-focus-lines)
(define-key search-map (kbd "e") 'consult-isearch-history)

;;; PREFIX: C-s o C-r
(define-key isearch-mode-map (kbd "M-e")     'consult-isearch-history)         ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s e")   'consult-isearch-history)       ;; orig. isearch-edit-string
(define-key isearch-mode-map (kbd "M-s l")   'consult-line)                  ;; needed by consult-line to detect isearch
(define-key isearch-mode-map (kbd "M-s L")   'consult-line-multi)            ;; needed by consult-line to detect isearch

(define-key minibuffer-local-map (kbd "M-s") 'consult-history)                 ;; orig. next-matching-history-element
(define-key minibuffer-local-map (kbd "M-r") 'consult-history)                ;; orig. previous-matching-history-element

(vertico-mode)
(marginalia-mode)

(provide 'init-vertico)
;;; init-vertico.el ends here
