;;; init-defaults.el --- Better Emacs Defaults ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package diminish)

(use-package sudo-edit)

(use-package helpful)

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package savehist
  :ensure nil
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring))
  (setq savehist-autosave-interval 60)
  (savehist-mode +1))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 600
        recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                          "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                          ".*png$" ".*cache$"))
  (recentf-mode 1))

(use-package winner
  :ensure nil
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)
  (winner-mode 1))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-ignore-buffers-re "^\\*"))

(use-package display-line-numbers
  :ensure nil
  :hook prog-mode
  :config
  (setq-default
   display-line-numbers-width 3
   display-line-numbers-type 'relative))

(use-package display-fill-column-indicator
  :ensure nil
  :hook prog-mode
  :config
  (setq-default
   indicate-buffer-boundaries 'left
   display-fill-column-indicator-character ?┊))

(use-package dired
  :ensure nil
  :config
  (when *is-a-mac* (setq insert-directory-program "gls"))

  (setq dired-listing-switches "-laGh1v"
        list-directory-brief-switches "-CFh"
        list-directory-verbose-switches "-lhG"
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t))

(use-package avy
  :bind ("C-j" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.25))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :hook prog-mode
  :diminish rainbow-mode)

;; Smartparens
;; (require 'smartparens-config)
;; (setq show-paren-delay 0)
;; (setq sp-highlight-pair-overlay nil)
;; (add-hook 'prog-mode-hook #'smartparens-mode)
;; (add-hook 'text-mode-hook #'smartparens-mode)

(provide 'init-defaults)
;;; init-defaults.el ends here
