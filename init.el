;; -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(load-theme 'modus-operandi t)
(require 'better-defaults)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package (quote (smex
                         ido-completing-read+
                         pulsar
                         ace-window
                         diminish
                         crux
                         aggressive-indent
                         highlight-thing
                         smart-hungry-delete
                         multiple-cursors
                         easy-kill
                         browse-kill-ring
                         magit
                         typescript-mode
                         diff-hl
                         workgroups2
                         which-key)))
  (unless (package-installed-p package)
    (package-install package)))

(require 'ido-config)
(require 'smex-config)
(require 'pulsar-config)
(require 'ace-window-config)
(require 'diminish-config)
(require 'crux-config)
(require 'aggressive-indent-config)
(require 'highlight-thing-config)
(require 'smart-hungry-delete-config)
(require 'multiple-cursors-config)
(require 'easy-kill-config)
(require 'browse-kill-ring-config)
(require 'magit-config)
(require 'diff-hl-config)
(require 'workgroups-config)
(require 'which-key-config)

