;; -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load Theme
(load-theme 'modus-operandi t)

;; Load Better defaults
(require 'better-defaults)

;; Initialize the package manager
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(when (member "IBM Plex Mono" (font-family-list))
  (custom-set-faces
   '(default ((t (:family "IBM Plex Mono" :height 125))))))

(mapc (lambda (x)
        (unless (package-installed-p x)
          (package-install x))
        (require (intern (concat (symbol-name x)
                                 "-config"))))
      '(neotree
        vertico
        corfu
        pulsar
        ace-window
        diminish
        crux
        aggressive-indent
        multiple-cursors
        magit
        diff-hl
        which-key
        eglot
        marginalia
        consult
        typescript-mode
        dart-mode))
