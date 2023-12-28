;;; init-lisp.el --- Lisp Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; Paredit Cheat Sheet

;; Navigation
;; Go to the opening parenthesis: M-C-u (paredit-backward-up)
;; Go to the closing parenthesis: M-C-d (paredit-forward-down)

;; Parentheses and Quotes Manipulation
;; Insert balanced parentheses: M-( (paredit-wrap-round)
;; Slurp a parenthesis forward: C-) (paredit-forward-slurp-sexp)
;; Slurp a parenthesis backward: C-( (paredit-backward-slurp-sexp)
;; Barf a parenthesis forward: C-} (paredit-forward-barf-sexp)
;; Barf a parenthesis backward: C-{ (paredit-backward-barf-sexp)
;; Surround with quotes: M-\" (paredit-meta-doublequote)

;; Deletion and Killing
;; Delete a character forward: C-d (paredit-forward-delete)
;; Delete a character backward: DEL (paredit-backward-delete)
;; Kill a line (keeping parentheses balanced): C-k (paredit-kill)

;; Splitting and Joining
;; Split an s-expression: M-S (paredit-split-sexp)
;; Join two s-expressions: M-J (paredit-join-sexps)
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode) . paredit-mode))

(use-package elisp-slime-nav
  :hook ((emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
         (ielm-mode       . turn-on-elisp-slime-nav-mode)))

(use-package slime
  :commands slime
  :config
  ;; (setq inferior-lisp-program "/path/to/your/lisp-implementation") ; es. sbcl, clisp, ecc.
  (setq slime-contribs '(slime-fancy slime-company)))

(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
        slime-company-after-completion 'slime-company-just-one-space))

(provide 'init-lisp)
;;; init-lisp.el ends here
