;; -*- lexical-binding: t; -*-

;; Easy Kill Config
;;
;; M-w w: save word at point
;; M-w s: save sexp at point
;; M-w l: save list at point (enclosing sexp)
;; M-w d: save defun at point
;; M-w D: save current defun name
;; M-w f: save file at point
;; M-w b: save buffer-file-name or default-directory. - changes the kill to the directory name, + to full name and 0 to basename.
;;
;; @:     append selection to previous kill and exit. For example, M-w d @ will append current function to last kill.
;; C-w:   kill selection and exit
;; +, - and 1..9: expand/shrink selection
;; 0      shrink the selection to the initial size i.e. before any expansion
;; SPC:   cycle through things in easy-kill-alist
;; C-SPC: turn selection into an active region
;; C-g:   abort
;; ?:     help
(global-set-key [remap kill-ring-save] 'easy-kill)

(provide 'easy-kill-config)
