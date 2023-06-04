;; -*- lexical-binding: t; -*-

;; Smex Config
;;
;; Remember:
;; C-h f  while Smex is active, runs describe-function on the currently selected command.
;; M-.    jumps to the definition of the selected command.
;; C-h w  shows the key bindings for the selected command. (Via where-is.)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(setq smex-history-length 10)
(setq smex-prompt-string "Boss? ")

(provide 'smex-config)
