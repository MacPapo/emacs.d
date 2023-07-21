;; -*- lexical-binding: t; -*-

;; Diff Hl Config

(setq diff-hl-draw-borders nil  ; disattiva il disegno dei bordi attorno ai cambiamenti
      diff-hl-margin-side 'left ; mostra i cambiamenti sul lato sinistro del buffer
      diff-hl-margin-width 1)    ; utilizza una larghezza di 1 per le linee dei cambiamenti

(add-hook 'prog-mode-hook 'diff-hl-margin-mode) ; abilita diff-hl-margin-mode per i file di codice
(add-hook 'prog-mode-hook 'diff-hl-flydiff-mode) ; abilita diff-hl-flydiff-mode per i file di codice

(global-diff-hl-mode)

(provide 'diff-hl-config)
