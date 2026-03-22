;;; early-init.el --- Early Startup Optimization -*- lexical-binding: t; -*-

;;; Commentary:
;; Boot ultra-veloce ispirato a minimal-emacs.d e Doom.
;; Modifica l'ambiente prima che la GUI o i pacchetti vengano caricati.

;;; Code:

;; ==========================================
;; 1. PACKAGE MANAGER & GARBAGE COLLECTOR
;; ==========================================
(setq package-enable-at-startup nil)

;; Disattiviamo il GC durante il boot (sia threshold che percentage)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

;; ==========================================
;; 2. OTTIMIZZAZIONE I/O (Il trucco più potente)
;; ==========================================
;; Disabilitiamo le regex sui nomi dei file durante l'avvio.
(defvar zen--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Lo ripristiniamo a boot finito, unendolo a eventuali handler caricati nel frattempo.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist
                  (delete-dups (append file-name-handler-alist zen--file-name-handler-alist)))))

;; ==========================================
;; 3. PRE-RENDERING GUI E FLASH PREVENTION
;; ==========================================
;; Spegniamo i fronzoli prima che la finestra venga disegnata
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tooltip-mode nil
      use-dialog-box nil
      use-file-dialog nil)

;; Evita che Emacs tenti di ridimensionarsi in base alla griglia dei caratteri
(setq frame-inhibit-implied-resize t)

;; Utile per i Tiling Window Manager o macOS per evitare gap strani
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;; ==========================================
;; 4. CORE PERFORMANCE (Testo e Font)
;; ==========================================
;; Evita operazioni intensive di compattazione memoria per i font
(setq inhibit-compacting-font-caches t)

;; Disabilita la scansione del testo bidirezionale (Arab/Hebrew) per un re-display più veloce
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; ==========================================
;; 5. SILENZIAMENTO MESSAGGI NATIVI
;; ==========================================
;; Spegne gli avvisi fastidiosi di native-comp compilazione asincrona
(setq native-comp-async-report-warnings-errors 'silent)

;;; early-init.el ends here
