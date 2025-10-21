;;; system-packages-ext.el --- Version checking for use-package -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name
;; Author: Your Name <your.email@example.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (use-package "2.4") (system-packages "1.0") (use-package-ensure-system-package "0.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/yourusername/system-packages-ext

;;; Commentary:
;;
;; This package "advises" (wraps) the original
;; 'use-package-ensure-system-package' to add support for
;; version-checked dependencies.
;;
;; It advises BOTH the normalizer and the handler to
;; transparently support the new spec format.
;;
;; Usage:
;;   (use-package system-packages-ext
;;     :after use-package-ensure-system-package)
;;
;;   (use-package some-package
;;     :ensure-system-package
;;     (((ruby :version "3.4") . "mise use -g ruby@3.4")
;;      (rubocop . "gem install rubocop")
;;      ripgrep))

;;; Code:

(require 'use-package)
(require 'system-packages)
(require 'use-package-ensure-system-package)

;;;; Helper Functions (Sincrone e Robuste)

(defun system-packages-ext--run-shell-sync (cmd)
  "Run shell CMD string synchronously.
Return non-nil on success (exit code 0)."
  (message "system-packages: Running: %s" cmd)
  (zerop (shell-command cmd nil nil)))

(defun system-packages-ext--check-version (binary required-version &optional version-flag)
  "Check if BINARY version is >= REQUIRED-VERSION.
Returns non-nil if requirement is met."
  (let* ((flag (or version-flag "--version"))
         (cmd (format "%s %s 2>&1"
                      (shell-quote-argument (symbol-name binary))
                      flag))
         (output (shell-command-to-string cmd))
         (found-version (when (string-match "\\([0-9]+\\(\\.[0-9]+\\)+\\)" output)
                          (match-string 1 output))))
    (if (not found-version)
        (progn
          (warn "system-packages-ext: Cannot parse version for %s from: %s" binary output)
          nil)
      (not (version-list-< (version-to-list found-version)
                           (version-to-list required-version))))))

(defun system-packages-ext--ensure-versioned (binary version cmd &optional version-flag)
  "Synchronously ensure BINARY exists with version >= VERSION.
If missing or outdated, run CMD."
  (unless (and (executable-find (symbol-name binary))
               (system-packages-ext--check-version binary version version-flag))
    (message "system-packages-ext: Ensuring %s >= %s..." binary version)
    (unless (system-packages-ext--run-shell-sync cmd)
      (error "Failed to ensure %s (command: %s)" binary cmd))))


;;;; Advice 1: Il Normalizer (Il tuo codice, che funziona)

(defun system-packages-ext--advice-consify (orig-fun arg)
  "Around advice for `use-package-ensure-system-package-consify'.
Handles extended version spec, otherwise calls original function."
  (if (and (consp arg)
           (listp (car arg))
           (symbolp (caar arg))
           (plist-member (cdar arg) :version)
           (stringp (cdr arg)))
      ;; SÌ: Nostro formato.
      (let* ((spec-list (car arg))
             (binary (car spec-list))
             (plist (cdr spec-list))
             (version (plist-get plist :version))
             (version-flag (plist-get plist :version-flag))
             (cmd (cdr arg)))
        ;; Normalizza in (CHIAVE . CODICE-LISP)
        (cons binary
              `(system-packages-ext--ensure-versioned
                ',binary ,version ,cmd ,version-flag)))
    ;; NO: Formato originale. Chiama la funzione originale.
    (funcall orig-fun arg)))

(advice-add 'use-package-ensure-system-package-consify
            :around #'system-packages-ext--advice-consify)


;;;; Advice 2: Il Handler (LA CORREZIONE)

(defun system-packages-ext--advice-handler (orig-fun name keyword arg rest state)
  "Around advice for `use-package-handler/:ensure-system-package'.
Bypasses the original `unless exists?' check for our custom commands."
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar
      (lambda (cons)
        (let ((key (car cons))
              (form (cdr cons)))
          ;; Controlla se 'form' è il nostro comando speciale.
          (if (and (listp form)
                   (eq (car form) 'system-packages-ext--ensure-versioned))
              ;; SÌ: È il nostro. Esegui il 'form' DIRETTAMENTE,
              ;; senza il wrapper 'unless'.
              form
            ;; NO: È un comando originale. Usa la logica originale.
            `(unless (use-package-ensure-system-package-exists? ',key)
               ,form))))
      arg)
     body)))

;; Sostituiamo l'handler originale con il nostro wrapper
(advice-add 'use-package-handler/:ensure-system-package
            :around #'system-packages-ext--advice-handler)

(provide 'system-packages-ext)
;;; system-packages-ext.el ends here
