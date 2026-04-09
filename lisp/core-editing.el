;;; core-editing.el --- Surgical text manipulation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; A surgical editing module, ultra-fast and dependency-free.
;; Combines Vim's determinism (Inner selections) with modern editor
;; ergonomics (DWIM, Line moving, Instant surround).

;;; Code:

;; ==========================================
;; 1. LINE MANIPULATION
;; ==========================================

;; Note: `duplicate-dwim` is native in Emacs 29+, bound in Section 4.

(defun core-edit-move-up-dwim ()
  "Move the selected line or lines up with mathematical precision."
  (interactive)
  (let* ((is-region (use-region-p))
         (beg (if is-region
                  (save-excursion (goto-char (region-beginning)) (line-beginning-position))
                (line-beginning-position)))
         (end (if is-region
                  (save-excursion
                    (goto-char (region-end))
                    (if (and (bolp) (> (point) beg)) (point) (line-beginning-position 2)))
                (line-beginning-position 2))))
    (if (= beg (point-min))
        (user-error "Core: Already at the first line!")
      (let ((pt-offset (- (point) beg))
            (mk-offset (when is-region (- (mark) beg)))
            (text (delete-and-extract-region beg end)))
        (forward-line -1)
        (let ((insert-pos (point)))
          (insert text)
          (if is-region
              (progn
                (push-mark (+ insert-pos mk-offset) t t)
                (goto-char (+ insert-pos pt-offset))
                (setq deactivate-mark nil))
            (goto-char (+ insert-pos pt-offset))))))))

(defun core-edit-move-down-dwim ()
  "Move the selected line or lines down with mathematical precision."
  (interactive)
  (let* ((is-region (use-region-p))
         (beg (if is-region
                  (save-excursion (goto-char (region-beginning)) (line-beginning-position))
                (line-beginning-position)))
         (end (if is-region
                  (save-excursion
                    (goto-char (region-end))
                    (if (and (bolp) (> (point) beg)) (point) (line-beginning-position 2)))
                (line-beginning-position 2))))
    (if (= end (point-max))
        (user-error "Core: Already at the last line!")
      (let ((pt-offset (- (point) beg))
            (mk-offset (when is-region (- (mark) beg)))
            (text (delete-and-extract-region beg end)))
        (forward-line 1)
        (let ((insert-pos (point)))
          (insert text)
          (if is-region
              (progn
                (push-mark (+ insert-pos mk-offset) t t)
                (goto-char (+ insert-pos pt-offset))
                (setq deactivate-mark nil))
            (goto-char (+ insert-pos pt-offset))))))))

(defun core-edit-open-line-below ()
  "Open a new line below the current one and indent (Vim: o)."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun core-edit-open-line-above ()
  "Open a new line above the current one and indent (Vim: O)."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun core-edit-join-line-below ()
  "Join the line below to the current one (Vim: J)."
  (interactive)
  (delete-indentation t))

;; ==========================================
;; 2. SURGICAL SELECTIONS (The "Nouns")
;; ==========================================

(defun core-edit-select-inner-symbol ()
  "Select the symbol or word under the cursor (Vim: viw)."
  (interactive)
  (if-let* ((bounds (or (bounds-of-thing-at-point 'symbol)
                        (bounds-of-thing-at-point 'word))))
      (progn
        (goto-char (car bounds))
        (push-mark (point) t t)
        (goto-char (cdr bounds)))
    (user-error "Core: No symbol under cursor!")))

(defun core-edit-select-inner-quotes ()
  "Select the text content inside current quotes (Vim: vi\")."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (start-quote (nth 8 ppss)))
    (if (nth 3 ppss) ; true if we are inside a string
        (progn
          (goto-char start-quote)
          (forward-sexp)
          (let ((end-quote (point)))
            (goto-char (1+ start-quote))
            (push-mark (point) t t)
            (goto-char (1- end-quote))))
      (user-error "Core: Not inside a string!"))))

(defun core-edit-select-inner-parens ()
  "Select the content inside the parentheses block (Vim: vi())."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (start-paren (nth 1 ppss)))
    (if start-paren
        (progn
          (goto-char start-paren)
          (forward-sexp)
          (let ((end-paren (point)))
            (goto-char (1+ start-paren))
            (push-mark (point) t t)
            (goto-char (1- end-paren))))
      (user-error "Core: Not inside any parentheses!"))))

;; ==========================================
;; 3. ADVANCED MANIPULATIONS (The "Verbs")
;; ==========================================

(defun core-edit-change-dwim ()
  "Vaporize the active selection. If no selection, vaporize the symbol.
Replaces Vim's 'c' behavior for selections and 'ciw' for words."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (if-let* ((bounds (or (bounds-of-thing-at-point 'symbol)
                          (bounds-of-thing-at-point 'word))))
        (delete-region (car bounds) (cdr bounds))
      (user-error "Core: No target to vaporize here!"))))

(defun core-edit-surround-dwim (char)
  "Surround the active selection or the symbol under the cursor with CHAR."
  (interactive "cSurround with: ")
  (let (beg end)
    (if (use-region-p)
        (setq beg (region-beginning) end (region-end))
      (if-let* ((bounds (or (bounds-of-thing-at-point 'symbol)
                            (bounds-of-thing-at-point 'word))))
          (setq beg (car bounds) end (cdr bounds))
        (user-error "Core: No text to surround here!")))

    (let* ((char-str (char-to-string char))
           (close-char (if-let* ((pair (alist-get char '((?\( . ?\))
                                                         (?\[ . ?\])
                                                         (?\{ . ?\})
                                                         (?<  . ?>)))))
                           (char-to-string pair)
                         char-str)))
      (save-excursion
        ;; Insert at the end first to avoid offsetting coordinates
        (goto-char end)
        (insert close-char)
        (goto-char beg)
        (insert char-str))
      (deactivate-mark)
      (message "Core: Surrounded with %s and %s" char-str close-char))))

;; ==========================================
;; 4. KEYBINDINGS
;; ==========================================

(defvar-keymap core-edit-inner-map
  :doc "Keymap for inner selections (text objects)."
  "s" #'core-edit-select-inner-symbol  ; Symbol/Word
  "q" #'core-edit-select-inner-quotes  ; Quotes
  "p" #'core-edit-select-inner-parens) ; Parens

;; --- Lines & General Editing ---
(keymap-global-set "C-c d"        #'duplicate-dwim)
(keymap-global-set "M-S-<down>"   #'duplicate-dwim)
(keymap-global-set "M-<up>"       #'core-edit-move-up-dwim)
(keymap-global-set "M-<down>"     #'core-edit-move-down-dwim)
(keymap-global-set "C-<return>"   #'core-edit-open-line-below)
(keymap-global-set "C-S-<return>" #'core-edit-open-line-above)
(keymap-global-set "M-J"          #'core-edit-join-line-below)

;; --- Actions (Verbs) & Selections (Nouns) ---
(keymap-global-set "M-k"          #'core-edit-change-dwim)
(keymap-global-set "M-'"          #'core-edit-surround-dwim)
(keymap-global-set "M-i"          core-edit-inner-map)
(keymap-global-set "M-z"          #'zap-up-to-char)

;; --- Buffers ---
(keymap-global-set "C-x K"        #'kill-current-buffer)

(provide 'core-editing)
;;; core-editing.el ends here
