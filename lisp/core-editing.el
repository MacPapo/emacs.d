;;; core-editing.el --- Surgical text manipulation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jacopo Costantini
;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;;
;; A surgical editing module, ultra-fast and dependency-free.
;; Combines Vim's determinism (Inner selections) with modern editor
;; ergonomics (DWIM, Line moving, Instant surround).
;; Architecture is separated into: Text Objects (Registry), Nouns (Selections),
;; and Verbs (Transformations).

;;; Code:

;;; TEXT OBJECT REGISTRY

(defun core-edit--bounds-for (type)
  "Registry for text object bounds. TYPE is 'symbol, 'string, or 'parens.
Returns (BEG . END) of the inner object, or nil if not found."
  (pcase type
    ('symbol
     (or (bounds-of-thing-at-point 'symbol)
         (bounds-of-thing-at-point 'word)))
    ('string
     (let ((state (syntax-ppss)))
       (when (nth 3 state)
         (save-excursion
           (goto-char (nth 8 state))
           (let ((inner-beg (1+ (point))))
             (forward-sexp 1)
             (let ((inner-end (1- (point))))
               (when (> inner-end inner-beg)
                 (cons inner-beg inner-end))))))))
    ('parens
     (let ((state (syntax-ppss)))
       (when-let ((start-paren (nth 1 state)))
         (save-excursion
           (goto-char start-paren)
           (let ((inner-beg (1+ (point))))
             (forward-sexp 1)
             (let ((inner-end (1- (point))))
               (when (> inner-end inner-beg)
                 (cons inner-beg inner-end))))))))))

(defun core-edit--bounds-dwim ()
  "Return (BEG . END) of active region or fallback to symbol under point.
Signals a user-error if no target is found."
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (core-edit--bounds-for 'symbol))))
    (unless bounds
      (user-error "Core: No target text found!"))
    bounds))

;;; ADVANCED MANIPULATIONS (Verbs)

(defun core-edit-delete-dwim ()
  "Vaporize selection or symbol under cursor.
NOTE: Uses delete-region to avoid polluting the kill-ring (Vim's black-hole)."
  (interactive)
  (let ((bounds (core-edit--bounds-dwim)))
    (delete-region (car bounds) (cdr bounds))))

(defun core-edit-surround-dwim (char)
  "Surround target with CHAR. Uses Atomic Undo and fast insertion."
  (interactive "cSurround with: ")
  (atomic-change-group
    (let* ((bounds (core-edit--bounds-dwim))
           (beg (car bounds))
           (end (cdr bounds))
           (pairs '((?\( . ?\))
                    (?\[ . ?\])
                    (?\{ . ?\})
                    (?<  . ?>)))
           (pair-match (alist-get char pairs))
           (close-char (or pair-match char)))
      (save-excursion
        (goto-char end)
        (insert-char close-char)
        (goto-char beg)
        (insert-char char))
      (deactivate-mark))))

;;; SURGICAL SELECTIONS

(defun core-edit-select-inner-symbol ()
  "Select symbol/word (Vim: viw)."
  (interactive)
  (let ((bounds (core-edit--bounds-dwim)))
    (goto-char (car bounds))
    (push-mark (point) t t)
    (goto-char (cdr bounds))))

(defun core-edit-select-inner-quotes ()
  "Select content inside quotes (Vim: vi\"). Bulletproof via syntax-ppss."
  (interactive)
  (if-let ((bounds (core-edit--bounds-for 'string)))
      (progn
        (goto-char (car bounds))
        (push-mark (point) t t)
        (goto-char (cdr bounds)))
    (user-error "Core: Not inside a string!")))

(defun core-edit-select-inner-parens ()
  "Select content inside parentheses (Vim: vi()). Bulletproof via syntax-ppss."
  (interactive)
  (if-let ((bounds (core-edit--bounds-for 'parens)))
      (progn
        (goto-char (car bounds))
        (push-mark (point) t t)
        (goto-char (cdr bounds)))
    (user-error "Core: Not inside any parentheses!")))

;;; LINE MANIPULATION

(defun core-edit-move-up-dwim ()
  "Move the selected line or lines up with atomic undo."
  (interactive)
  (atomic-change-group
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
              (goto-char (+ insert-pos pt-offset)))))))))

(defun core-edit-move-down-dwim ()
  "Move the selected line or lines down with atomic undo."
  (interactive)
  (atomic-change-group
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
              (goto-char (+ insert-pos pt-offset)))))))))

(defun core-edit-duplicate-dwim ()
  "Duplicate current line or region with surgical ergonomics.
- No region: Duplicate line below and move cursor there.
- Single-line region: Duplicate inline with a space.
- Multi-line region: Duplicate block below and select the NEW block."
  (interactive)
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties beg end))
             (is-multiline (> (count-lines beg end) 1)))
        (if is-multiline
            (let (new-beg)
              (goto-char end)
              (unless (bolp) (insert "\n"))
              (setq new-beg (point))
              (insert text)
              (unless (bolp) (insert "\n"))
              (indent-region new-beg (point))
              (push-mark new-beg t t)
              (setq deactivate-mark nil))
          (goto-char end)
          (insert " " text)
          (let ((copy-beg (+ end 1))
                (copy-end (point)))
            (push-mark copy-beg t t)
            (goto-char copy-end)
            (setq deactivate-mark nil))))
    (let ((line (buffer-substring (line-beginning-position) (line-end-position)))
          (col (current-column)))
      (end-of-line)
      (insert "\n" line)
      (move-to-column col))))

(defun core-edit-duplicate-and-comment-dwim ()
  "Wrapper: duplicate and comment the original. Focus stays on the copy."
  (interactive)
  (let ((beg (if (use-region-p) (region-beginning) (line-beginning-position)))
        (end (if (use-region-p) (region-end) (line-end-position))))
    (core-edit-duplicate-dwim)
    (save-excursion
      (comment-region beg end))
    (setq deactivate-mark nil)))

(defun core-edit-open-line-below ()
  "Open a new line below and indent (Vim: o)."
  (interactive) (end-of-line) (newline-and-indent))

(defun core-edit-open-line-above ()
  "Open a new line above and indent (Vim: O)."
  (interactive) (beginning-of-line) (newline) (forward-line -1) (indent-according-to-mode))

(defun core-edit-join-line-below ()
  "Join the line below to the current one (Vim: J)."
  (interactive) (delete-indentation t))


;;; KEYBINDINGS & REPEAT MAPS

(defvar-keymap core-edit-move-repeat-map
  :doc "Repeat map for surgical line moving."
  :repeat t
  "<up>"   #'core-edit-move-up-dwim
  "<down>" #'core-edit-move-down-dwim)

(defvar-keymap core-edit-inner-map
  :doc "Inner selections (text objects)."
  "s" #'core-edit-select-inner-symbol
  "q" #'core-edit-select-inner-quotes
  "p" #'core-edit-select-inner-parens)

;; Global Bindings
(keymap-global-set "C-c d"        #'core-edit-duplicate-dwim)
(keymap-global-set "C-c C-d"      #'core-edit-duplicate-and-comment-dwim)
(keymap-global-set "M-S-<down>"   #'core-edit-duplicate-dwim)
(keymap-global-set "M-<up>"       #'core-edit-move-up-dwim)
(keymap-global-set "M-<down>"     #'core-edit-move-down-dwim)
(keymap-global-set "C-<return>"   #'core-edit-open-line-below)
(keymap-global-set "C-S-<return>" #'core-edit-open-line-above)
(keymap-global-set "M-J"          #'core-edit-join-line-below)
(keymap-global-set "M-k"          #'core-edit-delete-dwim)
(keymap-global-set "M-'"          #'core-edit-surround-dwim)
(keymap-global-set "M-i"          core-edit-inner-map)
(keymap-global-set "M-z"          #'zap-up-to-char)
(keymap-global-set "C-x K"        #'kill-current-buffer)

(provide 'core-editing)
;;; core-editing.el ends here
