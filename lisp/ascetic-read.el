;;; ascetic-read.el --- Plan 9 inspired, pure text completion engine -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;;
;; Ascetic-read replaces the standard Emacs completion UI with a minimal,
;; stream-based text interface inspired by Unix and Plan 9.
;;
;; PHILOSOPHY & VISION:
;; 1. Text is Reality: The minibuffer prompt is the absolute truth of intent.
;; 2. Decoupled Signals: Composition (M-1..M-9) mutates state. Execution (RET) commits it.
;; 3. Trust the User: Confirmation prompts are considered visual noise.
;; 4. Interruptible Flow: UI rendering never blocks keyboard input.

;;; Code:

(defgroup ascetic-read nil
  "Plan 9 inspired, pure text completion engine."
  :group 'minibuffer)

(defcustom ascetic-max-candidates 5
  "Maximum number of candidates to compute and display. (Max 9)"
  :type 'integer
  :group 'ascetic-read)

(defvar ascetic-input-filter-function #'identity
  "Filter applied to minibuffer stream before engine evaluation.")

;; Session registers
(defvar ascetic--collection nil)
(defvar ascetic--predicate nil)
(defvar ascetic--require-match nil)
(defvar ascetic--default nil)
(defvar ascetic--overlay nil)
(defvar ascetic--current-candidates nil)
(defvar ascetic--current-base-size 0)

;; Pure algorithmic core

(defun ascetic--extract-shortest (lst n)
  "Extract N shortest strings from LST in O(N).
Maintains order via trailing pointer injection to bypass GC thrashing."
  (let ((res nil))
    (while lst
      (let* ((curr (car lst))
             (len-curr (length curr)))
        (cond
         ((or (null res) (<= len-curr (length (car res))))
          (setq res (cons curr res))
          (let ((tail (nthcdr (1- n) res)))
            (when tail (setcdr tail nil))))
         ((or (< (length res) n) (<= len-curr (length (car (last res)))))
          (let ((prev res))
            (while (and (cdr prev) (< (length (cadr prev)) len-curr))
              (setq prev (cdr prev)))
            (setcdr prev (cons curr (cdr prev)))
            (let ((tail (nthcdr (1- n) res)))
              (when tail (setcdr tail nil)))))))
      (setq lst (cdr lst)))
    res))

(defun ascetic--take-n (lst n)
  "Extract up to N elements safely from LST."
  (let ((i n) (res nil))
    (while (and (> i 0) (consp lst))
      (push (car lst) res)
      (setq lst (cdr lst))
      (setq i (1- i)))
    (nreverse res)))

;; I/O pipeline and display

(defun ascetic--update-completions ()
  "Compute completions synchronously. Aborts immediately on pending input.
Binds non-essential to prevent blocking I/O (e.g., TRAMP connections)."
  (when ascetic--overlay
    (let* ((raw-content (minibuffer-contents))
           (content (funcall ascetic-input-filter-function raw-content))
           ;; 'while-no-input' returns evaluated body, or 't' if interrupted.
           (state
            (while-no-input
              (let* ((non-essential t)
                     (completions (completion-all-completions
                                   content ascetic--collection ascetic--predicate (length content)))
                     (last-cell (last completions))
                     (base-size (if (and last-cell (numberp (cdr last-cell)))
                                    (prog1 (cdr last-cell) (setcdr last-cell nil))
                                  0))
                     (lst (if (or (string-empty-p content) (not completions))
                              (ascetic--take-n completions ascetic-max-candidates)
                            (ascetic--extract-shortest completions ascetic-max-candidates))))
                (cons lst base-size)))))

      ;; If state is a cons cell, computation finished uninterrupted.
      (when (consp state)
        (let ((lst (car state))
              (base-size (cdr state)))
          (setq ascetic--current-candidates lst)
          (setq ascetic--current-base-size base-size)
          (move-overlay ascetic--overlay (point-min) (point-max) (current-buffer))

          (if lst
              (let* ((i 0)
                     (text (concat " \n"
                                   (mapconcat (lambda (item)
                                                (setq i (1+ i))
                                                (format "%d. %s" i item))
                                              lst "\n"))))
                ;; Inform C display engine to lock cursor before the overlay.
                (put-text-property 0 1 'cursor t text)
                (overlay-put ascetic--overlay 'after-string text))
            (overlay-put ascetic--overlay 'after-string "")))))))

;; Interactive routes

(defun ascetic--insert-nth (n)
  "Materialize candidate N into the prompt without executing."
  (interactive)
  (let ((candidate (nth n ascetic--current-candidates)))
    (if candidate
        (progn
          (delete-region (+ (minibuffer-prompt-end) ascetic--current-base-size) (point-max))
          (insert candidate))
      (minibuffer-message "No candidate %d" (1+ n)))))

(defun ascetic--submit-raw ()
  "Commit exact prompt state. Validates only if match is strictly mandated."
  (interactive)
  (let ((input (minibuffer-contents)))
    (cond
     ((and (string-empty-p input) ascetic--default) (exit-minibuffer))
     ((eq ascetic--require-match t)
      (if (test-completion input ascetic--collection ascetic--predicate)
          (exit-minibuffer)
        (minibuffer-message "Strict match required")))
     (t (exit-minibuffer)))))

(defun ascetic--submit-first ()
  "Fast path execution: immediately commit the top candidate."
  (interactive)
  (if ascetic--current-candidates
      (let ((candidate (car ascetic--current-candidates)))
        (delete-region (+ (minibuffer-prompt-end) ascetic--current-base-size) (point-max))
        (insert candidate)
        (exit-minibuffer))
    (ascetic--submit-raw)))

;; Routing map

(defvar ascetic-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") #'ignore)
    (define-key map (kbd "C-p") #'ignore)
    (define-key map (kbd "<down>") #'ignore)
    (define-key map (kbd "<up>") #'ignore)
    (define-key map (kbd "TAB") #'ignore)

    (define-key map (kbd "M-p") #'previous-history-element)
    (define-key map (kbd "M-n") #'next-history-element)

    (define-key map (kbd "RET") #'ascetic--submit-raw)
    (define-key map (kbd "C-j") #'ascetic--submit-raw)
    (define-key map (kbd "M-RET") #'ascetic--submit-first)

    (dotimes (i (min ascetic-max-candidates 9))
      (let ((key (format "M-%d" (1+ i)))
            (idx i))
        (define-key map (kbd key)
		    (lambda () (interactive) (ascetic--insert-nth idx)))))
    map)
  "Keymap mapping structural intent over visual navigation.")

;; Environment setup

(defun ascetic--minibuffer-setup ()
  "Initialize stream context and attach visual display hook."
  (make-local-variable 'ascetic--overlay)
  (make-local-variable 'ascetic--current-candidates)
  (make-local-variable 'ascetic--current-base-size)
  (setq ascetic--overlay (make-overlay (point-min) (point-max) (current-buffer)))
  (add-hook 'post-command-hook #'ascetic--update-completions nil t))

(defun ascetic-completing-read (prompt collection &optional predicate require-match
                                       initial-input hist def inherit-input-method)
  "Main entry point: intercept standard minibuffer read."
  (let ((ascetic--collection collection)
        (ascetic--predicate predicate)
        (ascetic--require-match require-match)
        (ascetic--default def))

    (add-hook 'minibuffer-setup-hook #'ascetic--minibuffer-setup)
    (unwind-protect
        (let ((raw (read-from-minibuffer prompt initial-input ascetic-minibuffer-map nil hist def)))
          (cond
           ((not (string-empty-p raw)) raw)
           ((consp def) (car def))
           ((stringp def) def)
           (t raw)))
      (remove-hook 'minibuffer-setup-hook #'ascetic--minibuffer-setup)
      (when ascetic--overlay
        (delete-overlay ascetic--overlay)))))

;;;###autoload
(define-minor-mode ascetic-read-mode
  "Toggle Plan 9 inspired completion UI."
  :global t
  :group 'ascetic-read
  (if ascetic-read-mode
      (setq completing-read-function #'ascetic-completing-read)
    (setq completing-read-function #'completing-read-default)))

(provide 'ascetic-read)

;;; ascetic-read.el ends here
