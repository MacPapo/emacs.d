;;; ascetic-read.el --- Plan 9 inspired, pure text completion engine -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jacopo Costantini

;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;;
;; Ascetic-read replaces the standard Emacs completion UI with a minimal,
;; stream-based text interface inspired by Unix and Plan 9. It eliminates
;; vertical scrolling and visual menus in favor of strict string refinement and
;; composable pipeline actions.

;;; Code:

(defgroup ascetic-read nil
  "Plan 9 inspired, pure text completion engine."
  :group 'minibuffer)

(defcustom ascetic-max-candidates 5
  "Maximum number of candidates to compute and display. (Max 9)"
  :type 'integer
  :group 'ascetic-read)

(defvar ascetic--collection nil
  "Store collection for the current completion session.")

(defvar ascetic--predicate nil
  "Store predicate for the current completion session.")

(defvar ascetic--overlay nil
  "Overlay used to display candidates inline.")

(defvar ascetic--current-candidates nil
  "List of currently available completion candidates in the active session.")

(defvar ascetic--current-base-size 0
  "Length of the invariant base context (e.g. the directory path) to preserve.")

(defvar ascetic-input-filter-function #'identity
  "Function to filter the minibuffer content before passing it to the completion engine.
Takes a string and returns a string.")

(defun ascetic--take-n (lst n)
  "Extract up to N elements safely from a proper or improper list LST."
  (let ((i n)
        (res nil))
    (while (and (> i 0) (consp lst))
      (push (car lst) res)
      (setq lst (cdr lst))
      (setq i (1- i)))
    (nreverse res)))

(defun ascetic--select-nth (n)
  "Select the Nth candidate from the currently computed list and exit."
  (let ((candidate (nth n ascetic--current-candidates)))
    (if candidate
        (progn
          (delete-region (+ (minibuffer-prompt-end) ascetic--current-base-size) (point-max))
          (insert candidate)
          (exit-minibuffer))
      (minibuffer-message "No candidate %d" (1+ n)))))

(defun ascetic--exit-with-first ()
  "Exit with the first candidate, or current input if no candidates match."
  (interactive)
  (if (string-empty-p (minibuffer-contents))
      (exit-minibuffer)
    (if ascetic--current-candidates
        (ascetic--select-nth 0)
      (exit-minibuffer))))

(defun ascetic--submit-raw ()
  "Force submission of the exact typed input, ignoring completion candidates."
  (interactive)
  (exit-minibuffer))

(defvar ascetic-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") #'ignore)
    (define-key map (kbd "C-p") #'ignore)
    (define-key map (kbd "<down>") #'ignore)
    (define-key map (kbd "<up>") #'ignore)

    (define-key map (kbd "RET") #'ascetic--exit-with-first)
    (define-key map (kbd "C-j") #'ascetic--submit-raw)

    (dotimes (i (min ascetic-max-candidates 9))
      (let ((key (format "M-%d" (1+ i)))
            (idx i))
        (define-key map (kbd key)
          (lambda () (interactive) (ascetic--select-nth idx)))))
    map)
  "Keymap enforcing direct signal selection, disabling navigation.")

(defun ascetic--minibuffer-setup ()
  "Attach the stream listener to the current minibuffer."
  (make-local-variable 'ascetic--overlay)
  (make-local-variable 'ascetic--current-candidates)
  (make-local-variable 'ascetic--current-base-size)
  (setq ascetic--overlay (make-overlay (point-min) (point-max) (current-buffer)))
  (add-hook 'post-command-hook #'ascetic--update-completions 0 t))

(defun ascetic--update-completions ()
  "Compute completions for current input and echo them."
  (when ascetic--overlay
    (move-overlay ascetic--overlay (point-min) (point-max) (current-buffer))
    (let* ((raw-content (minibuffer-contents))
           (content (funcall ascetic-input-filter-function raw-content))
           (completions (completion-all-completions
                         content ascetic--collection ascetic--predicate (length content)))
           (last-cell (last completions))
           (base-size (if (and last-cell (numberp (cdr last-cell)))
                          (prog1 (cdr last-cell)
                            (setcdr last-cell nil))
                        0))
           (sorted-completions (if (or (string-empty-p content) (not completions))
                                   completions
                                 (sort completions (lambda (s1 s2) (< (length s1) (length s2))))))
           (lst (ascetic--take-n sorted-completions ascetic-max-candidates)))

      (setq ascetic--current-candidates lst)
      (setq ascetic--current-base-size base-size)

      (if lst
          (let* ((i 0)
                 (text (concat " \n"
                               (mapconcat (lambda (item)
                                            (setq i (1+ i))
                                            (format "%d. %s" i item))
                                          lst "\n"))))
            (put-text-property 0 1 'cursor t text)
            (overlay-put ascetic--overlay 'after-string text))
        (overlay-put ascetic--overlay 'after-string "")))))

(defun ascetic-completing-read (prompt collection &optional predicate require-match
                                       initial-input hist def inherit-input-method)
  "Intercept minibuffer I/O, bypassing default UI for stream-based completion."
  (let ((ascetic--collection collection)
        (ascetic--predicate predicate))

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
