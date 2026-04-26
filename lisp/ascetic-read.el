;;; ascetic-read.el --- Plan 9 inspired text completion UI -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jacopo Costantini
;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Minimal, stream-based completion UI.
;; UI rendering is decoupled from input composition.

;;; Code:

(require 'seq)

(defgroup ascetic-read nil
  "Plan 9 inspired completion engine."
  :group 'minibuffer)

(defcustom ascetic-max-candidates 5
  "Maximum number of candidates to display."
  :type 'integer
  :group 'ascetic-read)

(defvar ascetic-input-filter-function #'identity
  "Filter function applied to minibuffer input before evaluation.")

(defvar ascetic--collection nil "Active collection.")
(defvar ascetic--predicate nil "Active predicate.")
(defvar ascetic--require-match nil "Strict match flag.")
(defvar ascetic--default nil "Default value.")
(defvar ascetic--overlay nil "Rendering overlay.")
(defvar ascetic--current-candidates nil "Displayed candidates.")
(defvar ascetic--current-base-size 0 "Candidate base size.")
(defvar ascetic--history-hash nil "O(1) history index.")

(defun ascetic--remote-p (path)
  "Return t if PATH is a remote TRAMP path."
  (string-match-p "\\`/[^/|:]+:" (substitute-in-file-name path)))

(defun ascetic--smart-sort (completions)
  "Sort COMPLETIONS by history index, then length."
  (if (not ascetic--history-hash)
      (sort completions :key #'length :in-place t)
    (sort completions
          :in-place t
          :lessp (lambda (c1 c2)
                   (let ((idx1 (gethash c1 ascetic--history-hash))
                         (idx2 (gethash c2 ascetic--history-hash)))
                     (cond
                      ((and idx1 idx2) (< idx1 idx2))
                      (idx1 t)
                      (idx2 nil)
                      (t (< (length c1) (length c2)))))))))

(defun ascetic--update-completions ()
  "Compute completions synchronously and render overlay."
  (when ascetic--overlay
    (move-overlay ascetic--overlay (point-min) (point-max) (current-buffer))
    (let* ((raw-content (minibuffer-contents-no-properties))
           (content (funcall ascetic-input-filter-function raw-content)))
      (redisplay)
      (let* ((metadata (completion-metadata content ascetic--collection ascetic--predicate))
             (category (completion-metadata-get metadata 'category))
             (is-remote (and (eq category 'file)
                             (or (ascetic--remote-p content)
                                 (ascetic--remote-p default-directory))))
             (compute-engine
              (lambda ()
                (let* ((non-essential t)
                       (gc-cons-threshold most-positive-fixnum)
                       (completions (completion-all-completions
                                     content ascetic--collection ascetic--predicate (length content)))
                       (last-cell (last completions))
                       (base-size (if (and last-cell (numberp (cdr last-cell)))
                                      (prog1 (cdr last-cell) (setcdr last-cell nil))
                                    0))
                       (filtered (if (and completions (eq category 'file))
                                     (completion-pcm--filename-try-filter completions)
                                   completions))
                       (sort-fn (or (completion-metadata-get metadata 'display-sort-function)
                                    #'ascetic--smart-sort))
                       (lst (when filtered
                              (seq-take (funcall sort-fn filtered) ascetic-max-candidates))))
                  (cons lst base-size))))
             (state (if is-remote
                        (funcall compute-engine)
                      (while-no-input (funcall compute-engine)))))
        (when (consp state)
          (let ((lst (car state))
                (base-size (cdr state)))
            (setq ascetic--current-candidates lst)
            (setq ascetic--current-base-size base-size)
            (if lst
                (let ((text (concat " \n  " (mapconcat #'identity lst "\n  "))))
                  (put-text-property 0 1 'cursor t text)
                  (overlay-put ascetic--overlay 'after-string text))
              (overlay-put ascetic--overlay 'after-string ""))))))))

(defun ascetic-read-refresh ()
  "Force a synchronous refresh of the completion overlay."
  (interactive)
  (when (and ascetic--overlay (minibufferp))
    (ascetic--update-completions)))

(defun ascetic--insert-nth (n)
  "Materialize candidate N into the prompt."
  (interactive)
  (let ((candidate (nth n ascetic--current-candidates)))
    (if candidate
        (progn
          (delete-region (+ (minibuffer-prompt-end) ascetic--current-base-size) (point-max))
          (insert candidate))
      (minibuffer-message "No candidate %d" (1+ n)))))

(defun ascetic--insert-by-chord ()
  "Extract digit from key and materialize candidate."
  (interactive)
  (let ((idx (- (event-basic-type last-command-event) ?1)))
    (ascetic--insert-nth idx)))

(defun ascetic--submit-raw ()
  "Commit prompt state. Bypass confirmation."
  (interactive)
  (let ((input (minibuffer-contents-no-properties)))
    (cond
     ((and (string-empty-p input) ascetic--default) (exit-minibuffer))
     ((eq ascetic--require-match t)
      (if (test-completion input ascetic--collection ascetic--predicate)
          (exit-minibuffer)
        (minibuffer-message "Strict match required")))
     (t (exit-minibuffer)))))

(defun ascetic--submit-first ()
  "Commit the top candidate immediately."
  (interactive)
  (if ascetic--current-candidates
      (let ((candidate (car ascetic--current-candidates)))
        (delete-region (+ (minibuffer-prompt-end) ascetic--current-base-size) (point-max))
        (insert candidate)
        (exit-minibuffer))
    (ascetic--submit-raw)))

(defun ascetic--expand-lcp ()
  "Expand input to longest common prefix."
  (interactive)
  (let* ((input (minibuffer-contents-no-properties))
         (start-pos (minibuffer-prompt-end))
         (try (completion-try-completion
               input ascetic--collection ascetic--predicate (- (point) start-pos))))
    (if (consp try)
        (let ((new-text (car try))
              (new-pos (cdr try)))
          (unless (string= input new-text)
            (delete-region start-pos (point-max))
            (insert new-text)
            (goto-char (+ start-pos new-pos))))
      (minibuffer-message "No expansion possible"))))

(defvar ascetic-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") #'ignore)
    (define-key map (kbd "C-p") #'ignore)
    (define-key map (kbd "TAB") #'ascetic--expand-lcp)
    (define-key map (kbd "RET") #'ascetic--submit-raw)
    (define-key map (kbd "M-RET") #'ascetic--submit-first)
    (dotimes (i (min ascetic-max-candidates 9))
      (define-key map (kbd (format "M-%d" (1+ i))) #'ascetic--insert-by-chord))
    map)
  "Keymap mapping structural intent over visual navigation.")

(defun ascetic--minibuffer-setup ()
  "Initialize session context."
  (make-local-variable 'ascetic--overlay)
  (make-local-variable 'ascetic--current-candidates)
  (make-local-variable 'ascetic--current-base-size)
  (make-local-variable 'ascetic--history-hash)
  (setq ascetic--overlay (make-overlay (point-min) (point-max) (current-buffer)))
  (let* ((hist-var minibuffer-history-variable)
         (hist-list (when (and hist-var (boundp hist-var)) (symbol-value hist-var))))
    (when hist-list
      (setq ascetic--history-hash (make-hash-table :test 'equal :size (length hist-list)))
      (let ((idx 0))
        (dolist (item hist-list)
          (unless (gethash item ascetic--history-hash)
            (puthash item idx ascetic--history-hash)
            (setq idx (1+ idx)))))))
  (add-hook 'post-command-hook #'ascetic--update-completions nil t))

(defun ascetic-completing-read (prompt collection &optional predicate require-match
                                       initial-input hist def _inherit-input-method)
  "Entry point for ascetic completion. Arguments follow `completing-read'."
  (let ((ascetic--collection collection)
        (ascetic--predicate predicate)
        (ascetic--require-match require-match)
        (ascetic--default def)
        (minibuffer-completion-table collection)
        (minibuffer-completion-predicate predicate)
        (minibuffer-completion-confirm require-match))
    (add-hook 'minibuffer-setup-hook #'ascetic--minibuffer-setup)
    (unwind-protect
        (let ((raw (read-from-minibuffer prompt initial-input ascetic-minibuffer-map nil hist def)))
          (cond ((not (string-empty-p raw)) raw)
                ((consp def) (car def))
                ((stringp def) def)
                (t raw)))
      (remove-hook 'minibuffer-setup-hook #'ascetic--minibuffer-setup)
      (when ascetic--overlay (delete-overlay ascetic--overlay)))))

;;;###autoload
(define-minor-mode ascetic-read-mode
  "Toggle Plan 9 inspired completion UI."
  :global t
  (if ascetic-read-mode
      (setq completing-read-function #'ascetic-completing-read)
    (setq completing-read-function #'completing-read-default)))

(provide 'ascetic-read)
;;; ascetic-read.el ends here
