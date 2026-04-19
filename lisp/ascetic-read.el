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
;; 5. Protocol Obedience: Respects backend metadata (display-sort-function).

;;; Code:

(require 'seq)

(defgroup ascetic-read nil
  "Plan 9 inspired, pure text completion engine."
  :group 'minibuffer)

(defcustom ascetic-max-candidates 5
  "Maximum number of candidates to compute and display (Max 9)."
  :type 'integer
  :group 'ascetic-read)

(defvar ascetic-input-filter-function #'identity
  "Function filtering the minibuffer stream before engine evaluation.")

;; Session registers
(defvar ascetic--collection nil "Session register for the current collection.")
(defvar ascetic--predicate nil "Session register for the current predicate.")
(defvar ascetic--require-match nil "Session register for strict match requirements.")
(defvar ascetic--default nil "Session register for the default value.")
(defvar ascetic--overlay nil "Session register for the rendering overlay.")
(defvar ascetic--current-candidates nil "Session register for displayed candidates.")
(defvar ascetic--current-base-size 0 "Session register for candidate base size.")
(defvar ascetic--history-hash nil "Session register for pre-computed history O(1) index.")

(defun ascetic--smart-sort (completions)
  "Sort COMPLETIONS using a hybrid engine.
Priority 1: History (O(1) lookup via session-cached hash table).
Priority 2: String length (In-place C fallback)."
  (if (not ascetic--history-hash)
      ;; Fast path: No history available, fallback to length.
      (sort completions :key #'length :in-place t)
    ;; History path: Use pre-computed session index.
    (sort completions
          :in-place t
          :lessp (lambda (c1 c2)
                   (let ((idx1 (gethash c1 ascetic--history-hash))
                         (idx2 (gethash c2 ascetic--history-hash)))
                     (cond
                      ((and idx1 idx2) (< idx1 idx2)) ;; Both in history, lowest index wins
                      (idx1 t)                        ;; Only c1 in history
                      (idx2 nil)                      ;; Only c2 in history
                      (t (< (length c1) (length c2))))))))) ;; Neither in history, shortest wins

(defun ascetic--update-completions ()
  "Compute completions synchronously and update the overlay.
Aborts immediately on pending input. Binds `non-essential' to prevent
blocking I/O (e.g., TRAMP). Suspends Garbage Collection during
computation to guarantee frame rate."
  (when ascetic--overlay
    (let* ((raw-content (minibuffer-contents-no-properties))
           (content (funcall ascetic-input-filter-function raw-content))
           (state
            (while-no-input
              (let* ((non-essential t)
                     (gc-cons-threshold most-positive-fixnum)
                     (completions (completion-all-completions
                                   content ascetic--collection ascetic--predicate (length content)))
                     (metadata (completion-metadata content ascetic--collection ascetic--predicate))
                     ;; PROTOCOL: Defer to backend metadata if present, else use our smart sort.
                     (sort-fn (or (completion-metadata-get metadata 'display-sort-function)
                                  #'ascetic--smart-sort))
                     (last-cell (last completions))
                     (base-size (if (and last-cell (numberp (cdr last-cell)))
                                    (prog1 (cdr last-cell) (setcdr last-cell nil))
                                  0))
                     ;; FIX: Never bypass sorting on empty strings to prioritize history.
                     (lst (if completions
                              (seq-take (funcall sort-fn completions) ascetic-max-candidates)
                            nil)))
                (cons lst base-size)))))

      ;; If state is a cons cell, computation finished uninterrupted.
      (when (consp state)
        (let ((lst (car state))
              (base-size (cdr state)))
          (setq ascetic--current-candidates lst)
          (setq ascetic--current-base-size base-size)
          (move-overlay ascetic--overlay (point-min) (point-max) (current-buffer))

          (if lst
              (let ((text (concat " \n  " (mapconcat #'identity lst "\n  "))))
                ;; Inform C display engine to lock cursor before the overlay.
                (put-text-property 0 1 'cursor t text)
                (overlay-put ascetic--overlay 'after-string text))
            (overlay-put ascetic--overlay 'after-string "")))))))

(defun ascetic--insert-nth (n)
  "Materialize candidate N into the prompt without executing."
  (interactive)
  (let ((candidate (nth n ascetic--current-candidates)))
    (if candidate
        (progn
          (delete-region (+ (minibuffer-prompt-end) ascetic--current-base-size) (point-max))
          (insert candidate))
      (minibuffer-message "No candidate %d" (1+ n)))))

(defun ascetic--insert-by-chord ()
  "Extract digit from the pressed key and materialize the candidate."
  (interactive)
  (let ((idx (- (event-basic-type last-command-event) ?1)))
    (ascetic--insert-nth idx)))

(defun ascetic--submit-raw ()
  "Commit exact prompt state.
Validates only if `ascetic--require-match' is strictly t.
Confirmation prompts (e.g. `confirm') are bypassed by design."
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
  "Brutally replace current input with the LCP provided by the system's completion engine."
  (interactive)
  (let* ((input (minibuffer-contents-no-properties))
         (start-pos (minibuffer-prompt-end))
         ;; Chiediamo al sistema: "Qual è la tua miglior espansione per questo input?"
         (try (completion-try-completion
               input
               ascetic--collection
               ascetic--predicate
               (- (point) start-pos))))
    (if (consp try)
        (let ((new-text (car try))
              (new-pos (cdr try)))
          ;; Se il sistema suggerisce qualcosa di più lungo, sostituiamo
          (unless (string= input new-text)
            (delete-region start-pos (point-max))
            (insert new-text)
            ;; Posizioniamo il cursore dove il sistema suggerisce
            (goto-char (+ start-pos new-pos))))
      (minibuffer-message "No further expansion possible"))))

(defvar ascetic-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-n") #'ignore)
    (define-key map (kbd "C-p") #'ignore)
    (define-key map (kbd "<down>") #'ignore)
    (define-key map (kbd "<up>") #'ignore)

    ;; INJECTED THE BRUTAL LCP EXPANSION
    (define-key map (kbd "TAB") #'ascetic--expand-lcp)

    (define-key map (kbd "M-p") #'previous-history-element)
    (define-key map (kbd "M-n") #'next-history-element)

    (define-key map (kbd "RET") #'ascetic--submit-raw)
    (define-key map (kbd "C-j") #'ascetic--submit-raw)
    (define-key map (kbd "M-RET") #'ascetic--submit-first)

    (dotimes (i (min ascetic-max-candidates 9))
      (define-key map (kbd (format "M-%d" (1+ i))) #'ascetic--insert-by-chord))
    map)
  "Keymap mapping structural intent over visual navigation.")

(defun ascetic--minibuffer-setup ()
  "Initialize stream context and attach visual display hook."
  (make-local-variable 'ascetic--overlay)
  (make-local-variable 'ascetic--current-candidates)
  (make-local-variable 'ascetic--current-base-size)
  (make-local-variable 'ascetic--history-hash)

  (setq ascetic--overlay (make-overlay (point-min) (point-max) (current-buffer)))

  ;; Pre-compute O(1) history index purely once per minibuffer session.
  (let* ((hist-var minibuffer-history-variable)
         (hist-list (when (and hist-var (boundp hist-var))
                      (symbol-value hist-var))))
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
  "Intercept standard minibuffer read given PROMPT and COLLECTION.
PREDICATE limits the candidates. REQUIRE-MATCH enforces strict matches;
values like `confirm' are intentionally treated as nil to reduce noise.
INITIAL-INPUT, HIST, DEF, and INHERIT-INPUT-METHOD follow standard semantics."
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
