;;; ascetic-plumber.el --- Plan 9 inspired completion routing -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jacopo Costantini
;; Author: Jacopo Costantini <jacopocostantini32@gmail.com>
;; License: GNU General Public License version 3 (or later)

;;; Commentary:
;; Pure router for Emacs completions.
;; Routes candidate streams to stdin of processes or lisp sinks.

;;; Code:

(require 'compile)

(defgroup ascetic-plumber nil
  "Plan 9 inspired completion routing."
  :group 'minibuffer)

(defcustom ascetic-plumber-actions
  '((file   . ((">" . ascetic-plumber-action-dired)))
    (buffer . ((">" . ascetic-plumber-action-ibuffer)
               ("!" . ascetic-plumber-action-kill-buffers)))
    (t      . (("<" . ascetic-plumber-action-insert)
               (">" . ascetic-plumber-action-print)
               ("!" . ascetic-plumber-action-shell-async)
               ("|" . ascetic-plumber-action-shell-sync))))
  "Routing table mapping categories and operators to sinks."
  :type '(alist :key-type symbol :value-type alist)
  :group 'ascetic-plumber)

(defcustom ascetic-plumber-operator-export " > " "Export to Lisp UI."    :type 'string)
(defcustom ascetic-plumber-operator-insert " < " "Insert raw text."     :type 'string)
(defcustom ascetic-plumber-operator-bang   " ! " "Async shell pipe."    :type 'string)
(defcustom ascetic-plumber-operator-pipe   " | " "Sync shell pipe."     :type 'string)

(defun ascetic-plumber-action-insert (_cmd candidates _category)
  "Insert CANDIDATES as raw text."
  (insert (mapconcat #'identity candidates " ")))

(defun ascetic-plumber-action-print (_cmd candidates _category)
  "Dump CANDIDATES into a stream buffer."
  (let ((buf (get-buffer-create "*Plumber Stream*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (mapconcat #'identity candidates "\n")))
    (display-buffer buf)))

(defun ascetic-plumber-action-dired (_cmd candidates _category)
  "Materialize file CANDIDATES into Dired."
  (dired (cons default-directory candidates)))

(defun ascetic-plumber-action-ibuffer (_cmd candidates _category)
  "Materialize buffer CANDIDATES into Ibuffer."
  (ibuffer nil "*Plumber Buffers*" (list (cons 'name (regexp-opt candidates)))))

(defun ascetic-plumber-action-kill-buffers (cmd candidates _category)
  "Kill CANDIDATES buffers if CMD is 'kill'."
  (if (string= (string-trim cmd) "kill")
      (progn (mapc #'kill-buffer candidates)
             (message "Ascetic Plumber: Killed %d buffers." (length candidates)))
    (user-error "Ascetic Plumber: Unknown buffer command '%s'" cmd)))

(defun ascetic-plumber-action-shell-async (cmd candidates _category)
  "Stream CANDIDATES to CMD via STDIN asynchronously."
  (when (not (string-empty-p cmd))
    (let* ((clean-cmd (string-trim cmd))
           (buf-name (format "*Plumber: %s*" clean-cmd))
           (input-data (concat (mapconcat #'identity candidates "\n") "\n")))
      (let* ((comp-buf (compilation-start clean-cmd t (lambda (_) buf-name)))
             (proc (get-buffer-process comp-buf)))
        (when proc
          (process-send-string proc input-data)
          (process-send-eof proc))))))

(defun ascetic-plumber-action-shell-sync (cmd candidates _category)
  "Stream CANDIDATES to CMD via STDIN synchronously."
  (when (not (string-empty-p cmd))
    (let ((clean-cmd (string-trim cmd))
          (input-data (concat (mapconcat #'identity candidates "\n") "\n"))
          (target-buf (current-buffer)))
      (with-temp-buffer
        (insert input-data)
        (shell-command-on-region (point-min) (point-max) clean-cmd (current-buffer) t)
        (let ((output (buffer-string)))
          (with-current-buffer target-buf
            (insert output)))))))

(defun ascetic-plumber--parse-input (input)
  "Parse INPUT based on declared operators."
  (let* ((ops (list ascetic-plumber-operator-export
                    ascetic-plumber-operator-insert
                    ascetic-plumber-operator-bang
                    ascetic-plumber-operator-pipe))
         (lexer-rx (concat "\\(" (mapconcat #'regexp-quote ops "\\|") "\\)")))
    (if (string-match lexer-rx input)
        (list (match-string 1 input)
              (substring input 0 (match-beginning 0))
              (substring input (match-end 0)))
      nil)))

(defun ascetic-plumber--harvest (query)
  "Extract truth from native completion metadata for QUERY."
  (let* ((metadata (completion-metadata query minibuffer-completion-table minibuffer-completion-predicate))
         (category (completion-metadata-get metadata 'category))
         (raw      (completion-all-completions query minibuffer-completion-table minibuffer-completion-predicate (length query)))
         (tail     (last raw))
         (base-size (if (and tail (numberp (cdr tail))) (cdr tail) 0)))
    (when (and tail (numberp (cdr tail))) (setcdr tail nil))
    (list category
          (substring query 0 (min base-size (length query)))
          (mapcar #'substring-no-properties raw))))

(defun ascetic-plumber-commit ()
  "Hijack RET, parse intent, and dispatch to sinks."
  (interactive)
  (let* ((content (minibuffer-contents-no-properties))
         (plumb   (ascetic-plumber--parse-input content)))
    (if (not plumb)
        (if (fboundp 'ascetic--submit-raw)
            (ascetic--submit-raw)
          (exit-minibuffer))
      (let* ((operator   (string-trim (car plumb)))
             (query      (string-trim (cadr plumb)))
             (cmd        (string-trim (caddr plumb)))
             (harvested  (ascetic-plumber--harvest query))
             (category   (nth 0 harvested))
             (base-str   (nth 1 harvested))
             (candidates (nth 2 harvested))
             (cat-rules  (or (alist-get category ascetic-plumber-actions)
                             (alist-get t ascetic-plumber-actions)))
             (sink-fn    (or (alist-get operator cat-rules nil nil #'string=)
                             (alist-get operator (alist-get t ascetic-plumber-actions) nil nil #'string=)))
             (ctx-dir    (expand-file-name base-str default-directory))
             (target-buf (window-buffer (minibuffer-selected-window))))
        (unless sink-fn
          (user-error "No action mapped for category '%s' and operator '%s'" category operator))
        (run-at-time 0 nil (lambda ()
                             (let ((default-directory ctx-dir))
                               (with-current-buffer target-buf
                                 (funcall sink-fn cmd candidates category)))))
        (abort-recursive-edit)))))

;; --- Integration ---

(defun ascetic-plumber-setup-bindings ()
  "Bind plumber routing to minibuffer RET."
  (let ((map (if (boundp 'ascetic-minibuffer-map)
                 ascetic-minibuffer-map
               minibuffer-local-map)))
    (define-key map (kbd "RET") #'ascetic-plumber-commit)))

(with-eval-after-load 'ascetic-read
  (ascetic-plumber-setup-bindings))

(add-hook 'minibuffer-setup-hook #'ascetic-plumber-setup-bindings)

(provide 'ascetic-plumber)
;;; ascetic-plumber.el ends here
