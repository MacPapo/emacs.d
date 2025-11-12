;;; cue-gen.el --- Minimal CUE generator for 01-Title.wav files -*- lexical-binding: t -*-

;; Author: Your Name
;; Version: 3.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: multimedia, cd, cue

;;; Commentary:
;; Generate gapless CUE from numbered WAVs.
;; Uses soxi if available, else asks duration.
;; Sorts correctly, cleans titles, handles spaces.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar cue-gen-use-soxi t
  "Use soxi for duration if t and available.")

(defun cue-gen--track-number (file)
  "Return track number from FILE like '01-Title.wav' -> 1."
  (let ((name (file-name-nondirectory file)))
    (when (string-match "^\\([0-9]+\\)" name)
      (string-to-number (match-string 1 name)))))

(defun cue-gen--track-title (file)
  "Return title without '01-' prefix."
  (let ((name (file-name-nondirectory file)))
    (if (string-match "^[0-9]+- *\\(.*\\)\\.wav$" name)
        (match-string 1 name)
      (file-name-base name))))

(defun cue-gen--sort-tracks (files)
  "Sort FILES by track number."
  (seq-sort-by #'cue-gen--track-number #'< files))

(defun cue-gen--duration (file)
  "Get duration in seconds."
  (if (and cue-gen-use-soxi
           (executable-find "soxi"))
      (let ((out (shell-command-to-string
                  (format "soxi -D %s" (shell-quote-argument file)))))
        (when (string-match "[0-9]+\\(\\.[0-9]*\\)?" (string-trim out))
          (string-to-number (match-string 0 out))))
    (read-number (format "Duration (sec) for %s: " (file-name-nondirectory file)))))

(defun cue-gen--mmss (seconds)
  "Convert SECONDS to MM:SS:00."
  (let* ((s (floor seconds))
         (m (/ s 60))
         (ss (% s 60)))
    (format "%02d:%02d:00" m ss)))

(defun generate-cue-from-wavs (dir)
  "Generate CUE from WAVs in DIR."
  (interactive "DDirectory: ")
  (let* ((wavs (directory-files dir t "\\.wav$" t))
         (tracks (seq-filter #'cue-gen--track-number wavs))
         (sorted (cue-gen--sort-tracks tracks))
         (album-wav (or (seq-find
                         (lambda (f)
                           (string-match-p (regexp-quote "album") (downcase f)))
                         wavs)
                        (car sorted)))
         (cue-file (expand-file-name "album.cue" dir))
         (artist (read-string "Artist: " "You"))
         (album (read-string "Album: " "For Her"))
         (cumul 0.0)
         (track 1))

    (unless sorted (user-error "No numbered WAVs in %s" dir))
    (unless album-wav (user-error "No album WAV found"))

    (with-temp-file cue-file
      (insert (format "PERFORMER \"%s\"\nTITLE \"%s\"\n" artist album))
      (insert (format "FILE \"%s\" WAVE\n" (file-name-nondirectory album-wav)))

      (dolist (f sorted)
        (let* ((dur (cue-gen--duration f))
               (time (cue-gen--mmss cumul))
               (title (cue-gen--track-title f)))
          (insert (format "  TRACK %02d AUDIO\n    TITLE \"%s\"\n    INDEX 01 %s\n"
                          track title time))
          (setq cumul (+ cumul dur))
          (setq track (1+ track)))))

    (message "CUE generated: %s" cue-file)))

(provide 'cue-gen)

;;; cue-gen.el ends here
