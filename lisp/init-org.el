;;; init-org.el --- Org Configuration ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org)
(setq org-completion-use-ido t
      org-outline-path-complete-in-steps nil)

(use-package org-pomodoro)

(provide 'init-org)
;;; init-org.el ends here
