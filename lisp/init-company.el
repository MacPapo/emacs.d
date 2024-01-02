;;; init-company.el --- Company Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package company
  :demand t
  :config
  (setq company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-lighter-base "©"
        company-tooltip-limit 10
        company-idle-delay 0.1
        company-minimum-prefix-length 1
        ;; company-require-match 'never
        ;; company-format-margin-function 'company-text-icons-margin
        company-tooltip-minimum 4
        ;; company-text-face-extra-attributes '(:weight bold :slant italic)
        company-text-icons-add-background nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-tooltip-flip-when-above t
        ;; company-show-quick-access 'left
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-files-exclusions '(".git/" ".DS_Store")
        ;; company-transformers '(delete-consecutive-dups
                               ;; company-sort-by-occurrence)
        company-global-modes '(not erc-mode message-mode help-mode))
  (global-company-mode +1))

(use-package company-prescient
  :demand t
  :config
  (company-prescient-mode +1))

(provide 'init-company)
;;; init-company.el ends here
