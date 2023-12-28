;;; init-gnus.el --- Better Emacs Gnus ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gnus
  :ensure nil
  :bind (:map gnus-group-mode-map
              ("o" . my-gnus-group-list-subscribed-groups))
  :hook ((message-mode . (lambda ()
                           (flyspell-mode t))))
  :config
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-date)
          (not gnus-article-sort-by-number))
        ;; Patch article
        gnus-article-patch-conditions
        '( "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" )
        ;; Specify the send mail function
        send-mail-function         'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)

  (defun my-gnus-group-list-subscribed-groups ()
    "List all subscribed groups with or without un-read messages"
    (interactive)
    (gnus-group-list-all-groups 5)))

(provide 'init-gnus)
;;; init-gnus.el ends here
