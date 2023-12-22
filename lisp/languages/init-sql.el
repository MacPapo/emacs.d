;;; init-sql.el --- Sql Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package sqlformat
  :bind (:map sql-mode-map
              ("C-c C-f" . sqlformat))
  :hook (sql-mode . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g")))

(use-package sqlup-mode
  :bind (:map sql-mode-map
              ("C-c u" . sqlup-capitalize-keywords-in-region))
  :hook ((sql-mode . sqlup-mode)
         ;; Capitalize keywords in an interactive session (e.g. psql)
         (sql-interactive-mode . sqlup-mode))
  :config
  (add-to-list 'sqlup-blacklist "name"))

(provide 'init-sql)
;;; init-sql.el ends here
