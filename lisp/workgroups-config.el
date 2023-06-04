;; -*- lexical-binding: t; -*-

;; Workgroups2 Config
;;
;; <prefix> C-c    - create a new workgroup
;; <prefix> C-v    - open an existing workgroup
;; <prefix> C-k    - delete an existing workgroup
(require 'workgroups2)
;; Change prefix key (before activating WG)
(setq wg-prefix-key "C-c z")
(workgroups-mode 1)

(provide 'workgroups-config)
