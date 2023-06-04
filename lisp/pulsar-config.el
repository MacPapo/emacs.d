;; -*- lexical-binding: t; -*-
;; Pulsar Config
;;
(require 'pulsar)
(setq pulsar-pulse t
      pulsar-face 'pulsar-magenta
      pulsar-highlight-face 'pulsar-yellow
      pulsar-delay 0.055)
(pulsar-global-mode 1)

(provide 'pulsar-config)
