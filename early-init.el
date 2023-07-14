(setq package-enable-at-startup nil)
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))

(setq inhibit-startup-screen t)
