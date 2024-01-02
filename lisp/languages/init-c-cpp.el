;;; init-c-cpp.el --- C-Cpp Configuration ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; C++
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(provide 'init-c-cpp)
;;; init-c-cpp.el ends here
