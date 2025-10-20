;;; c-cpp-lang.el --- C and C++ configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; 


;;; Code:

(use-package cc-mode
  :custom
  (c-default-style '((java-mode . "java") (awk-mode . "awk") (c-mode . "linux") (other . "gnu"))))

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(use-package cwarn
  :hook ((c-mode c++-mode) . cwarn-mode))

(use-package disaster
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package eglot
  :ensure t
  :pin gnu
  :hook ((c-mode c++-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd"
                                      "-j=8"
                                      "--clang-tidy"
                                      "--background-index"
                                      "--pch-storage=memory"
                                      "--all-scopes-completion"
                                      "--header-insertion=iwyu"
                                      "--fallback-style=llvm"))))

(provide 'c-cpp-lang)

;;; c-cpp-lang.el ends here
