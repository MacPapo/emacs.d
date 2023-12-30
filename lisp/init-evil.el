;;; init-evil.el --- Using VIM bindings ;; -*- lexical-bindings: t; -*-
;;; Commentary:
;;; Code:

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default
        evil-undo-system 'undo-redo
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-regexp-search t
        evil-search-wrap t
        evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-tutor)

(use-package general
  :demand t
  :config
  (general-evil-setup t)

  (general-create-definer mp/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC") ;; to use in insert mode

  (mp/leader-keys
   "SPC"     '(smex :wk "SMEX")
   ","       '(switch-to-buffer :wk "Switch buffer")
   "."       '(find-file :wk "Find File")
   "TAB TAB" '(comment-line :wk "Comment lines")
   "u"       '(universal-argument :wk "Universal argument"))

  (mp/leader-keys
   "b"       '(:ignore t :wk "Buffers/Bookmarks")
   "b b"     '(switch-to-buffer :wk "Switch to buffer")
   "b c"     '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
   "b C"     '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
   "b d"     '(bookmark-delete :wk "Delete bookmark")
   "b i"     '(ibuffer :wk "Ibuffer")
   "b k"     '(kill-current-buffer :wk "Kill current buffer")
   "b K"     '(kill-some-buffers :wk "Kill multiple buffers")
   "b l"     '(list-bookmarks :wk "List bookmarks")
   "b m"     '(bookmark-set :wk "Set bookmark")
   "b n"     '(next-buffer :wk "Next buffer")
   "b p"     '(previous-buffer :wk "Previous buffer")
   "b r"     '(revert-buffer :wk "Reload buffer")
   "b R"     '(rename-buffer :wk "Rename buffer")
   "b s"     '(basic-save-buffer :wk "Save buffer")
   "b S"     '(save-some-buffers :wk "Save multiple buffers")
   "b w"     '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  (mp/leader-keys
   "d"       '(:ignore t :wk "Dired")
   "d d"     '(dired :wk "Open dired")
   "d j"     '(dired-jump :wk "Dired jump to current"))

  (mp/leader-keys
   "e"       '(:ignore t :wk "Eshell/Evaluate")
   "e b"     '(eval-buffer :wk "Evaluate elisp in buffer")
   "e d"     '(eval-defun :wk "Evaluate defun containing or after point")
   "e e"     '(eval-expression :wk "Evaluate and elisp expression")
   "e l"     '(eval-last-sexp :wk "Evaluate elisp expression before point")
   "e r"     '(eval-region :wk "Evaluate elisp in region")
   "e R"     '(eww-reload :which-key "Reload current page in EWW")
   "e s"     '(eshell :which-key "Eshell")
   "e w"     '(eww :which-key "EWW emacs web wowser"))

  (mp/leader-keys
   "g"       '(:ignore t :wk "Git")
   "g /"     '(magit-dispatch :wk "Magit dispatch")
   "g ."     '(magit-file-dispatch :wk "Magit file dispatch")
   "g b"     '(magit-branch-checkout :wk "Switch branch")

   "g c"     '(:ignore t :wk "Create")
   "g c b"   '(magit-branch-and-checkout :wk "Create branch and checkout")
   "g c c"   '(magit-commit-create :wk "Create commit")
   "g c f"   '(magit-commit-fixup :wk "Create fixup commit")
   "g C"     '(magit-clone :wk "Clone repo")

   "g f"     '(:ignore t :wk "Find")
   "g f c"   '(magit-show-commit :wk "Show commit")
   "g f f"   '(magit-find-file :wk "Magit find file")
   "g f g"   '(magit-find-git-config-file :wk "Find gitconfig file")
   "g F"     '(magit-fetch :wk "Git fetch")
   "g g"     '(magit-status :wk "Magit status")
   "g i"     '(magit-init :wk "Initialize git repo")
   "g l"     '(magit-log-buffer-file :wk "Magit buffer log")
   "g r"     '(vc-revert :wk "Git revert file")
   "g s"     '(magit-stage-file :wk "Git stage file")
   "g t"     '(git-timemachine :wk "Git time machine")
   "g u"     '(magit-stage-file :wk "Git unstage file"))

  (mp/leader-keys
    "w"      '(:ignore t :wk "Windows")

    ;; Window splits
    "w c"    '(evil-window-delete :wk "Close window")
    "w n"    '(evil-window-new :wk "New window")
    "w s"    '(evil-window-split :wk "Horizontal split window")
    "w v"    '(evil-window-vsplit :wk "Vertical split window")

    ;; Window motions
    "w h"   '(evil-window-left :wk "Window left")
    "w j"   '(evil-window-down :wk "Window down")
    "w k"   '(evil-window-up :wk "Window up")
    "w l"   '(evil-window-right :wk "Window right")
    "w w"   '(evil-window-next :wk "Goto next window")

    ;; Move Windows
    "w H"   '(buf-move-left :wk "Buffer move left")
    "w J"   '(buf-move-down :wk "Buffer move down")
    "w K"   '(buf-move-up :wk "Buffer move up")
    "w L"   '(buf-move-right :wk "Buffer move right"))

  ;; bind in motion state (inherited by the normal, visual, and operator states)
  ;; (mmap
  ;;  ";" 'evil-ex
  ;;  ":" 'evil-repeat-find-char)

  ;;,* Mode Keybindings
  (nmap
    :keymaps 'emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
  )

(provide 'init-evil)
;;; init-evil.el ends here
