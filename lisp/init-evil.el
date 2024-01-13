;;; init-evil.el --- Using VIM bindings ;; -*- lexical-bindings: t; -*-
  ;;; Commentary:
  ;;; Code:

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-redo
        evil-search-module 'evil-search
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-regexp-search t
        evil-search-wrap t
        evil-want-C-u-scroll t)
  :config
  ;;(with-eval-after-load 'evil-maps
  ;;   (define-key evil-motion-state-map (kbd "SPC") nil)
  ;;   (define-key evil-motion-state-map (kbd "RET") nil)
  ;;   (define-key evil-motion-state-map (kbd "TAB") nil))

  (evil-mode +1))

(use-package evil-collection
  :demand t
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-want-find-usages-bindings t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :diminish
  :config
  (evil-commentary-mode +1))

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
   ":"       '(execute-extended-command :wk "M-x")
   ","       '(consult-buffer :wk "Switch buffer")
   "."       '(find-file :wk "Find File")
   "TAB TAB" '(comment-line :wk "Comment lines")
   "u"       '(universal-argument :wk "Universal argument"))

  (mp/leader-keys
   "b"       '(:ignore t :wk "Buffers/Bookmarks")
   "b b"     '(consult-buffer :wk "Switch to buffer")
   "b w"     '(consult-buffer-other-window :wk "Switch to buffer other window")
   "b f"     '(consult-buffer-other-frame :wk "Switch to buffer other frame")
   "b t"     '(consult-buffer-other-tab :wk "Switch to buffer other tab")
   "b k"     '(kill-current-buffer :wk "Kill current buffer")
   "b K"     '(kill-buffer :wk "Kill buffers")
   "b c"     '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
   "b C"     '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
   "b n"     '(next-buffer :wk "Next buffer")
   "b p"     '(previous-buffer :wk "Previous buffer")
   "b P"     '(consult-project-buffer :wk "Consult Project buffers")
   "b r"     '(revert-buffer :wk "Reload buffer")
   "b R"     '(rename-buffer :wk "Rename buffer")
   "b s"     '(basic-save-buffer :wk "Save buffer")
   "b S"     '(save-some-buffers :wk "Save multiple buffers")
   "b i"     '(ibuffer :wk "Ibuffer Page")
   "b d"     '(bookmark-delete :wk "Delete bookmark")
   "b m"     '(bookmark-set :wk "Create bookmark")
   "b j"     '(consult-bookmark :wk "Jump to bookmark"))

  (mp/leader-keys
   "s"      '(:ignore t :wk "Search")
   "s f"    '(consult-find :wk "Find")
   "s F"    '(consult-fd :wk "Find using FD")
   "s l"    '(consult-locate :wk "Locate")
   "s g"    '(consult-grep :wk "Consult Grep")
   "s G"    '(grep :wk "Grep Page")
   "s r"    '(consult-ripgrep :wk "Consult RipGrep")
   "s s"    '(consult-line :wk "Consult line")
   "s S"    '(consult-line-multi :wk "Consul line multi")
   "s k"    '(consult-keep-lines :wk "Consult keep lines")
   "s u"    '(consult-focus-lines :wk "Consult focus lines")
   "s m"    '(consult-man :wk "Consult Man pages")
   "s w"    '(woman :wk "Similar to Man"))

  (mp/leader-keys
   "t"      '(:ignore t :wk "Toggle")
   "t l"    '(display-line-numbers-mode :wk "Toggle line numbers"))

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
   "f" '(:ignore t :wk "Files")    
   "f c" '((lambda () (interactive)
             (find-file (concat user-emacs-directory "papo-config.org"))) 
           :wk "Open emacs papo-config.org")
   "f e" '((lambda () (interactive)
             (dired user-emacs-directory)) 
           :wk "Open user-emacs-directory in dired")
   "f d" '(find-grep-dired :wk "Search for string in files in DIR")
   "f i" '((lambda () (interactive)
             (find-file (concat user-emacs-directory "init.el"))) 
           :wk "Open emacs init.el")
   "f u" '(sudo-edit-find-file :wk "Sudo find file")
   "f U" '(sudo-edit :wk "Sudo edit file"))

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
   "G G"     '(consult-git-grep :wk "Consult Git Grep")
   "g i"     '(magit-init :wk "Initialize git repo")
   "g l"     '(magit-log-buffer-file :wk "Magit buffer log")
   "g r"     '(vc-revert :wk "Git revert file")
   "g s"     '(magit-stage-file :wk "Git stage file")
   "g t"     '(git-timemachine :wk "Git time machine")
   "g u"     '(magit-stage-file :wk "Git unstage file"))

  (mp/leader-keys
   "p"      '(projectile-command-map :wk "Projectile"))

  (mp/leader-keys
   "h"      '(:ignore t :wk "Help")
   "h a"    '(apropos :wk "Apropose Page")
   "h b"    '(general-describe-keybindings :wk "General Describe bindings")
   "h B"    '(describe-bindings :wk "Describe bindings Page")
   "h c"    '(describe-char :wk "Describe char under cursor")
   "h e"    '(view-echo-area-messages :wk "View echo area messages")
   "h f"    '(helpful-callable :wk "Describe Function")
   "h F"    '(describe-function :wk "Describe Function")
   "h g"    '(describe-gnu-project :wk "Describe GNU Project")
   "h i"    '(consult-info :wk "Info")
   "h I"    '(describe-input-method :wk "Describe Input Method")
   ;; "h k"    '(describe-key :wk "Describe key")
   "h k"    '(helpful-key :wk "Describe key")
   "h l"    '(view-lossage :wk "Display recent keystrokes and the commands run")
   "h L"    '(describe-language-environment :wk "Describe language environment")
   "h m"    '(describe-mode :wk "Describe mode")
   "h t"    '(load-theme :wk "Load theme")
   "h V"    '(describe-variable :wk "Describe variable")
   "h v"    '(helpful-variable :wk "Describe variable")
   "h w"    '(where-is :wk "Prints keybinding for command if set")
   ;; "h x"    '(describe-command :wk "Display full documentation for command")
   "h x"    '(helpful-command :wk "Display full documentation for command")

   "h d"    '(:ignore t :wk "Emacs Documentation")
   "h d a"  '(about-emacs :wk "About Emacs")
   "h d d"  '(view-emacs-debugging :wk "View Emacs debugging")
   "h d f"  '(view-emacs-FAQ :wk "View Emacs FAQ")
   "h d m"  '(info-emacs-manual :wk "The Emacs Manual")
   "h d n"  '(view-emacs-news :wk "View Emacs News")
   "h d o"  '(describe-distribution :wk "How to obtain Emacs")
   "h d p"  '(view-emacs-problems :wk "View Emacs problems")
   "h d t"  '(view-emacs-todo :wk "View Emacs todo")
   "h d w"  '(describe-no-warranty :wk "Describe no wannanty")
   )

  (mp/leader-keys
   "w"      '(:ignore t :wk "Windows")

   ;; Window splits
   "w q"    '(evil-window-delete :wk "Close window")
   "w u"    '(delete-other-windows :wk "Unique window")
   "w n"    '(evil-window-new :wk "New window")
   "w S"    '(evil-window-split :wk "Horizontal split window")
   "w V"    '(evil-window-vsplit :wk "Vertical split window")

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

  (mp/leader-keys
   "c"   '(:ignore t :wk "Code")
   "c a" '(eglot-code-actions :wk "Actions")
   "c o" '(eglot-code-action-organize-imports :wk "Organize imports")
   "c r" '(eglot-rename :wk "Rename")
   "c f" '(eglot-format :wk "Format")
   "c i" '(imenu :wk "imenu"))

  (mp/leader-keys
   "!"   '(:ignore t :wk "Flymake/Flycheck")
   "! c" '(consult-flymake :wk "Consult Flymake")
   "! n" '(flycheck-next-error :wk "Next error")
   "! p" '(flycheck-previous-error :wk "Prev error"))

  ;;,* Mode Keybindings
  ;; (nmap
  ;;  :keymaps 'emacs-lisp-mode-map
  ;;  "K" 'elisp-slime-nav-describe-elisp-thing-at-point)

  ;; (nmap
  ;;  :keymaps 'ruby-mode-map
  ;;  "." 'xref-find-definitions
  ;;  "," 'xref-go-back
  ;;  "K" 'yari)
  )

(provide 'init-evil)
  ;;; init-evil.el ends here
