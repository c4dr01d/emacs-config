;; -*- lexical-binding: t -*-
(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay nil)
  (which-key-ellipsis "..")
  (which-key-prefix-prefix "+")
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-max-display-columns nil)
  (which-key-allow-multiple-replacements t)
  :config
  (which-key-setup-minibuffer))

(use-package general
  :straight t
  :after evil
  :demand t
  :config
  (general-auto-unbind-keys)
  (general-evil-setup t)

  (general-create-definer define-leader-key!
    :states '(insert emacs visual normal)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (general-create-definer define-localleader-key!
    :states '(insert emacs visual normal)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "M-SPC m")

  (define-leader-key!
    "SPC" '(execute-extended-command :wk "M-x")
    ">" '(switch-to-next-buffer :wk "Next buffer")
    "<" '(switch-to-prev-buffer :wk "Previous buffer")
    ";" '(pp-eval-expression :wk "Eval expression")
    ":" #'project-find-file
    "X" #'org-capture
    "u" '(universal-argument :wk "C-u")
    "C" #'universal-coding-system-argument
    "O" #'other-window-prefix

    "q" '(nil :wk "quit/session")
    "qq" #'save-buffers-kill-terminal
    "qQ" #'kill-emacs
    "qS" #'server-start
    "qR" #'recover-session
    "qd" #'desktop-read
    "qD" #'desktop-lazy-complete
    "qs" #'desktop-save

    "f" '(nil :wk "file")
    "fS" '(write-file :wk "Save as ...")
    "fi" #'auto-insert
    "ff" #'find-file
    "fs" #'save-buffer
    "ft" #'recover-this-file
    "fT" #'recover-file
    "fE" `(,(commandify! (dired user-emacs-directory))
           :wk "User config directory")

    "b" '(nil :wk "buffer")
    "bI" #'ibuffer
    "bx" #'bury-buffer
    "bS" #'save-some-buffers
    "bs" #'scratch-open-project-scratch-buffer
    "bM" #'view-echo-area-messages
    "bk" `(,(commandify! (kill-buffer (current-buffer)))
             :wk "Kill this buffer")
    "br" '(revert-buffer :wk "Revert")
    "bR" '(rename-buffer :wk "Rename")

    "bm" '(nil :wk "Bookmark")
    "bmm" #'bookmark-set
    "bmd" #'bookmark-delete

    "bv" '(nil :wk "locals")
    "bvv" '(add-file-local-variable :wk "Add")
    "bvV" '(delete-file-local-variable :wk "Delete")
    "bvp" '(add-file-local-variable-prop-line :wk "Add in prop line")
    "bvP" '(delete-file-local-variable-prop-line :wk "Delete from prop line")
    "bvd" '(add-dir-local-variable :wk "Add to dir-locals")
    "bvD" '(delete-dir-local-variable :wk "Delete from dir-locals")
    "bvr" '(nil :wk "reload dir-locals for...")
    "bvrr" '(dir-locals-reload-for-this-buffer :wk "This buffer")
    "bvrd" '(dir-locals-reload-for-all-buffers-in-this-directory :wk "All buffers in this directory")

    "w" '(nil :wk "window")
    "wd" #'delete-window
    "wD" #'delete-windows-on
    "wo" #'delete-other-windows
    "wm" #'maximize-window
    "wu" #'winner-undo
    "wU" #'winner-redo

    "m" '(nil :wk "mode-specific")

    "t" '(nil :wk "toggle")
    "td" #'toggle-debug-on-error
    "tr" #'read-only-mode
    "tl" #'follow-mode
    "tv" #'visible-mode
    "tf" #'flymake-mode

    "h" '(nil :wk "help")
    "hi" #'info
    "hg" #'general-describe-keybindings
    "he" '(nil :wk "elisp/emacs")
    "hes" #'elisp-index-search
    "hem" #'info-emacs-manual
    "hei" #'Info-search
    "hd" '(nil :wk "describe")
    "hdk" #'describe-key
    "hdm" #'describe-keymap
    "hdb" #'describe-bindings
    "hds" #'describe-symbol
    "hdv" #'describe-variable
    "hdc" #'describe-command
    "hdf" #'describe-function
    "hdp" #'describe-package

    "p" '(nil :wk "project")
    "pw" #'project-switch-project
    "pc" #'project-compile
    "pd" #'project-find-dir
    "pf" #'project-find-file
    "pk" #'project-kill-buffers
    "pb" #'project-switch-to-buffer
    "pD" #'dir-locals-open-or-create
    "p-" #'project-dired
    "px" #'project-execute-extended-command

    "pc" #'project-compile

    "pr" '(nil :wk "run")
    "pre" #'project-eshell
    "prs" #'project-shell
    "prc" #'project-shell-command
    "prC" #'project-async-shell-command

    "pF" '(nil :wk "forget/cleanup")
    "pFp" #'project-forget-project
    "pFu" #'project-forget-projects-under
    
    "ps" '(nil :wk "search/replace")
    "pss" #'project-search
    "psn" '(fileloop-continue :wk "Next match")
    "psr" #'project-query-replace-regexp
    "psf" #'project-find-regexp)

  (general-def
    :keymaps 'universal-argument-map
    :prefix "SPC"
    :global-prefix "M-SPC"
    "u" #'univeral-argument-more)

  (provide 'general-initialized))

(use-package hydra
  :straight t)
(provide 'init-keymaps)
