;; -*- lexical-binding: t -*-
(use-package tramp
  :init
  (unless windows-p
    (setq tramp-default-method "ssh"))
  (setq tramp-persistency-file-name (concat user-emacs-directory "tramp/persistency.el"))
  :custom
  (tramp-auto-save-directory (concat user-emacs-dir "tramp/auto-save/"))
  (tramp-default-remote-shell "/bin/bash"))

(provide 'init-tramp)
