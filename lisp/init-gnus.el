;; -*- lexical-binding: t -*-
(use-package gnus
  :custom
  (gnus-dribble-directory (directory-ensure user-emacs-directory "gnus/dribble/"))
  (gnus-init-file (concat user-emacs-directory "gnus/init.el"))
  (gnus-startup-file (concat user-emacs-directory "gnus/newsrc")))

(provide 'init-gnus)
