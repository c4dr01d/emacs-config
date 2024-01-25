;; -*- lexical-binding: t -*-
(use-package treemacs
  :straight t
  :init
  (map! "op" #'treemacs)
  :custom
  (treemacs-persist-file (concat user-emacs-directory "treemacs/persist.el"))
  (treemacs-last-error-persist-file (concat user-emacs-directory "treemacs/last-error-persist.el"))
  (treemacs-width 30)
  :config
  (set-face-attribute 'treemacs-root-face nil :height 1.0))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs nerd-icons
  :demand t
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'init-treemacs)
