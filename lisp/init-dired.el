;; -*- lexical-binding: t -*-
(use-package dired
  :hook (dired-mode . turn-on-gnus-dired-mode)
  :custom
  (dired-dwim-target t)
  (dired-auto-revert-buffer t))

(use-package image-dired
  :custom
  (image-dired-dir (directory-ensure user-emacs-directory "image-dired/"))
  (image-dired-tags-db-file (concat user-emacs-directory "image-dired/tags-db.el"))
  (image-dired-temp-rotate-image-file (concat user-emacs-directory "image-dired/temp-rotate-image")))

(use-package dirvish
  :straight t
  :demand t
  :custom
  (dirvish-attributes '(subtree-state nerd-icons file-size vc-state git-msg))
  (dirvish-cache-dir (directory-ensure user-emacs-directory "dirvish/"))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t)
  (dirvish-use-mode-line t)
  :init
  (map!
    "o-" '(dirvish :wk "Dirvish")
    "oq" '(dirvish-quick-access :wk "Dirvish quick access")
    "sd" '(dirvish-fd :wk "Dirvish fd"))
  :config
  (nvmap! :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit
    "s" #'dirvish-subtree-toggle
    "y" #'dirvish-yank-menu)

  (dirvish-override-dired-mode 1))

(use-package dired-rsync
  :straight (:files ("dired-rsync.el" "dired-rsync-transient.el"))
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)
              ("C-c C-x" . dired-rsync-transient)))
(provide 'init-dired)
