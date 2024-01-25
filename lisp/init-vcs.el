;; -*- lexical-binding: t -*-
(use-package magit
  :straight t
  :init
  (map! :infix "g"
    "g" #'magit-status
    "C" #'magit-clone
    "b" #'magit-blame
    "l" #'magit-log
    "d" #'magit-diff-dwim
    "s" #'magit-stage
    "i" #'magit-init)
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

(use-package magit-todos
  :straight t
  :after magit
  :demand t
  :config
  (magit-todos-mode 1))

(use-package magit-imerge
  :straight t
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-merge "m"
      '("M" "magit-imerge" magit-imerge))))

(use-package git-timemachine
  :straight t
  :init
  (map! "gt" #'git-timemachine-toggle)
  :custom
  (git-timemachine-show-minibuffer-details t))

(use-package git-commit
  :after magit
  :demand t
  :custom
  (git-commit-summary-max-length 72)
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :config
  (evil-set-initial-state 'git-commit-mode 'insert)
  (global-git-commit-mode 1))

(use-package git-modes
  :straight t
  :init
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package diffview
  :straight t
  :init
  (with-eval-after-load 'diff-mode
    (map-local! :keymaps 'diff-mode-map
      "v" #'diffview-current
      "V" #'diffview-region))
  :config
  (nvmap! :keymaps 'diffview--mode-map
    "=" #'diffview--align-windows
    "+" #'diffview--align-windows
    "C-j" #'diffview--next-file
    "C-k" #'diffview--prev-file
    "q" #'diffview--quit))
(provide 'init-vcs)
