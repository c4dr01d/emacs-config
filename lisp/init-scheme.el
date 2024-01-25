;; -*- lexical-binding: t -*-
(use-package geiser
  :straight t
  :custom
  (geiser-default-implementation 'guile))

(use-package geiser-guile
  :straight t)

(use-package geiser-racket
  :straight t)

(use-package macrostep-geiser
  :straight t
  :after geiser
  :hook ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup)
  :init
  (map-local! :keymaps '(geiser-mode-map geiser-repl-mode-map)
    "m" '(macrostep-expand :wk "Expand macro")
    "M" #'macrostep-geiser-expand-all))
(provide 'init-scheme)
