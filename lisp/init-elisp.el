;; -*- lexical-binding: t -*-
(use-package macrostep
  :straight t
  :init
  (map-local! :keymaps '(emacs-lisp-mode-map lisp-mode-map)
    "m" '(macrostep-expand :wk "Expand macro")))

(use-package elisp-demos
  :straight t
  :after elisp-mode
  :demand t
  :init
  (map! :infix "he"
    "d" #'elisp-demos-find-demo
    "D" #'elisp-demos-add-demo)
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package helpful
  :straight t
  :init
  (map! :keymaps 'emacs-lisp-mode-map
    :infix "h"
    "p" #'helpful-at-point
    "o" #'helpful-symbol
    "c" #'helpful-command
    "F" #'helpful-function
    "f" #'helpful-callable))

(provide 'init-elisp)
