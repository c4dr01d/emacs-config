;;; emacs-loaded.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(when emacs-after-startup-hook
  (setq emacs-after-startup-hook (reverse emacs-after-startup-hook))
  (log! "Running %d `emacs-after-startup-hook' hooks."
        (length emacs-after-startup-hook))
  (run-hooks 'emacs-after-startup-hook))

(if emacs-not-lazy-p
    (require 'emacs-lazy)
  (eval-when-idle-for! 2 (require 'emacs-lazy)))

(log! "Providing `emacs-loaded'.")

(provide 'emacs-loaded)
;;; emacs-loaded.el ends here.
