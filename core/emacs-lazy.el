;;; emacs-lazy.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(when emacs-lazy-hook
  (setq emacs-lazy-hook (append (delq 'gcmh-mode (reverse emacs-lazy-hook))
                                '(gcmh-mode)))
  (if emacs-not-lazy-p
      (progn
        (log! "Loading %d lazy packages immediately." (length emacs-lazy-hook))
        (run-hooks 'emacs-lazy-hook))
    (log! "Loading %s lazy packages incrementally." (length emacs-lazy-hook))
    (apply #'eval-when-idle (append '(1) emacs-lazy-hook))))

(log! "Providing `emacs-lazy'.")

(provide 'emacs-lazy)
;;; emacs-lazy.el ends here.
