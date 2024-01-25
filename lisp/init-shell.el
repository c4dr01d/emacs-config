;; -*- lexical-binding: t -*-
(use-package eshell
  :custom
  (eshell-aliases-file (concat user-emacs-directory "eshell/aliases"))
  (eshell-directory-name (directory-ensure user-emacs-directory "eshell/"))
  (eshell-history-file-name (concat user-emacs-directory "eshell/history.el"))
  (eshell-last-dir-ring-file-name (concat user-emacs-directory "eshell/last-dir-ring.el"))
  (eshell-login-script (concat user-emacs-directory "eshell/login"))
  (eshell-rc-script (concat user-emacs-directory "eshell/rc"))
  (eshell-scroll-to-bottom-on-input 'this))

(use-package term
  :config
  (advice-add 'term-sentinel
	      :around (defun term--kill-after-exit:around-a (orig-fn proc msg)
			(if (memq (process-status proc) '(signal exit))
			    (let ((buffer (process-buffer proc)))
			      (apply orig-fn (list proc msg))
			      (kill-buffer buffer))
			  (apply orig-fn (list proc msg))))))
(provide 'init-shell)
