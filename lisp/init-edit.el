;; -*- lexical-binding: t -*-
(use-package autoinsert
  :custom
  (auto-insert-directory (directory-ensure user-emacs-directory "auto-insert/")))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t))

(use-package electric
  :config
  (setq-default electric-indent-chars '(?\n ?\^?))

  (defvar-local electric-indent-words '()
    "The list of electric words. Typing these will trigger reindentation of the
current line.")

  (setq-hook! (sh-mode bash-ts-mode)
    electric-indent-words
    (delete-dups (apply #'append (mapcar (lambda (e)
					   (list (car e) (cdr e)))
					 (cdar sh-smie-sh-grammar)))))

  (add-hook 'electric-indent-functions
	    (defun electric-indent-char-fn (_c)
	      (when (and (eolp) electric-indent-words)
		(save-excursion
		  (backward-word)
		  (looking-at-p (concat "\\<" (regexp-opt electric-indent-words))))))))

(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :init
  (defun electric-pair-tweaks-h ()
    (with-eval-after-load 'elec-pair
      (setq-local electric-pair-inhibit-predicate `(lambda (char)
						     (if (char-equal char ?<) t (,electric-pair-inhibit-predicate char)))
		  electric-pair-pairs (append electric-pair-pairs (alist-get major-mode electric-pair-mode-pairs-alist)))))

  (defvar electric-pair-mode-pairs-alist
    '((org-mode . ((?= . ?=) (?~ . ?~) (?` . ?`)))
      (markdown-mode . ((?` . ?`) (?* . ?*)))))

  (dolist (mode (mapcar #'car electric-pair-mode-pairs-alist))
    (add-hook (intern (format "%s-hook" mode)) #'electric-pair-tweaks-h)))

(use-package savehist
  :hook (after-init . savehist-mode)
  :custom (savehist-file (concat user-emacs-directory "savehist.el")))

(use-package saveplace
  :init (save-place-mode 1)
  :custom (save-place-file (concat user-emacs-directory "save-place.el")))

(use-package display-line-numbers
  :hook ((prog-mode conf-mode text-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 4)
  (display-line-numbers-widen t))

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package vimish-fold
  :straight t
  :hook (after-init . vimish-fold-global-mode))

(use-package evil-vimish-fold
  :straight t
  :hook (vimish-fold-global-mode . global-evil-vimish-fold-mode)
  :commands evil-vimish-fold/next-fold evil-vimish-fold/previous-fold vimish-fold/delete evil-vimish-fold/delete-all evil-vimish-fold/create evil-vimish-fold/create-line
  :custom
  (vimish-fold-dir (concat user-emacs-directory "vimish-fold/"))
  (vimish-fold-indication-mode 'right-fringe)
  :init
  (with-eval-after-load 'evil
    (evil-define-key* 'motion 'global
      "zf" #'evil-vimish-fold/create
      "zF" #'evil-vimish-fold/create-line
      "zd" #'vimish-fold-delete
      "zE" #'vimish-fold-delete-all)))
(provide 'init-edit)
