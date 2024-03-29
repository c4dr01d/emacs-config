;; -*- lexical-binding: t -*-
(defgroup completion nil
  "Completion related stuff.")

(use-package cape
  :straight t
  :demand t
  :init
  (defcustom cape-global-capes '(tempel-complete :completion cape-dict)
    "A list of global capes to be available at all times.
The key `:completion' is used to specify where completion candidates shoule be
placed, otherwise they come first."
    :group 'completion
    :type '(repeat symbol))
  (defcustom cape-hosts '(eglot-completion-at-point
			  lsp-completion-at-point
			  elisp-completion-at-point
			  tags-completion-at-point-function)
    "A prioritised list of host capfs to create a super cape onto from
`cape-global-capes'."
    :group 'completion
    :type '(repeat function))
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

  (add-hook! 'completion-at-point-functions '(cape-file cape-keyword cape-dict))

  (add-hook! (emacs-lisp-mode git-commit-mode)
    (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t))

  (add-hook! org-mode
    (add-hook 'completion-at-point-functions #'cape-elisp-block nil t))

  (defun cape-apply-capf-super ()
    "Apply Capf super to call capes specified in `cape-global-capes' and `cape-hosts'."
    (interactive)
    (when-let ((host (cl-intersection cape-hosts completion-at-point-functions)))
      (setq-local
       completion-at-point-functions
       (cl-substitute (apply #'cape-capf-super
			     (cl-substitute (car host)
					    :completion
					    (append (cl-pushnew :completion cape-global-capes))))
		      (car host)
		      completion-at-point-functions))))

  (defun toggle-cape-auto-capf-super (&optional disable)
    "Enable auto generating Cape's super Capf.
This depends on `cape-hosts' and `cape-global-capes'."
    (interactive)
    (let ((enabled (get 'cape-auto-capf-super 'enabled)))
      (dolist (hook '(lsp-mode-hook eglot-managed-mode-hook change-major-mode-hook))
	(apply (if (or enabled disable) #'remove-hook #'add-hook) (list hook #'cape-apply-capf-super))
	(put 'cape-auto-capf-super 'enabled (not (or enabled disable)))))))

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook ((prog-mode . corfu-mode)
	 (eshell-mode . corfu-less-intrusive-h)
	 (minibuffer-setup . corfu-enable-in-minibuffer-h))
  :bind (:map corfu-map
	      ("M-m" . corfu-complete-in-minibuffer)
	      ("<tab>" . corfu-next)
	      ("<backtab>" . corfu-previous)
	      ("C-j" . corfu-next)
	      ("C-k" . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-min-width 25)
  (corfu-auto-delay 0.2)
  :config
  (defun corfu-enable-in-minibuffer-h ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))

  (defun corfu-less-intrusive-h ()
    (setq-local corfu-quit-at-boundary t
		corfu-quit-no-match t
		corfu-auto nil)
    (corfu-mode 1))

  (defun corfu-complete-in-minibuffer ()
    "Move current completions to the minibuffer."
    (interactive)
    (let ((completion-extra-properties corfu--extra)
	  completion-cycle-threshold
	  completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data))))

(use-package corfu-popupinfo
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (:package corfu
		  :map corfu-map
		  ("M-p" . corfu-popupinfo-scroll-down)
		  ("M-n" . corfu-popupinfo-scroll-up)
		  ("M-d" . corfu-popupinfo-toggle))
  :custom
  (corfu-popupinfo-delay 0.1)
  (corfu-popupinfo-max-height 15))

(use-package corfu-history
  :hook (corfu-mode . corfu-history-mode)
  :config
  (unless (bound-and-true-p savehist-mode)
    (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-terminal
  :straight t
  :hook (corfu-mode . corfu-terminal-mode))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :demand t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package consult
  :straight t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-local-map
	      ("C-r" . consult-history)
	      ("C-S-v" . consult-yank-pop)
	      :package isearch
	      :map isearch-mode-map
	      ("C-S-v" . consult-yank-pop))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (register-preview-function #'consult-register-format)
  :init
  (map!
    "bll" #'consult-line
    "blf" #'consult-focus-lines
    "blk" #'consult-keep-lines
    "blg" #'consult-goto-line
    "bb" #'consult-buffer
    "bB" #'consult-buffer-other-window
    "bF" #'consult-buffer-other-frame
    "bmM" #'consult-bookmark
    "bi" #'consult-imenu
    "bO" #'consult-outline

    "fr"  #'consult-recent-file

    "gG"  #'consult-git-grep

    "ss" (if (executable-find "rg") #'consult-ripgrep #'consult-grep)
    "sS" (if (executable-find "rg") #'consult-grep #'consult-ripgrep)
    "sf" (if (executable-find "fd") #'consult-fd #'consult-find)
    "sF" (if (executable-find "fd") #'consult-find #'consult-fd)
    "sM" #'consult-man
    "st" #'consult-locate
    "sh" #'consult-history
    "sa" #'consult-org-agenda
    "sl" #'consult-locate
    "si" #'consult-isearch-history

    "pl" #'consult-line-multi
    "pi" #'consult-imenu-multi

    "cm" #'consult-flymake
    "cE" #'consult-compile-error

    "ec" #'consult-complex-command

    "iy" #'consult-yank-from-kill-ring
    "ip" #'consult-yank-pop
    "ir" '(nil :wk "register")
    "irr" #'consult-register
    "irl" #'consult-register-load
    "irs" #'consult-register-store

    "hu" #'consult-theme
    "hI" #'consult-info)

  (map-local! :keymaps 'org-mode-map
    "h" #'consult-org-heading)
  :config
  (setq-default completion-in-region-function #'consult-completion-in-region)

  (consult-customize
   consult-find :initial (region-or-thing-at-point)
   consult-grep :initial (region-or-thing-at-point)
   consult-line :initial (region-or-thing-at-point)
   consult-line-multi :initial (region-or-thing-at-point)
   consult-man :initial (region-or-thing-at-point)
   consult-ripgrep :initial (region-or-thing-at-point)))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
	 :package vertico
	 :map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file))
  :init
  (map! "ed" #'consult-dir))

(use-package embark
  :straight t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (map!
    "a" #'embark-act
    "A" #'embark-collect))

(use-package embark-consult
  :straight t
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :straight t
  :hook (after-init . marginalia-mode))

(use-package nerd-icons-completion
  :straight t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package orderless
  :straight t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      :map minibuffer-local-map
	      ("C-S-k" . kill-line))
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12))

(use-package vertico-directory
  :after vertico
  :demand t
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word)
	      ("M-h" . vertico-directory-up)))

(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-R" . vertico-repeat))

(use-package wgrep
  :straight t
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(provide 'init-completion)
