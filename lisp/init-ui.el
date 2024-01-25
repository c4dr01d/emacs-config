;; -*- lexical-binding: t -*-
(advice-add 'load-theme
	    :before (defun disable-previous-themes:before-a (&rest _)
		      "Disable previously enabled before enabling the new one."
		      (mapc #'disable-theme custom-enabled-themes)))

(use-package nerd-icons
  :straight t
  :hook (build-functions . nerd-icons-install-fonts)
  :config
  (setcdr (assoc "m" nerd-icons-extension-icon-alist)
	  '(nerd-icons-mdicon "nf-md-math_integral_box" :face nerd-icons-orange))
  (when (and (display-graphic-p) (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts 'dont-ask)))

(use-package doom-themes
  :straight t
  :config (doom-themes-org-config))

(use-package dashboard
  :straight t
  :after evil evil-collection
  :demand t
  :init (map! "oD" #'dashboard-open)
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-banner-ascii "GNU Emacs")
  (dashboard-banner-logo-title "Welcome to GNU Emacs!")
  (dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
  (dashboard-image-banner-max-width 600)
  (dashboard-projects-backend 'project-el)
  :config
  (evil-collection-dashboard-setup)

  (unless (cl-some #'buffer-file-name (buffer-list))
    (dashboard-open)))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes
   '(markdown-mode gfm-mode org-mode rst-mode latex-mode tex-mode text-mode))
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus
      github debug repl lsp minor-modes input-method indent-info buffer-encoding
      major-mode process vcs checker time "  ")))

(use-package solaire-mode
  :straight t
  :hook (after-init . solaire-global-mode)
  :config
  (dolist (face '(mode-line mode-line-active mode-line-inactive mode-line-emphasis))
    (setf (alist-get face solaire-mode-remap-alist) nil)))
(provide 'init-ui)
