;; -*- lexical-binding: t -*-
(use-package emacs
  :hook (after-save . save--guess-file-mode-h)
  :custom
  (auto-save-list-file-prefix (directory-ensure user-emacs-directory "auto-save/"))
  (auto-save-default t)
  (auto-save-include-big-deletions t)
  (auto-save-file-name-transforms `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat auto-save-list-file-prefix "tramp-\\2") t)
				    (".*" ,auto-save-list-file-prefix t)))
  (auto-window-vscroll nil)
  (fast-but-imprecise-scrolling t)
  (scroll-preserve-screen-position t)
  (scroll-conservatively 101)
  (scroll-margin 1)
  (scroll-step 1)
  (hscroll-margin 2)
  (hscroll-step 1)
  (create-lockfiles nil)
  (make-backup-files t)
  (backup-directory-alist (list (cons "." (directory-ensure user-emacs-directory "backup/"))))
  (version-control t)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 5)
  (kept-new-versions 5)
  (dired-kept-versions 5)
  (tab-always-indent 'complete)
  (require-final-newline nil)
  (undo-limit 10000000)
  (undo-strong-limit 50000000)
  (undo-outer-limit 150000000)
  (use-system-tooltips nil)
  (window-combination-resize t)
  (frame-resize-pixelwise t)
  (inhibit-compacting-font-caches t)
  (read-process-output-max (if linux-p
			       (condition-case nil
				   (with-temp-buffer
				     (insert-file-contents "/proc/sys/fs/pipe-max-size")
				     (string-to-number (buffer-string)))
				 (error read-process-output-max))
			     (* 1024 1024)))
  (confirm-nonexistent-file-or-buffer nil)
  (enable-recursive-minibuffers t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (find-file-visit-truename t)
  (sentence-end-double-space nil)
  (delete-by-moving-to-trash nil)
  (save-some-buffers-default-predicate #'save-some-buffers-root)
  (inhibit-startup-screen t)
  (ring-bell-function #'ignore)
  (visible-bell nil)
  (large-file-warning-threshold (* 50 1024 1024))
  (initial-scratch-message ";; GNU Emacs -- start here!")
  (initial-major-mode 'fundamental-mode)
  (use-dialog-box nil)
  (use-short-answers t)
  (prettify-symbols-unprettify-at-point t)
  (display-fill-column-indicator-character ?\u250a)
  (vc-follow-symlinks t)
  (uniquify-buffer-name-style 'forward)
  (widget-image-enable nil)
  (tooltip-hide-delay 20)
  :init
  (setq-default truncate-lines nil
		fill-column 80
		tab-width 4)

  (fset 'display-startup-echo-area-message #'ignore)

  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)
  (set-language-environment "Chinese-GBK")
  (set-locale-environment "zh_CN.UTF-8")
  (set-selection-coding-system (if windows-p 'utf-16-e 'utf-8))
  :config
  (defun save--guess-file-mode-h ()
    "Guess major mode when saving a file `fundamental-mode'.
Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
    (when (eq major-mode 'fundamental-mode)
      (let ((buffer (or (buffer-base-buffer) (current-buffer))))
	(and (buffer-file-name buffer)
	     (eq buffer (window-buffer (selected-window)))
	     (set-auto-mode))))))

(use-package simple
  :hook ((after-init . line-number-mode)
         (after-init . column-number-mode)
         (after-init . size-indication-mode)
	 ((prog-mode conf-mode text-mode) . visual-line-mode))
  :init (setq-default indent-tabs-mode nil)
  :custom
  (kill-do-not-save-duplicates t)
  (save-interprogram-paste-before-kill t))

(use-package minibuffer
  :custom
  (read-file-name-completion-ignore-case t)
  (completions-detailed t))

(use-package mb-depth
  :hook (after-init . minibuffer-depth-indicate-mode))

(use-package eldoc
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package abbrev
  :custom
  (abbrev-file-name (concat user-emacs-directory "abbrev.el")))

(use-package bookmark
  :custom
  (bookmark-default-file (concat user-emacs-directory "bookmark.el"))
  (bookmark-save-flag 1))

(use-package calc
  :custom
  (calc-settings-file (concat user-emacs-directory "calc-settings.el")))

(use-package desktop
  :custom
  (desktop-base-file-name "emacs-session.el")
  (desktop-base-lock-name (concat desktop-base-file-name ".lock"))
  (desktop-restore-eager 5)
  (desktop-file-checksum t)
  (desktop-save-buffer t))

(use-package recentf
  :custom
  (recentf-save-file (concat user-emacs-directory "recentf-save.el"))
  (recentf-max-saved-items 200)
  (recentf-case-fold-search t)
  (recentf-exclude
   `(file-remote-p
     ,(rx (or "/elfeed-db/" "/eln-cache/" "/cache/" "/.maildir/" "/.cache/"))
     ,(rx bol "/tmp/")))
  :init
  (recentf-mode 1))

(use-package time
  :hook (after-init . display-time-mode)
  :custom (display-time-string-forms '((propertize (concat 24-hours ":" minutes)))))

(use-package time-stamp
  :hook (before-save . time-stamp)
  :custom
  (time-stamp-active t)
  (time-stamp-line-limit 12)
  (time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S"))

(use-package gcmh
  :straight t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 20)
  (gcmh-high-cons-threshold (* 1024 1024 256)))
(provide 'init-builtin)
