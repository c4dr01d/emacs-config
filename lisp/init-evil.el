;; -*- lexical-binding: t -*-
(use-package evil
  :straight t
  :hook (after-init . evil-mode)
  :preface
  (setq evil-want-keybinding nil
	evil-want-integration t)
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-fine-undo t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-split-window-right t)
  (evil-kill-on-visual-paste nil)
  (evil-undo-system 'undo-redo)
  (evil-respect-visual-line-mode nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  :config
  (evil-select-search-module 'evil-search-module 'isearch))

(use-package evil-collection
  :straight t
  :after evil
  :demand t
  :config
  (evil-collection-init (seq-filter
			 (lambda (mode)
			   (not (memq mode '(evil-mc elisp-mode))))
			 evil-collection-mode-list))
  (with-eval-after-load 'elisp-mode
    (when evil-collection-want-find-usages-bindings
      (evil-collection-define-key 'normal 'emacs-lisp-mode-map
	"gr" 'xref-find-references))))

(use-package evil-commentary
  :straight t
  :hook (after-init . evil-commentary-mode))

(use-package evil-surround
  :straight t
  :hook (after-init . global-evil-surround-mode))

(use-package evil-snipe
  :straight t
  :hook ((after-init . evil-snipe-mode)
	 (after-init . evil-snipe-override-mode))
  :custom
  (evil-snipe-scope 'buffer)
  (evil-snipe-smart-case t)
  (evil-snipe-auto-scroll t))

(use-package evil-numbers
  :straight t)
(provide 'init-evil)
