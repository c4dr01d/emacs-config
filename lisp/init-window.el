;; -*- lexical-binding: t -*-
(use-package transient
  :straight t
  :bind (:map transient-map
	      ("q" . transient-quit-one)
	      ("<escape>" . transient-quit-one)))

(use-package tab-bar
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
  (tab-bar-tab-name-format-function #'tab-bar-tab-spaced-name-format)
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width-max '(150 20))
  (tab-bar-tab-hints t)
  (tab-bar-show nil)
  :config
  (defun tab-bar-tab-spaced-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format " %c " (+ ?❶ (1- i))) "")
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab))))
  (with-eval-after-load 'nerd-icons
    (setq tab-bar-close-button
          (propertize (concat (nerd-icons-faicon "nf-fa-close" :height 0.5) " ")
                      'close-tab t :help "Click to close tab"))))

(use-package winner
  :hook (after-init . winner-mode))

(use-package windmove
  :demand t
  :config
  (windmove-default-keybindings 'shift))

(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window))
  :custom
  (aw-dispatch-always t)
  :init
  (map! "wa" #'ace-window))
(provide 'init-window)
