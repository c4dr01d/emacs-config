;; -*- lexical-binding: t -*-
(use-package vterm
  :straight t
  :when (not windows-p)
  :hook (build-functions . vterm-module-compile)
  :bind (:map vterm-mode-map ("<return>" . vterm-send-return))
  :init
  (map!
    "ot" '(nil :wk "vterm"))
  (add-to-list 'display-buffer-alist
               `(" \\*Install vterm\\*"
                 (display-buffer-no-window)
                 (allow-no-window . t)))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  (vterm-tramp-shells '(("docker" "/bin/bash")))
  :config
  (imap! :keymaps 'vterm-mode-map
    "C-l" #'vterm-send-right
    "C-h" #'vterm-send-left
    "C-k" #'vterm-send-up
    "C-j" #'vterm-send-down))

(use-package multi-vterm
  :straight t
  :when (not windows-p)
  :init
  (map!
    "otT" #'multi-vterm
    "ott" #'multi-vterm-dedicated-toggle
    "otp" #'multi-vterm-project)
  (add-to-list 'display-buffer-alist
               `("\\*vterminal - .*\\*"
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  :custom
  (multi-vterm-dedicated-window-height-percent 30)
  :config
  (advice-add 'multi-vterm-dedicated-open :after
              (defun multi-vterm--remote-change-working-directory:after-a (&rest _)
                (if-let ((dir (file-remote-p default-directory 'localname)))
                    (vterm-send-string (format "cd %S\n" dir)))))
  (nvmap!
    :keymaps 'vterm-mode-map
    ",c" #'multi-vterm
    ",n" #'multi-vterm-next
    ",p" #'multi-vterm-prev
    "<return>" #'evil-insert-resume))

(use-package with-editor
  :straight t
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . with-editor-export-all)
  :init
  (defvar with-editor-ignore-matching-buffers '("\\*julia\\*"))
  (defun +with-editor-export-all ()
    (unless (seq-some (apply-partially-right #'string-match-p (buffer-name)) with-editor-ignore-matching-buffers)
      (with-editor-export-editor)
      (with-editor-export-hg-editor)
      (with-editor-export-git-editor)))
  :bind (("<remap> <async-shell-command>" . with-editor-async-shell-command)
         ("<remap> <shell-command>" . with-editor-shell-command)))

(use-package guix
  :straight t
  :when (executable-find "guix")
  :init (map! "og" #'guix))

(provide 'init-tools)
