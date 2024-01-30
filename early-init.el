;;; early-init.el --- Emacs early initialization tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum
      load-prefer-newer t)

(when (>= emacs-major-version 29)
  (when-let* ((alpha (getenv "EMACS_ALPHA"))
              (alpha (string-to-number alpha)))
    (push (cons 'alpha-background (if (or (zerop alpha) (> alpha 100)) 93 alpha))
          default-frame-alist)))

(load (expand-file-name "core/emacs-vars.el" (file-name-directory (file-truename load-file-name))) nil t)

(when (and os-darwin-p (featurep 'ns))
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; (load-user-configs 'early-config 'local/early-config)
;;; early-init.el ends here.
