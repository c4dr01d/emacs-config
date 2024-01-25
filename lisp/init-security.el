;; -*- lexical-binding: t -*-
(use-package password-cache
  :custom
  (password-cache t)
  (password-cache-expiry 60))

(use-package auth-source
  :custom
  (auth-sources '("~/.authinfo.gpg"))
  (auth-source-do-cache t)
  (auth-source-cache-expiry 86400))

(use-package epa
  :custom
  (epg-pinentry-mode 'loopback))

(use-package epa-file
  :demand t
  :config (epa-file-enable))

(provide 'init-security)
