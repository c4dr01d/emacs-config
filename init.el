;; -*- lexical-binding: t -*-
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Library
(require 'primitive)
(require 'eval)
(require 'plist)
(require 'hook)
(require 'variable)
(require 'distro)
(require 'building)
(require 'file)
(require 'region)
(require 'command)
(require 'keymaps)

;; Configuration
(require 'init-bootstrap)
(require 'init-builtin)
(require 'init-compat)
(require 'init-evil)
(require 'init-keymaps)
(require 'init-ui)
(require 'init-edit)
(require 'init-window)
(require 'init-completion)
(require 'init-prog)
(require 'init-elisp)
(require 'init-scheme)
(require 'init-vcs)
(require 'init-security)
(require 'init-shell)
(require 'init-dired)
(require 'init-treemacs)
(require 'init-tramp)
(require 'init-tools)
(require 'init-org)

;; Third party package
(require 'binary)
(require 'scratch)
(require 'dir-locals)
(require 'valgrind)
