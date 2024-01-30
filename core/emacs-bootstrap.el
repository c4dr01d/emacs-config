;;; emacs-bootstrap.el --- Bootstrap packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq straight-base-dir emacs-local-dir
      straight-build-dir (format "build-%s%s" emacs-version (if emacs-repository-version (format "-%s" (substring emacs-repository-version 0 8)) ""))
      straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(unless (require 'use-package nil t)
  (straight-use-package 'use-package))

(require 'emacs-use-package-extra)

(setq use-package-verbose (cond (emacs-debug-p 'debug) (emacs-verbose-p t))
      use-package-always-defer (not emacs-always-demand-p)
      use-package-always-demand emacs-always-demand-p
      use-package-expand-minimally (not emacs-debug-p))

;;;###autoload
(defun straight-prune-build-cache ()
  "Prune straight.el build directories for old Emacs versions."
  (let* ((default-directory (file-name-concat straight-base-dir "straight/")))
    (straight-prune-build)
    (mapc (apply-partially-right #'delete-file-or-directory 'trash 'recursive)
          (seq-filter (lambda (name)
                        (not (member name (list straight-build-dir (concat straight-build-dir "-cache.el") "versions" "repos"))))
                      (directory-files default-directory "[^.][^.]?\\'")))))
(provide 'emacs-bootstrap)
;;; emacs-bootstrap.el ends here.
