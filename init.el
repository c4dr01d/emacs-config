;;; init.el --- Emacs core initialization file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(let ((minimal-version "28.0"))
  (when (and (version< emacs-version minimal-version)
             (not (getenv "EMACS_IGNORE_VERSION_CHECK")))
    (error "Emacs %s is not supported, Emacs requires %s or higher" emacs-version minimal-version)))

(put 'file-name-handler-alist 'original-value (default-toplevel-value 'file-name-handler-alist))
(set-default-toplevel-value 'file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (defun emacs--restore-file-name-handler-alist-h ()
            (setq file-name-handler-alist (delete-dups (append file-name-handler-alist
                                                               (get 'file-name-handler-alist 'original-value)))))
          100)

(unless (featurep 'emacs-vars)
  (load (expand-file-name "core/emacs-vars.el" (file-name-directory (file-truename load-file-name))) nil t))

(setq load-path (append (list emacs-core-dir emacs-elisp-dir emacs-extras-dir) load-path))

(require 'emacs-lib)

(setq user-emacs-directory emacs-local-dir)

(when (< emacs-major-version 29)
  (let ((backports-dir (concat emacs-core-dir "backports/")))
    (mapc (apply-partially #'load-wrapper backports-dir) (directory-files backports-dir nil "\\.el\\'"))))

(setq debug-on-error emacs-debug-p
      warning-minimum-level (if emacs-verbose-p :warning :error)
      warning-minimum-log-level warning-minimum-level
      byte-compile-warnings emacs-verbose-p
      byte-compile-verbose emacs-verbose-p)

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors (when emacs-verbose-p 'silent)
        native-comp-verbose (if emacs-verbose-p 1 0)
        native-comp-debug (if emacs-debug-p 1 0)
        native-comp-jit-compilation t)

  (startup-redirect-eln-cache (concat emacs-cache-dir "eln/")))

(defun emacs-generate-loaddefs ()
  "Generate Emacs loaddefs file."
  (interactive)
  (when (file-exists-p emacs-loaddefs-file)
    (delete-file emacs-loaddefs-file))
  (apply (if (fboundp 'loaddefs-generate)
             #'loaddefs-generate
           #'make-directory-autoloads)
         (list (list emacs-core-dir emacs-elisp-dir emacs-extras-dir) emacs-loaddefs-file)))

(unless (file-exists-p emacs-loaddefs-file)
  (emacs-generate-loaddefs))

(load-wrapper emacs-loaddefs-file)

(load-user-configs 'init-config 'local/init-config)

(env-load)

(defun emacs--loaded-h ()
  "This is Emacs synchronization point.

To achieve fast Emacs startup, we tries to defer loading most of the packages
until this hook is executed. This is managed by the `emacs-loaded' and
`emacs-lazy' features.

After loading Emacs, the `emacs-startup-hook' gets executed, we use this hook to profile the startup time, load the theme, and make a persistent scratch buffer.

Lastly we require the `emacs-loaded' synchronization module, which runs the
`emacs-after-startup-hook' hooks and provide `emacs-loaded' so the
packages loaded with `:after emacs-loaded' can be loaded.

The `emacs-loaded' will require `emacs-lazy' when Emacs goes idle, this
provides `emacs-lazy' so the packages loaded with `:after emacs-lazy' can
be loaded then it incrementally run the hooks in `emacs-lazy-hook' when Emacs
goes idle."
  (info! "Loaded Emacs %s in %s." (if (daemonp) "(in daemon mode)" "") (emacs-init-time))
  (unless (featurep 'emacs-org-export-async-init)
    (load-theme-wrapper))

  (require 'emacs-loaded))

(add-hook 'emacs-startup-hook #'emacs--loaded-h -101)

(make-first-file-hook! 'org "\\.org$")
(make-first-file-hook! 'elisp "\\.elc?$")
(make-first-file-hook! 'python (rx "." (or "py" "pyw" "pyx" "pyz" "pyzw") eol))
(make-first-file-hook! 'c/c++ (rx "." (or "c" "cpp" "cxx" "cc" "c++" "h" "hpp" "hxx" "hh" "h++" "ixx" "cppm" "cxxm" "c++m" "ccm") eol))
(make-first-file-hook! nil ".")

(if (featurep 'emacs-org-export-async-init)
    (progn (message "Loading \"init.el\" in an org-export-async context.")
           (setq emacs-not-lazy-p t))
  (load-wrapper emacs-core-dir "emacs-modules.el")
  (load-user-configs 'modules 'local/modules))

(setq emacs-core-modules
      (delete-dups (append (when 'emacs-splash emacs-core-modules) '(emacs-splash))
                   '(emacs-bootstrap)
                   (when (< emacs-major-version 29) '(emacs-compat))
                   '(emacs-builtin emacs-gc)
                   emacs-core-modules))

(dolist (module-file (append
                      (mapcar (apply-partially #'format "%s%s.el" emacs-core-dir) emacs-core-modules)
                      (mapcar (apply-partially #'format "%s%s.el" emacs-modules-dir) emacs-modules)))
  (load-wrapper module-file))

(setq custom-file (concat emacs-config-dir "custom-vars.el"))

(when (file-exists-p custom-file)
  (load-wrapper custom-file))

(load-user-configs 'config 'local/config)

(lazy!
 (when (featurep 'native-compile)
   (info! "Trying to clean outdated native compile cache")
   (shutup! (native-compile-prune-cache)))
 (info! "Trying to clean outdated straight build cache")
 (shutup! (+straight-prune-build-cache))
 (info! "Trying to clean MinEmacs' root directory")
 (shutup! (+minemacs-root-dir-cleanup)))


(log! "Loaded init.el")
;;; init.el ends here.
