;;; backports-29.el --- Backports for Emacs 29 -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(unless (= emacs-major-version 28)
  (user-error "This file should only be called from an Emacs 28.x"))

(require 'cl-lib)
(require 'cl-macs)
(require 'cl-extra)
(require 'rx)
(require 'wid-edit)

(defmacro with-memoization (place &rest code)
  "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
  (declare (indent 1)
           (debug (gv-place body)))
  (gv-letplace (getter setter) place
    `(or ,getter ,(macroexp-let2 nil val (macroexp-progn code)
                   `(progn
                      ,(funcall setter val)
                      ,val)))))

(defun startup-redirect-eln-cache (cache-directory)
  "Redirect the user's eln-cache directory to CACHE-DIRECTORY.
CACHE-DIRECTORY must be a single directory, a string.
This function destructively changes `native-comp-eln-load-path'
so that its first element is CACHE-DIRECTORY.  If CACHE-DIRECTORY
is not an absolute file name, it is interpreted relative
to `user-emacs-directory'.
For best results, call this function in you early-init file,
so that the rest of initialization and package loading uses
the updated value."
  (setq native-comp-eln-load-path (cdr native-comp-eln-load-path))
  (push (expand-file-name (file-name-as-directory cache-directory)
                          user-emacs-directory)
        native-comp-eln-load-path))

(defun scratch-buffer ()
  "Switch to the *scratch* buffer.
If the buffer doesn't exist, create it first."
  (interactive)
  (pop-to-buffer-same-window (get-scratch-buffer-create)))

(defun native-compile-prune-cache ()
  "Remove .eln files that aren't applicable to the current Emacs invocation."
  (interactive)
  (unless (featurep 'native-compile)
    (user-error "This Emacs isn't built with native-compile support"))
  (dolist (dir (butlast native-comp-eln-load-path))
    (setq dir (expand-file-name dir invocation-directory))
    (when (file-exists-p dir)
      (dolist (subdir (seq-filter
                       (lambda (f)
                         (not (string-match (rx "/." (? ".") eos) f)))
                       (directory-files dir t)))
        (when (and (file-directory-p subdir)
                   (file-writable-p subdir)
                   (not (equal (file-name-nondirectory
                                (directory-file-name subdir))
                               comp-native-version-dir)))
          (message "Deleting `%s'..." subdir)
          (dolist (eln (directory-files subdir t "\\.eln\\(\\.tmp\\)?\\'"))
            (when (file-writable-p eln)
              (delete-file eln)))
          (when (directory-empty-p subdir)
            (delete-directory subdir))))))
  (message "Cache cleared"))

(defmacro setopt (&rest pairs)
  "Set VARIABLE/VALUE pairs, and return the final VALUE.
This is like `setq', but is meant for user options instead of
plain variables.  This means that `setopt' will execute any
`custom-set' form associated with VARIABLE.

\(fn [VARIABLE VALUE]...)"
  (declare (debug setq))
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of variable/value members"))
  (let ((expr nil))
    (while pairs
      (unless (symbolp (car pairs))
        (error "Attempting to set a non-symbol: %s" (car pairs)))
      (push `(setopt--set ',(car pairs) ,(cadr pairs))
            expr)
      (setq pairs (cddr pairs)))
    (macroexp-progn (nreverse expr))))

(defun setopt--set (variable value)
  (custom-load-symbol variable)
  (when-let ((type (get variable 'custom-type)))
    (unless (widget-apply (widget-convert type) :match value)
      (warn "Value `%S' does not match type %s" value type)))
  (put variable 'custom-check-value (list value))
  (funcall (or (get variable 'custom-set) #'set-default) variable value))

(when (featurep 'native-compile)
  (defvaralias 'native-comp-jit-compilation 'native-comp-deferred-compilation)
  (defvaralias 'native-comp-jit-compilation-deny-list 'native-comp-deferred-compilation-deny-list))

(defvar messages-buffer-name "*Messages*")

(defalias 'string-split #'split-string)

(provide 'backports-29)
;;; backports-29.el ends here.
