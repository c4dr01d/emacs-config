;; -*- lexical-binding: t -*-
(defvar debug-p (and (or (getenv "EMACS_LISP_DEBUG") init-file-debug) t)
  "GNU Emacs is started in debug mode.")

(defvar verbose-p (and (or (getenv "EMACS_LISP_VERBOSE") debug-p) t)
  "GNU Emacs is started in verbose mode.")
(provide 'variable)
