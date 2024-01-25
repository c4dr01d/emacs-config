;; -*- lexical-binding: t -*-
(defmacro map! (&rest args)
  "A wrapper around `define-leader-key!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (define-leader-key! ,@args)))

(defmacro map-local! (&rest args)
  "A wrapper around `define-localleader-key!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (define-localleader-key! ,@args)))

(defmacro nmap! (&rest args)
  "A wrapper around `general-nmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (general-nmap ,@args)))

(defmacro vmap! (&rest args)
  "A wrapper around `general-vmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (general-vmap ,@args)))

(defmacro mmap! (&rest args)
  "A wrapper around `general-mmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (general-mmap ,@args)))

(defmacro imap! (&rest args)
  "A wrapper around `general-imap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (general-imap ,@args)))

(defmacro emap! (&rest args)
  "A wrapper around `general-emap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (general-emap ,@args)))

(defmacro omap! (&rest args)
  "A wrapper around `general-omap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (general-omap ,@args)))

(defmacro rmap! (&rest args)
  "A wrapper around `general-rmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (general-rmap ,@args)))

(defmacro iemap! (&rest args)
  "A wrapper around `general-iemap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (general-iemap ,@args)))

(defmacro nvmap! (&rest args)
  "A wrapper around `general-nvmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'general-initialized
     (general-nvmap ,@args)))
(provide 'keymaps)
