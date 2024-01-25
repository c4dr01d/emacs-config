;; -*- lexical-binding: t -*-
(defvar build-functions nil
  "Special hook for build functions that are run after completing package
updates.")

(defun run-build-functions (&optional dont-ask-p)
  "Run all build functions in `build-functions'."
  (interactive "P")
  (dolist (fn build-functions)
    (message "Running `%s'" fn)
    (if dont-ask-p
	(cl-letf (((symbol-function 'yes-or-no-p) #'always)
		  ((symbol-function 'y-or-n-p) #'always))
	  (funcall-interactively fn))
      (funcall-interactively fn))))
(provide 'building)
