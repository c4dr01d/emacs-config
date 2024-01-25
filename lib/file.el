;; -*- lexical-binding: t -*-
(defun directory-ensure (&rest path-parts)
  "Concatenate PATH-PARTS to construct a path and return it.

Ensure the path exists, if not create it. The exact behavior it to create the
parent directory if the path is a file, and if the path is directory, create
that directory."
  (let* ((path (mapconcat #'identity path-parts nil))
		 (parent-dir (file-name-directory path)))
	(unless (file-directory-p parent-dir)
	  (ignore-errors (mkdir parent-dir t))
	  (unless (file-directory-p parent-dir)
		(error "Cannot create directory %s" parent-dir)))
	path))

(provide 'file)
