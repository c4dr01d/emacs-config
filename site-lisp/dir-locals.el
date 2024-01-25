;; -*- lexical-binding: t -*-
(defun dir-locals-reload-for-this-buffer ()
  "Reload directory-local for the curent buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)
    (message "Reloaded directory-local variables for buffer %s" (buffer-name (current-buffer)))))

(defun dir-locals-reload-for-all-buffers-in-this-directory ()
  "Reload dir-locals for all buffers in the current `default-directory'."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (equal default-directory dir)
	  (dir-locals-reload-for-this-buffer))))))

(defun dir-locals-autoreload-h ()
  "It is relevant to auto reload dir-locals for this buffer."
  (when (and (buffer-file-name)
	     (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (dir-locals-reload-for-all-buffers-in-this-directory)
    (message "Reloaded directory-local variables defined in %s." dir-locals-file)))

(defvar dir-locals-autoreload-p nil)

(defun dir-locals-toggle-autoreload (&optional enable)
  "Toggle autoloading dir-local variables after editing the \".dir-locals.el\".

If ENABLE is non-nil, force enabling autoreloading."
  (interactive)
  (if (or enable (not dir-locals-autoreload-p))
      (progn
	(remove-hook 'after-save-hook #'dir-locals-autoreload-h)
	(setq dir-locals-autoreload-p nil)
	(message "Disabled auto-reloading directory-locals."))
    (add-hook 'after-save-hook #'dir-locals-autoreload-h)
    (setq dir-locals-autoreload-p t)
    (message "Enabled auto-reloading directory-locals.")))

(defun dir-locals-open-or-create ()
  "Open or create the .dir-locals.el for the current project."
  (interactive)
  (let* ((file-name (buffer-file-name))
	 (base-dir (car (ensure-list (dir-locals-find-file file-name)))))
    (find-file
     (cond (base-dir (expand-file-name dir-locals-file base-dir))
	   ((project-current) (expand-file-name dir-locals-file (project-root (project-current))))
	   ((vc-root-dir) (expand-file-name dir-locals-file (vc-root-dir)))
	   (t (expand-file-name dir-locals-file (file-name-directory file-name)))))))
(provide 'dir-locals)
