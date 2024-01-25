;; -*- lexical-binding: t -*-
(defvar scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `scratch-dir'.")

(defvar scratch-dir (concat user-emacs-directory "pscratch/")
  "Where to save persistent scratch buffers.")

(defvar scratch-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.

Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar scratch-buffers nil
  "A list of active scratch buffers.")

(defvar scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put scratch-current-project 'permanent-local t)

(defvar scratch-buffer-created-hook ()
  "The hooks to run after a scratch buffer is created.")

(defun scratch-load-persistent-scratch-buffer (&optional project-name)
  (setq-local scratch-current-project (or project-name scratch-default-file))
  (let ((smart-scratch-file
	 (expand-file-name (concat scratch-current-project ".el") scratch-dir)))
    (make-directory scratch-dir t)
    (when (file-readable-p smart-scratch-file)
      (message "Reading persistent scratch from %s" smart-scratch-file)
      (cl-destructuring-bind (content point mode)
	  (with-temp-buffer
	    (save-excursion (insert-file-contents smart-scratch-file))
	    (read (current-buffer)))
	(erase-buffer)
	(funcall mode)
	(insert content)
	(goto-char point)
	t))))

(defun scratch-buffer (&optional dont-restore-p mode directory project-name)
  "Return a scratchpad buffer in major MODE.

When DONT-RESTORE-P, do not load the previously saved persistent buffer. Load
persistent buffer dedicated to PROJECT-NAME when provided.

When provided, set the `default-directory' to DIRECTORY."
  (let* ((buffer-name (if project-name (format "*pscratch:%s*" project-name) "*pscratch*"))
	 (pscratch-buff (get-buffer buffer-name)))
    (with-current-buffer (or pscratch-buff (get-buffer-create buffer-name))
      (setq-local default-directory (or directory default-directory)
		  so-long--inhibited t)
      (if dont-restore-p
	  (erase-buffer)
	(unless pscratch-buff
	  (scratch-load-persistent-scratch-buffer project-name)
	  (when (and (eq major-mode 'fundamental-mode) (functionp mode))
	    (funcall mode))))
      (cl-pushnew (current-buffer) scratch-buffers)
      (hook-once! 'window-buffer-change-functions (scratch-persist-buffers-h))
      (hook-once! 'server-visit-hook (scratch-persist-buffers-h))
      (hook-once! 'window-selection-change-functions (scratch-persist-buffers-h))
      (add-hook 'kill-buffer-hook #'scratch-persist-buffer-h nil 'local)
      (run-hooks 'scratch-buffer-created-hook)
      (current-buffer))))

(defun scratch-persist-buffer-h (&rest _)
  "Save the current buffer to `scratch-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
	(curr-point (point))
	(mode major-mode))
    (with-temp-file (expand-file-name (concat (or scratch-current-project scratch-default-file) ".el") scratch-dir)
      (prin1 (list content curr-point mode) (current-buffer)))))

(defun scratch-persist-buffers-h (&rest _)
  "Save all scratch buffers to `scratch-dir'."
  (setq scratch-buffers (cl-delete-if-not #'buffer-live-p scratch-buffers))
  (dolist (buffer scratch-buffers)
    (with-current-buffer buffer
      (scratch-persist-buffer-h))))

(defun scratch-persist-buffers-after-switch-h (&rest _)
  "Kill scratch buffers when they no longer visible, saving them to disk."
  (unless (cl-some #'get-buffer-window scratch-buffers)
    (mapc #'kill-buffer scratch-buffers)
    (remove-hook 'switch-buffer-hook #'scratch-persist-buffers-after-switch-h)))

(unless noninteractive
  (add-hook 'kill-emacs-hook #'scratch-persist-buffers-h))

(defun scratch-open-buffer (&optional arg project-p same-window-p)
  "Pop up a persistent scratch buffer.

If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
current project. When SAME-WINDOW-P is non-nil, open in the current window."
  (interactive "P")
  (funcall
   (if same-window-p #'switch-to-buffer #'pop-to-buffer)
   (scratch-buffer
    arg
    (cond
     ((eq scratch-initial-major-mode t)
      (unless (or buffer-read-only
                  (derived-mode-p 'special-mode)
                  (string-match-p "^ ?\\*" (buffer-name)))
        major-mode))
     ((symbolp scratch-initial-major-mode)
      scratch-initial-major-mode))
    default-directory
    (when-let ((project (and project-p (project-current))))
      (project-name project)))))

(defun switch-to-scratch-buffer (&optional arg project-p)
  "Like `scratch-open-buffer', but switch to it in the current window.

If passed the prefix ARG, do not restore the last scratch buffer. If PROJECT-P,
open the persistent buffer dedicated to the current project."
  (interactive "P")
  (scratch-open-buffer arg project-p 'same-window))

(defun scratch-open-project-scratch-buffer (&optional arg same-window-p)
  "Opens the (persistent) project scratch buffer in a popup.

If passed the prefix ARG, do not restore the last scratch buffer. When
SAME-WINDOW-P is non-nil, open in the same window."
  (interactive "P")
  (scratch-open-buffer arg 'project same-window-p))

(defun scratch-switch-to-project-scratch-buffer (&optional arg)
  "Like `scratch-open-project-scratch-buffer', but switch in the current window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (scratch-open-project-scratch-buffer arg 'same-window))

(defun scratch-revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*pscratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (scratch-load-persistent-scratch-buffer scratch-current-project)
    (message "Reloaded scratch buffer")))

(defun scratch-delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `scratch-dir'.

If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory scratch-dir t)
        (message "Cleared %S" (abbreviate-file-name scratch-dir)))
    (make-directory scratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " scratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))

(defun scratch-replace-with-persistent-scratch (&optional arg project-p)
  "Replace the *scratch* buffer with a persistent one."
  (interactive "P")
  (when-let ((buf (current-buffer))
             (s (get-buffer "*scratch*")))
    (kill-buffer s)
    (scratch-open-buffer arg project-p 'same-window)
    (when (buffer-live-p buf) (switch-to-buffer buf))))

(provide 'scratch)
