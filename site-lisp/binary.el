;; -*- lexical-binding: t -*-
(defgroup binary nil
  "Binary files.")

(defcustom objdump-executable "objdump"
  "The \"objdump\" command."
  :group 'binary
  :type '(choice file string))

(defcustom objdump-enable t
  "Enable or disable disassembling suitable files with objdump."
  :group 'binary
  :type 'boolean)

(defcustom hexl-enable t
  "Enable or disable opening suitable files in `hexl-mode'."
  :group 'binary
  :type 'boolean)

(defun objdump-p (filename)
  "Can FILENAME be recognized by \"objdump\"."
  (when-let* ((file (and filename (file-truename filename))))
    (and objdump-executable
	 (executable-find objdump-executable)
	 (not (file-remote-p file))
	 (file-exists-p file)
	 (not (file-directory-p file))
	 (not (zerop (file-attribute-size (file-attributes file))))
	 (not (string-match-p "file format not recognized"
			      (shell-command-to-string
			       (format "%s --file-headers %s"
				       objdump-executable
				       (shell-quote-argument file))))))))

(defun objdump-buffer-p (&optional buffer)
  "Can the BUFFER be viewed as disassembled code with objdump."
  (and objdump-enable (objdump-p (buffer-file-name buffer))))

(defun binary-buffer-p (&optional buffer)
  "Return whether BUFFER or the current buffer is binary.

A binary buffer is defined as containing at least one null byte.

Returns either nil, or the position of the first null byte."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (search-forward (string ?\x00) nil t 1))))

(defun hexl-buffer-p (&optional buffer)
  "Does BUFFER (defaults to the current buffer) shoule be viewed using `hexl-mode'."
  (and hexl-enable
       (binary-buffer-p buffer)
       (not (objdump-buffer-p buffer))))

(define-derived-mode objdump-disassemble-mode
  special-mode "Objdump Mode"
  "Major mode for viewing executable files disassembled using objdump."
  (if-let* ((file (buffer-file-name))
	    (objdump-file (objdump-p file)))
      (let ((buffer-read-only nil))
	(message "Disassembling %S using objdump." (file-name-nondirectory file))
	(erase-buffer)
	(set-visited-file-name (file-name-with-extension file "_dias.objdump"))
	(call-process "objdump" nil (current-buffer) nil "-d" file)
	(goto-char (point-min))
	(buffer-disable-undo)
	(set-buffer-modified-p nil)
	(view-mode 1)
	(read-only-mode 1)
	(require 'asm-mode)
	(set-syntax-table (make-syntax-table asm-mode-syntax-table))
	(modify-syntax-entry ?# "< b")
	(setq-local font-lock-defaults '(asm-font-lock-keywords)))
    (user-error "Objdump can not be used with this buffer")))

(defun hexl-mode-maybe ()
  "Activate `hexl-mode' if relevant for the current buffer."
  (interactive)
  (when (and (not (eq major-mode 'hexl-mode)) (hexl-buffer-p))
    (hexl-mode 1)))

(defun binary-setup-modes ()
  "Setup binary modes (objdump and hexl) for relevant buffers and file types."
  (add-to-list 'magic-fallback-mode-alist '(objdump-buffer-p . objdump-disassemble-mode) t)
  (add-to-list 'magic-fallback-mode-alist '(hexl-buffer-p . hexl-mode-maybe) t))

(provide 'binary)
