;; -*- lexical-binding: t -*-
(defmacro inhibit-messages! (fn &optional nomessage)
  "Add an advice around the function FN to suppress messages in echo area.
If NOMESSAGE is non-nil, do not print any message to `*Messages*' buffer."
  (let ((advice-fn (make-symbol (format "%s--inhibit-messages:around-a" fn))))
    `(advice-add ',fn :around (defun ,advice-fn (orig &rest args)
			       (let ((message-log-max (unless ,nomessage message-log-max)))
				(with-temp-message (or (current-message) "")
				 (apply orig args)))))))

(defmacro shutup! (&rest body)
  "Suppress new messages temporarily while evaluating BODY.
This inhibits both the echo area and the `*Messages*' buffer."
  (if (not verbose-p)
      `(let ((message-log-max nil))
        (with-temp-message (or (current-message) "") ,@body))
    `(progn ,@body)))

(defmacro commandify! (&rest body)
  "Convert BODY to an interactive command."
  `(lambda ()
     (interactive)
     ,@body))

(provide 'command)
