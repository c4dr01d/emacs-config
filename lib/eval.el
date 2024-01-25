;; -*- lexical-binding: t -*-
(defmacro after! (features &rest body)
  "Execute BODY after FEATURES have been loaded."
  (declare (indent 1))
  (let ((features (if (quoted-p features) (unquote features) (eval features))))
    (if (symbolp features)
	`(with-eval-after-load ',features ,@body)
      (let ((feature (car features)))
	(cond
	 ((memq feature '(:or :any))
	  (macroexp-progn
	   (cl-loop for next in (cdr features)
		    collect `(with-eval-after-load ',(unquote next) ,@body))))
	 ((memq feature '(:and :all))
	  (dolist (next (reverse (cdr features)) (car body))
	    (setq body `((with-eval-after-load ',(unquote next) ,@body)))))
	 (t `(after! '(:all ,@features) ,@body)))))))
(provide 'eval)
