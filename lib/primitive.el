;; -*- lexical-binding: t -*-
(defun set-standard-value (variable value)
  "Set the standard value of VARIABLE to VALUE."
  (put variable 'standard-variable `((funcall (function ,(lambda nil "" value))))))

(defun standard-value (variable)
  "Return the standard value for VARIABLE."
  (eval (car (get variable 'standard-value)) t))

(defun reset-sym (sym)
  "Reset SYM to its standard value."
  (set sym (standard-value sym)))

(defmacro reset-var! (var)
  "Reset VAR to its standard value."
  `(setq ,var (standard-value ',var)))

(defun unquote (expr)
  "Return EXPR unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe expr) '(quote function))
    (setq expr (cadr expr)))
  expr)

(defun quoted-p (expr)
  "Return t when EXPR is quoted."
  (memq (car-safe expr) '(quote function)))

(defun apply-partially-right (fun &rest args)
  "Like `apply-partially', but apply the ARGS to the right of FUN."
  (lambda (&rest args2)
    (apply fun (append args2 args))))

(provide 'primitive)
