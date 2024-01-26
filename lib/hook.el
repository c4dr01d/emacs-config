;; -*- lexical-binding: t -*-
(defmacro hook-with-delay! (hook secs function &optional depth local)
  "Add the FUNCTION to the value of HOOK.
The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.
DEPTH and LOCAL are passed as is to `add-hook'."
  (let* ((f-name (make-symbol (format "%s-on-%s-delayed-%.2fs-h" (unquote function) (unquote hook) secs)))
	 (f-doc (format "Call `%s' in %d seconds" (symbol-name (unquote function)) secs)))
    `(eval-when-compile
       (defun ,f-name ()
	,f-doc
	(run-with-idle-timer ,secs nil ,function))
       (add-hook ,hook #',f-fname ,depth ,local))))

(defvar hook-once-num 0)

(defmacro hook-once! (hook &rest body)
  "Hook BODY in HOOK, and make it run only once."
  (declare (indent 1))
  (let ((hook (unquote hook))
	(fn-name (intern (format "hook-once--function-%d-h" (cl-incf hook-once-num)))))
    `(add-hook ',hook (defun ,fn-name (&rest _)
		       ,(macroexp-progn body)
		       (remove-hook ',hook ',fn-name)))))

(defun resolve-hook-forms (hooks)
  "Convert a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
	hook-list
      (cl-loop for hook in hook-list
	       if (eq (car-safe hook) 'quote)
	       collect (cadr hook)
	       else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
			     vars)
			 (while args
			   (push (if singles
				     (list (pop args))
				   (cons (pop args) (pop args)))
				 vars))
			 (nreverse vars))
	   for hook in (resolve-hook-forms hooks)
	   for mode = (string-remove-suffix "-hook" (symbol-name hook))
	   append
	   (cl-loop for (var . val) in vars
		    collect
		    (list var val hook
			  (intern (format "setq-%s-for-%s-h" var mode))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

If the hook function should receive an argument (like in
`enable-theme-functions'), the `args' variable can be expanded in the forms

  (+add-hook! \\='enable-theme-functions
    (message \"Enabled theme: %s\" (car args)))

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest args) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook (nreverse ',hook-forms))
        (dolist (func (list ,@func-forms))
         ,(if remove-p
              `(remove-hook hook func ,local-p)
            `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same arguments as `add-hook!'.

If N = 1 and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Set buffer-local variables on HOOKS.

HOOKS can be expect receiving arguments (like in `enable-theme-functions'), the
`args' variable can be used inside VAR-VALS forms to get the arguments passed
the the function.

  (+setq-hook! \\='enable-theme-functions
    current-theme (car args))

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (setq-hook-fns hooks var-vals)
	    collect `(defun ,fn (&rest args)
		      ,(format "%s = %s" var (pp-to-string val))
		      (setq-local ,var ,val))
	    collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest VAR1 VAR2...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn)
	    in (setq-hook-fns hooks vars 'singles)
	    collect `(remove-hook ',hook #',fn))))
(provide 'hook)
