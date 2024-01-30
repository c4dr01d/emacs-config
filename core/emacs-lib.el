;;; emacs-lib.el --- Emacs Library (helper functions, extra features and commands) -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'emacs-vars)

(autoload 'cl-loop "cl-macs" nil nil 'macro)
(autoload 'url-filename "url-parse")
(autoload 'url-generic-parse-url "url-parse")
(autoload 'vc-git-root "vc-git")
(autoload 'vc-git-revert "vc-git")
(autoload 'tramp-make-tramp-file-name "tramp")
(defvar tramp-root-id-string)

(require 'rx)

;; Property list and attribute list functions
(defun varplist-get (vplist keyword &optional car-p)
  "Get KEYWORD's value from variable value length VPLIST.
Ex: (varplist-get \\='(:a \\='a :b \\='b1 \\='b2) :b) -> \\='(b1 b2)."
  (funcall (if car-p #'cadr #'cdr)
           (cl-loop for element in (memq keyword vplist)
                    until (and (not (eq element keyword)) (keywordp element))
                    collect element)))

(defun plist-keys (plist)
  "Return the keys of PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

(defmacro plist-push! (plist &rest key-vals)
  "Push KEY-VALS to PLIST."
  (declare (indent 1))
  (let ((out (list 'progn)))
    (while (length> key-vals 0)
      (let ((key (pop key-vals))
            (val (pop key-vals)))
        (setq out (append out `((setq ,plist (plist-put ,plist ,key ,val)))))))
    out))

(defun plist-combine (&rest plists)
  "Create a single property list from all plists in PLISTS.
Modified from `org-combine-plists'. This supposes the values to be vectors,
and concatenate them."
  (let ((res (copy-sequence (pop plists)))
        prop val plist)
    (while plists
      (setq plist (pop plists))
      (while plist
        (setq prop (pop plist) val (pop plist))
        (setq res (plist-put res prop (vconcat val (plist-get res prop))))))
    res))

(defun plist-delete (plist prop)
  "Delete property PROP from PLIST.
Adapted from `org-plist-delete'."
  (let (p)
    (while plist
      (if (not (eq prop (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun plist-to-alist (plist &optional trim-col)
  "Convert PLIST to an alist, trim first colon when TRIM-COL."
  (let (res)
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (key (if (and trim-col (string-prefix-p ":" (symbol-name key)))
                      (intern (substring (symbol-name key) 1))
                    key)))
        (push (cons key val) res)))
    (nreverse res)))

(defun alist-to-plist (alist &optional add-col)
  "Convert ALIST to a plist, add colon to the keys when ADD-COL."
  (let (res)
    (dolist (x alist)
      (push (if add-col (intern (format ":%s" (car x))) (car x)) res)
      (push (cdr x) res))
    (nreverse res)))

(defun alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets the cdr of the
first matching association in the list. It does not create duplicate
associations. By default, key comparison is done with `equal'. However, if
SYMBOL is non-nil, then `eq' is used instead.

The method may mutate the original alist, but you still need to use the return
value of this methid instead of the original alist, to ensure correct results."
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

;; Primitive utilities
(defun set-standard-value (variable value)
  "Set the standard value of VARIABLE to VALUE."
  (put variable 'standard-value `((funcall (function ,(lambda nil "" value))))))

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

;; Core functions and macros
(defmacro error! (msg &rest vars)
  "Log error MSG and VARS using `message'."
  (when (>= emacs-msg-level 1)
    `(apply #'message (list (concat "[Error] " ,msg) ,@vars))))

(defmacro info! (msg &rest vars)
  "Log info MSG and VARS using `message'."
  (when (>= emacs-msg-level 2)
    `(let ((inhibit-message t))
      (apply #'message (list (concat "[Info] " ,msg) ,@vars)))))

(defmacro log! (msg &rest vars)
  "Log MSG and VARS using `message' when `emacs-verbose-p' is non-nil."
  (when (>= emacs-msg-level 3)
    `(let ((inhibit-message t))
      (apply #'message (list (concat "[Log] ", ,msg) ,@vars)))))

(defun emacs-feature-p (&rest feats)
  "Is features FEATS are enabled in this Emacs build."
  (and (cl-every (lambda (feat)
                   (memq feat emacs-features)) feats) t))

(defmacro fn-inhibit-messages! (fn &optional no-message-log)
  "Add an advice around the function FN to suppress messages in echo area.
If NO-MESSAGE-LOG is non-nil, do not print any message to *Messages* buffer."
  (let ((advice-fn (make-symbol (format "%s--inhibit-messages:around-a" fn))))
    `(advice-add ',fn :around (defun ,advice-fn (origfn &rest args)
                               (let ((message-log-max (unless ,no-message-log
                                                       message-log-max)))
                                (with-temp-message (or (current-message) "")
                                 (log! "Inhibiting messages for %s" ,(symbol-name fn))
                                 (apply origfn args)))))))

(defmacro shutup! (&rest body)
  "Suppress new messages temporarily while evaluating BODY.
This inhibits both the echo area and the *Messages* buffer."
  (if (not emacs-verbose-p)
      `(let ((message-log-max nil))
        (with-temp-message (or (current-message) "") ,@body))
    `(progn ,@body)))

(defmacro commandify! (&rest body)
  "Convert BODY to an interactive command."
  `(lambda ()
     (interactive)
     ,@body))

(defun load-theme-wrapper ()
  "Load Emacs theme from `emacs-theme'."
  (interactive)
  (when emacs-theme
    (log! "Loading user theme: %s" emacs-theme)
    (unless (ignore-errors (load-theme emacs-theme t))
      (let ((default-theme (standard-value 'emacs-theme)))
        (error! "Cannot load theme %S, trying to load the default theme %S" emacs-theme default-theme)
        (unless (ignore-errors (load-theme default-theme t))
          (error! "Cannot load default theme %S, falling back to the builtin theme" default-theme)
          ;; (load-theme t)
          ))))
  (run-hooks 'emacs-after-load-theme-hook))

(defvar eval-when-idle--task-num 0)
(defcustom eval-when-idle-delay 5.0
  "The default delay (in seconds) to consider in `eval-when-idle!' macro."
  :group 'config-core
  :type 'float)

(defcustom lazy-delay 1.0
  "The default delay (in seconds) to consider in `lazy!' macro."
  :group 'config-core
  :type 'float)

(defun eval-when-idle (delay &rest fns)
  "Queue FNS to be processed when Emacs becomes idle after DELAY seconds."
  (let* ((task-num (cl-incf eval-when-idle--task-num))
         (task-name (make-symbol (format "eval-when-idle--task-%d" task-num))))
    (with-memoization (get task-name 'timer)
      (run-with-idle-timer delay t (lambda ()
                                     (when-let (fn (pop fns))
                                       (log! "Running task %d, calling function `%s'" task-num (truncate-string-to-width (format "%s" fn) 40 nil nil "..."))
                                       (funcall fn))
                                     (unless fns
                                       (cancel-timer (get task-name 'timer))
                                       (put task-name 'timer nil)))))))

(defmacro eval-when-idle! (&rest body)
  "Evaluate BODY when Emacs becomes idle."
  (declare (indent 0))
  `(eval-when-idle ,eval-when-idle-delay (lambda ()
                                           ,@body)))

(defmacro eval-when-idle-for! (delay &rest body)
  "Evaluate BODY after DELAY seconds from Emacs becoming idle."
  (declare (indent 1))
  `(eval-when-idle ,delay (lambda ()
                            ,@body)))

(defmacro deferred! (&rest body)
  "Run BODY after Emacs gets loaded, a.k.a. after `emacs-loaded'."
  `(with-eval-after-load 'emacs-loaded ,@body))

(defmacro deferred-when! (condition &rest body)
  "Like `deferred!', with BODY executed only if CONDITION is non-nil."
  (declare (indent 1))
  `(when ,condition (deferred! ,@body)))

(defmacro deferred-unless! (condition &rest body)
  "Like `deferred!', with BODY executed only if CONDITION is nil."
  (declare (indent 1))
  `(unless ,condition (deferred! ,@body)))

(defmacro lazy! (&rest body)
  "Run BODY as a lazy block (see `emacs-lazy')."
  `(with-eval-after-load 'emacs-lazy
    (eval-when-idle-for! lazy-delay ,@body)))

(defmacro lazy-when! (condition &rest body)
  "Like `lazy!', with BODY executed only if CONDITION is nil."
  (declare (indent 1))
  `(unless ,condition (lazy! ,@body)))

(defmacro after-load! (features &rest body)
  "Execute BODY after FEATURES have been loaded."
  (declare (indent 1))
  (let ((features (if (quoted-p features) (unquote features) (eval features))))
    (if (symbolp features)
        `(with-eval-after-load ',features ,@body)
      (let ((feature (car features)))
        (cond ((memq feature '(:or :any))
               (macroexp-progn (cl-loop for next in (cdr features)
                                        collect `(with-eval-after-load ',(unquote next) ,@body))))
              ((memq feature '(:and :all))
               (dolist (next (reverse (cdr features)) (car body))
                 (setq body `((with-eval-after-load ',(unquote next) ,@body)))))
              (t `(after-load! '(:all ,@features) ,@body)))))))

(defmacro hook-with-delay! (hook secs function &optional depth local)
  "Add the FUNCTION to the value of HOOK.
The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.
DEPTH and LOCAL are passed as is to `add-hook'."
  (let* ((f-name (make-symbol (format "%s-on-%s-delayed-%.2fs-h" (unquote function) (unquote hook) secs)))
         (f-doc (format "Call `%s' in %d seconds" (symbol-name (unquote function)) secs)))
    `(eval-when-compile
       (defun ,f-name () ,f-doc
        (run-with-idle-timer ,secs nil ,function))
       (add-hook ,hook #'f-name ,depth ,local))))

(defvar hook-once-num 0)

(defmacro hook-once! (hook &rest body)
  "Hook BODY in HOOK, and make it run only once."
  (declare (indent 1))
  (let ((hook (unquote hook))
        (fn-name (intern (format "hook-once--function-%d-h" (cl-incf hook-once-num)))))
    `(add-hook ',hook (defun ,fn-name (&rest _)
                       ,(macroexp-progn body)
                       (remove-hook ',hook ',fn-name)))))

(defmacro make-first-file-hook! (filetype ext-regexp)
  "Make a hook which run on the first FILETYPE file if a particular extensions.
The extension should matches EXT-REGEXP.

This will creates a function named `first-idle--FILETYPE-h' which gets executed
before `after-find-file'. This function will run on the first file that matches
EXT-REGEXP. When it runs, this function provides a feature named
`emacs-first-FILETYPE-file' and a run all hooks in
`emacs-first-FILETYPE-file-hook'."
  (let* ((filetype (unquote filetype))
         (fn-name (intern (format "first-%s-file:after-a" (if filetype (format "-%s" filetype) ""))))
         (hook-name (intern (format "emacs-first%s-file-hook" (if filetype (format "-%s" filetype) ""))))
         (feature-name (intern (format "emacs-first%s-file" (if filetype (format "-%s" filetype) ""))))
         (hook-docs (format "This hook will be run before opening the first %s file.

Applies to files that matches %S.

Executed before `find-file-noselect', it runs all hooks in `%s' and provide the `%s' feature."
                            (or filetype "") (eval ext-regexp) hook-name feature-name)))
    `(progn
       (log! "Setting up hook `%s' -- function `%s' -- feature `%s'." ',hook-name ',fn-name ',feature-name)
       (defcustom ,hook-name nil ,hook-docs :group 'config-core :type 'hook)
       (defun ,fn-name (&optional filename &rest _)
        (when (and after-init-time (or
                                    (featurep 'emacs-loaded)
                                    (when-let ((files (cdr command-line-args)))
                                     (cl-some (lambda (file)
                                                (string= (expand-file-name filename) (expand-file-name file))) files)))
               (let ((case-fold-search t))
                (string-match-p ,ext-regexp filename)))
         (log! "Running %d `%s' hooks." (length ,hook-name) ',hook-name)
         (advice-remove 'find-file-noselect #',fn-name)
         (provide ',feature-name)
         (run-hooks ',hook-name)))
       (if (daemonp)
           (add-hook 'after-init-hook (lambda ()
                                        (provide ',feature-name)
                                        (run-hooks ',hook-name)) 90)
         (advice-add 'find-file-noselect :before #',fn-name '((depth . ,(if filetype -100 -101))))))))

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
           append (cl-loop for (var . val) in vars
                           collect (list var val hook
                                         (intern (format "setq--%s-in-%s-h"
                                                         var mode))))))

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
        (:depth (setq depth (pop rest)))
        (:local (setq local-p t))
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
the function.

  (setq-hook! \\='enable-theme-functions current-theme (car args))

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn (cl-loop for (var val hook fn)
                           in (setq-hook-fns hooks var-vals)
                           collect `(defun ,fn (&rest args)
                                     ,(format "%s = %s" var (pp-to-string val))
                                     (setq-local ,var ,val))
                           collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest VAR1 VAR2...)"
  (declare (indent 1))
  (macroexp-progn (cl-loop for (_var _val hook fn)
                           in (setq-hook-fns hooks vars 'singles)
                           collect `(remove-hook ',hook #',fn))))

(defun compile-functions (&rest fns)
  "Queue FNS to be byte/natively-compiled after a brief delay."
  (dolist (fn fns)
    (eval-when-idle! (or (and (featurep 'native-compile)
                              (or (subr-native-elisp-p (indirect-function fn))
                                  (cl-letf (((symbol-function 'comp-log-to-buffer) #'ignore))
                                    (shutup! (ignore-errors (native-compile fn))))))
                         (byte-code-function-p fn)
                         (let (byte-compile-warnings)
                           (shutup! (byte-compile fn)))))))

;; Shell
(defvar shell-command-switch-wrapper
  (pcase shell-file-name
    ((rx bol "fish" eol) "-lc")
    ((rx bol (or "tcsh" "csh") eol) "-dc")
    (_ "-ilc")))

(defun shell-command-to-string-ignore-stderr (command)
  "Execute shell command COMMAND and return its output as a string.

Works like `shell-command-to-string' with two differences:
1. It uses `shell-command-switch' instead of `shell-command-switch'.
2. It returns only stdout and ignore the output of stderr."
  (with-output-to-string
    (with-current-buffer standard-output
      (let ((process-environment (append (list "TERM=smart") process-environment)))
        (process-file shell-file-name nil '(t nil) nil shell-command-switch command)))))

(defun env-save ()
  "Load environment variables from shell and save them to `env-file'."
  (interactive)
  (unless os-windows-p
    (with-temp-buffer
      (insert ";; -*- mode: emacs-lisp; no-byte-compile: t; no-native-compile: t; -*-\n\n")
      (let ((env-vars
             (mapcar
              (lambda (line)
                (let ((var-val (string-split line "=")))
                  (cons (car var-val) (string-join (cdr var-val) "="))))
              (string-split (shell-command-to-string-ignore-stderr "env --null") "\0"))))
        (when-let ((path (alist-get "PATH" env-vars nil nil #'string=)))
          (insert "\n;; Adding PATH content to `exec-path'\n"
                  (format "(setq exec-path (delete-dups (append exec-path '%s)))\n\n"
                          (mapcar (apply-partially #'format "\"%s\"")
                                  (parse-colon-path path)))))
        (insert ";; Adding the rest of the environment variables\n")
        (dolist (env-var env-vars)
          (unless (cl-some (apply-partially-right #'string-match-p (car env-var)) env-deny-vars)
            (let ((value (cdr env-var)))
              (dolist (pair '(("\a" . "\\a") ("\b" . "\\b") ("\f" . "\\f")
                              ("\n" . "\\n") ("\r" . "\\r") ("\t" . "\\t")
                              ("\v" . "\\v") ("\"" . "\\\"")))
                (set value (string-replace (car pair) (cdr pair) value)))
              (insert (format "(setenv \"%s\" \"%s\")\n" (car env-var) value))))))
      (write-file env-file))))

(defun env-load ()
  "Load environment variables from `env-file'."
  (interactive)
  (unless os-windows-p
    (unless (file-exists-p env-file)
      (env-save))
    (load-wrapper env-file)))

(defun ignore-root (&rest roots)
  "Add ROOTS to ignored projects, recentf, etc."
  (dolist (root roots)
    (with-eval-after-load 'recentf
      (add-to-list 'recentf-exclude (rx-to-string `(or ,root ,(expand-file-name root)))))))

(defun package-disabled-p (package)
  "Is package PACKAGE disabled in `emacs-disabled-packages'."
  (and (memq package (apply #'append (mapcar #'ensure-list emacs-disabled-packages))) t))

(defvar package--download-urls nil)

(defun package-download-from-urls (pkgname &rest args)
  "Download PKGNAME files from URLs in ARGS.

Pass `:redownload' to force redownloading the package files.
Returns the load path of the package, useful for usage with `use-package''s
`:load-path'."
  (let* ((pkg-load-path (directory-ensure emacs-extra-packages-dir (format "%s/" pkgname)))
         (default-directory pkg-load-path)
         (redownload-p (memq :redownload args))
         (urls (remq :redownload args)))
    (add-to-list 'package--download-urls (cons pkgname urls))
    (dolist (url urls)
      (when-let* ((url-file-name (url-filename (url-generic-parse-url url)))
                  (url-file-name (file-name-nondirectory url-file-name))
                  (url-file-name (car (string-split url-file-name "?"))))
        (when (and redownload-p (file-exists-p url-file-name))
          (delete-file url-file-name))
        (unless (file-exists-p url-file-name)
          (url-copy-file url url-file-name))))
    pkg-load-path))

(defun emacs-run-build-functions (&optional dont-ask-p)
  "Run all build functions in `emacs-build-functions'.

Call functions without asking when DONT-ASK-P is non-nil."
  (interactive "P")
  (dolist (fn emacs-build-functions)
    (message "Emacs: Running `%s'" fn)
    (if dont-ask-p
        (cl-letf (((symbol-function 'yes-or-no-p) #'always)
                  ((symbol-function 'y-or-n-p) #'always))
          (funcall-interactively fn))
      (funcall-interactively fn))))

(defun emacs-update-packages ()
  "Update third party packages."
  (interactive)
  (message "[Emacs]: Creating backups for the current versions of packages")
  (let* ((backup-dir (concat emacs-local-dir "emacs/versions/"))
         (dest-file (concat backup-dir (format-time-string "default-%Y%m%d%H%M%S.el")))
         (src-file (concat straight-base-dir "straight/versions/default.el")))
    (unless (file-directory-p backup-dir) (mkdir backup-dir 'parents))
    (when (file-exists-p src-file)
      (message "[Emacs]: Creating backup from \"%s\" to \"%s\" src-file dest-file")
      (copy-file src-file dest-file)))

  (straight-pull-recipe-repositories)

  (message "[Emacs]: Pulling packages")
  (straight-pull-all)
  (message "[Emacs]: Freezing packages")
  (straight-freeze-versions)
  (message "[Emacs]: Rebuilding packages")
  (straight-rebuild-all)

  (message "[Emacs]: Updating packages installed from URLS")
  (mapc (lambda (args)
          (apply #'package-download-from-urls (append args '(:redownload)))) package--download-urls)

  (message "[Emacs]: Running additional package-specific build functions")
  (emacs-run-build-functions 'dont-ask))

(defun emacs-update-restore-locked (restore-from-backup)
  "Restore lockfile packages list. Takes into account the pinned ones.
When called with \\[universal-argument] or with RESTORE-FROM-BACKUP, it will
restore the lockfile from backups, not Git."
  (interactive "P")
  (let* ((lockfile (concat straight-base-dir "straight/versions/default.el"))
         (default-directory (vc-git-root lockfile))
         (backup-dir (concat emacs-local-dir "emacs/versions/")))
    (straight-pull-recipe-repositories)
    (if (not restore-from-backup)
        (progn
          (message "[Emacs] Reverting file \"%s\" to the original" lockfile)
          (unless (zerop (vc-git-revert lockfile))
            (user-error "[Emacs] An error occurred when trying to revert \"%s\"" lockfile)))
      (message "[Emacs] Trying to restore the lockfile from backups.")
      (if-let* ((_ (file-exists-p backup-dir))
                (backups (directory-files backup-dir nil "[^.][^.]?\\'"))
                (restore-backup-file (completing-read "Select which backup to restore: " backups))
                (last-backup (expand-file-name restore-backup-file backup-dir)))
          (if (not (file-exists-p last-backup))
              (user-error "[Emacs] No backup file")
            (copy-file last-backup lockfile 'overwrite-existing)
            (message "[Emacs] Restored the last backup from \"%s\"" restore-backup-file))))
    (straight-freeze-versions)
    (when (file-exists-p (concat straight-base-dir "versions/pinned.el"))
      (message "[Emacs] Restoring pinned versions of packages")
      (straight-thaw-versions))
    (message "[Emacs] Restoring packages from the global lockfile versions")
    (straight-thaw-versions)
    (message "[Emacs] Rebuilding packages")
    (straight-rebuild-all)
    (message "[Emacs] Running additional package-specific build functions")
    (emacs-run-build-functions 'dont-ask)))

(defun emacs-root-dir-cleanup ()
  "Cleanup Emacs root directory."
  (let ((default-directory emacs-root-dir))
    (mapc (apply-partially-right #'delete-file-or-directory 'trash 'recursive)
          (directory-files emacs-root-dir nil (rx (seq bol (or "eln-cache" "auto-save-list" "elpa") eol))))))

;; Files, directories and IO helper functions
(defun file-mime-type (file)
  "Get MIME type for FILE based on magic codes provided by the \"file\" command.
Return a symbol of the MIME type, ex: `text/x-lisp', `text/plain',
`application/x-object', `application/octet-stream', etc."
  (if-let ((file-cmd (executable-find "file"))
           (mime-type (shell-command-to-string (format "%s --brief --mime-type %s" file-cmd file))))
      (intern (string-trim-right mime-type))
    (error "The \"file\" command isn't installed.")))

(defun file-name-incremental (filename)
  "Return a unique file name for FILENAME.
If \"file.ext\" exists, returns \"file-0.ext\"."
  (let* ((ext (file-name-extension filename))
         (dir (file-name-directory filename))
         (filename-regex (concat "^" file "\\(?:-\\(?1:[[:digit:]]+\\)\\)?" (if ext (concat "\\." ext) "")))
         (last-file (car (last (directory-files dir nil filename-regex))))
         (last-file-num (and last-file (string-match filename-regex last-file) (match-string 1 last-file)))
         (num (1+ (string-to-number (or last-file-num "-1")))))
    (file-name-concat dir (format "%s%s%s" file (if last-file (format "-%d" num) "") (if ext (concat "." ext) "")))))

(defun file-read-to-string (filename)
  "Return a string with the contents of FILENAME."
  (when (and (file-exists-p filename) (not (file-directory-p filename)))
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

(defun directory-subdirs (dir)
  "Return a list of sub-directories in DIR."
  (when dir
    (seq-filter #'file-directory-p
                (mapcar #'abbreviate-file-name (directory-files dir t "[^.][^.]?\\'")))))

(defun directory-ensure (&rest path-parts)
  "Concatenate PATH-PARTS to construct a path and return it.

Ensure the path exists. if not create it. The exact hebavior is to create the
parent directory if the path is a file, and if the path is a directory, create
that directory."
  (let* ((path (mapconcat #'identity path-parts nil))
         (parent-dir (file-name-directory path)))
    (unless (file-directory-p parent-dir)
      (ignore-errors (mkdir parent-dir t))
      (unless (file-directory-p parent-dir)
        (error! "Cannot create directory %s" parent-dir)))
    path))

(defun delete-this-file (&optional path force-p)
  "Delete PATH.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation."
  (interactive (list (buffer-file-name (buffer-base-buffer))
                     current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (abbreviate-file-name path)))
    (unless (and path (file-exists-p path))
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (unwind-protect
        (progn (delete-file path delete-by-moving-to-trash) t)
      (while (file-exists-p path)
        (error "Failed to delete %S" short-path)))))

(defun delete-this-file-and-buffer (&optional filename)
  "Delete FILENAME and its associated visiting buffer."
  (interactive)
  (when-let ((filename (or filename (buffer-file-name)))
             (short-path (abbreviate-file-name filename)))
    (if (vc-backend filename)
        (or (ignore-errors (vc-delete-file (buffer-file-name)))
            (delete-this-file filename)
            (kill-buffer))
      (when (y-or-n-p (format "Are you sure you want to delete %s? " short-path))
        (delete-file filename delete-by-moving-to-trash)
        (message "Deleted file %s" short-path)
        (kill-buffer)))))

(defun delete-file-or-directory (file-or-directory &optional trash recursive)
  "Delete FILE-OR-DIRECTORY with `delete-file' or `delete-directory'.

Move to trash when TRASH is non-nil, delete directories recursively when
RECURSIVE is non-nil."
  (if (file-directory-p file-or-directory)
      (delete-directory file-or-directory recursive trash)
    (delete-file file-or-directory trash)))

(if (fboundp 'rename-visited-file)
    (defalias 'move-this-file #'rename-visited-file)
  (defun move-this-file (new-path &optional force-p)
    "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
    (interactive (list (read-file-name "Move file to: ") current-prefix-arg))
    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting ant file"))
    (let ((old-path (buffer-file-name (buffer-base-buffer)))
          (new-path (expand-file-name new-path)))
      (when (directory-name-p new-path)
        (setq new-path (expand-file-name new-path)))
      (make-directory (file-name-directory new-path) t)
      (rename-file old-path new-path (or force-p 1))
      (set-visited-file-name new-path t t)
      (message "File moved to %S" (abbreviate-file-name new-path)))))

(defun tramp-sudo-file-path (file)
  "Construct a Tramp sudo path to FILE. Works for both local and remote files."
  (tramp-make-tramp-file-name "sudo" tramp-root-id-string nil (or (file-remote-p file 'host) "localhost") nil file))

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (tramp-sudo-file-path file)))

(defun sudo-this-file (file)
  "Open current FILE as root."
  (interactive)
  (if-let ((this-file (or buffer-file-name
                          (when (derived-mode-p 'dired-mode 'wdired-mode)
                            default-directory))))
      (find-file (tramp-sudo-file-path this-file))
    (user-error "Current buffer not found to a file")))

(defun sudo-save-buffer ()
  "Save this buffer as root. Save as new file name if called with prefix."
  (interactive)
  (if-let ((file (or (and (or (not buffer-file-name) current-prefix-arg)
                          (read-file-name "Save as root to: "))
                     buffer-file-name))
           (file (tramp-sudo-file-path (expand-file-name file)))
           (dest-buffer (find-file-noselect file))
           (src-buffer (current-buffer)))
      (progn
        (copy-to-buffer dest-buffer (point-min) (point-max))
        (unwind-protect (with-current-buffer dest-buffer (save-buffer))
          (unless (eq src-buffer dest-buffer) (kill-buffer dest-buffer))
          (with-current-buffer src-buffer (revert-buffer t t))))
    (user-error "Unable to open %S" (abbreviate-file-name file))))

(defun yank-this-file-name ()
  "Yank the file name of this buffer."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (with-temp-buffer
        (insert file)
        (kill-ring-save (point-min) (point-max)))
    (user-error "This buffer isn't bound to a file")))

(defun clean-file-name (filename &optional downcase-p)
  "Clean FILENAME, optionally convert to DOWNCASE-P."
  (replace-regexp-in-string
   "[:;\t\n\r /\\_]+"
   "-"
   (replace-regexp-in-string
    "[‘’‚“”„\"`'()&]+"
    ""
    (if downcase-p (downcase filename) filename))))

;; Lock files
(defun lock--file (name)
  "Get the absolute path of the lockfile for resource NAME."
  (expand-file-name (format "emacs-%s.lock" name) temporary-file-directory))

(defun lock--locker-pid (name)
  "Get thecker PID of resource NAME."
  (let ((fname (lock--file name)))
    (and (file-exists-p fname) (string-to-number (file-read-to-string fname)))))

(defun lockedp (name)
  "Return non-nil if the resource NAME is locked."
  (when-let ((pid (lock--locker-pid name)))
    (and (process-attributes pid) t)))

(defun locked-by-this-process-p (name)
  "Return non-nil if the resource NAME locked by this Emacs instance."
  (and (lockedp name) (equal (emacs-pid) (lock--locker-pid name))))

(defun lock (name)
  "Lock the resource named NAME."
  (if (lockedp name)
      (progn (info! "Resource `%s' already locked!" name) nil)
    (info! "Created lock file for resource `%s'!" name)
    (shutup! (with-temp-buffer
               (insert (format "%d" (emacs-pid)))
               (write-file (lock--file name))))
    t))

(defun unlock (name &optional force-p)
  "Unlock the resource named NAME if locked by this process.
If FORCE-P is non-nil, force unlocking even if the resource is not locked by the
current process."
  (when (or force-p (locked-by-this-process-p name))
    (info! "Resource `%s' unlocked" name)
    (delete-file (lock--file name))
    t))
(provide 'emacs-lib)
;;; emacs-lib.el ends here.
