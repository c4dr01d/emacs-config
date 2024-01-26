;; -*- lexical-binding: t -*-
(use-package project
  :demand t
  :hook (kill-emacs . project-forget-zombie-projects)
  :custom
  (project-list-file (concat user-emacs-directory "project-list.el"))
  (project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project")))

(use-package xref
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package elisp-mode
  :init
  (map-local! :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map ielm-map lisp-mode-map scheme-mode-map)
    "p" #'check-parens)
  (setq-hook! emacs-lisp-mode tab-width 8)
  :config
  (map-local! :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "d" '(nil :wk "edebug")
    "df" #'edebug-defun
    "dF" #'edebug-all-forms
    "dd" #'edebug-all-defs
    "dr" #'edebug-remove-instrumentation
    "do" #'edebug-on-entry
    "dO" #'edebug-cancel-on-entry
    "db" '(nil :wk "breakpoints")
    "dbb" #'edebug-set-breakpoint
    "dbr" #'edebug-unset-breakpoint
    "dbn" #'edebug-next-breakpoint
    "e" '(nil :wk "eval")
    "eb" #'eval-buffer
    "ed" #'eval-defun
    "ee" #'eval-last-sexp
    "er" #'eval-region
    "eR" #'elisp-eval-region-or-buffer
    "el" #'load-library
    "g" '(nil :wk "goto/find")
    "gf" #'find-function-at-point
    "gR" #'find-function
    "gv" #'find-variable-at-point
    "gV" #'find-variable
    "gL" #'find-library
    "c" '(nil :wk "compile")
    "cc" #'elisp-byte-compile-buffer
    "cf" #'elisp-byte-compile-file
    "cn" #'emacs-lisp-native-compile-and-load
    "cb" #'emacs-lisp-byte-compile-and-load)
  (map-local! :keymaps '(edebug-mode-map)
    "e" '(nil :wk "eval")
    "ee" #'edebug-eval-last-sexp
    "eE" #'edebug-eval-expression
    "et" #'edebug-eval-top-level-form)

  (defvar emacs-lisp--face nil)
  (defvar calculate-lisp-indent-check-for-keyword nil)
  (autoload #'ad-get-orig-definition "advice")

  (defun emacs-lisp--highlight-vars-and-faces (end)
    "Match defined variables and functions.
Functions are differentiated into \"special forms\", \"built-in functions\" and
\"library/userland functions\"."
    (catch 'matcher
      (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
        (let ((ppss (save-excursion (syntax-ppss))))
          (cond ((nth 3 ppss)
                 (search-forward "\"" end t))
                ((nth 4 ppss)
                 (forward-line +1))
                ((let ((symbol (intern-soft (match-string-no-properties 0))))
                   (and (cond ((null symbol) nil)
                              ((eq symbol t) nil)
                              ((keywordp symbol) nil)
                              ((special-variable-p symbol)
                               (setq emacs-lisp--face 'font-lock-variable-name-face))
                              ((and (fboundp symbol)
                                    (eq (char-before (match-beginning 0)) ?\()
                                    (not (memq (char-before (1- (match-beginning 0)))
                                               (list ?\' ?\`))))
                               (let ((unaliased (indirect-function symbol)))
                                 (unless (or (macrop unaliased)
                                             (special-form-p unaliased))
                                   (let (unadvised)
                                     (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                     (setq unaliased (indirect-function unadvised)))))
                                     unaliased)
                                   (setq emacs-lisp--face
                                         (if (subrp unaliased)
                                             'font-lock-constant-face
                                           'font-lock-function-name-face))))))
                        (throw 'matcher t)))))))
      nil))

  (defun emacs-lisp--calculate-lisp-indent:override-a (&optional parse-start)
    "Add better indentation for quoted and backquoted lists."
    (defvar calculate-lisp-indent-last-sexp)
    (save-excursion
      (beginning-of-line)
      (let ((indent-point (point))
            (desired-indent nil)
            (retry t)
            state calculate-lisp-indent-last-sexp containing-sexp)
        (cond ((or (markerp parse-start) (integerp parse-start))
               (goto-char parse-start))
              ((null parse-start) (beginning-of-defun))
              (t (setq state parse-start)))
        (unless state
          (while (< (point) indent-point)
            (setq state (parse-partial-sexp (point) indent-point 0))))
        (while (and retry state (> (elt state 0) 0))
          (setq retry nil
                containing-sexp (elt state 1)
                calculate-lisp-indent-last-sexp (elt state 2))
          (goto-char (1+ containing-sexp))
          (if (and calculate-lisp-indent-last-sexp (> calculate-lisp-indent-last-sexp (point)))
              (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
                (if (setq retry (car (cdr peek))) (setq state peek)))))
        (unless retry
          (goto-char (1+ containing-sexp))
          (if (not calculate-lisp-indent-last-sexp)
              (setq desired-indent (current-column))
            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
            (cond ((looking-at "\\s("))
                  ((> (save-excursion (forward-line 1) (point)) calculate-lisp-indent-last-sexp)
                   (if (or
                        (= (point) calculate-lisp-indent-last-sexp)
                        (and calculate-lisp-indent-check-for-keyword
                             (when-let (char-after (char-after (1+ containing-sexp)))
                               (char-equal char-after ?:)))
                        (let* ((positions (elt state 9))
                               (last (car (last positions)))
                               (rest (reverse (butlast positions)))
                               (any-quoted-p nil)
                               (point nil))
                          (or
                           (when-let (char (char-before last))
                             (or (char-equal char ?')
                                 (char-equal char ?`)))
                           (progn
                             (while (and rest (not any-quoted-p))
                               (setq point (pop rest)
                                     any-quoted-p
                                     (or
                                      (when-let (char (char-before point))
                                        (or (char-equal char ?') (char-equal char ?`)))
                                      (save-excursion
                                        (goto-char (1+ point))
                                        (looking-at-p "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                             any-quoted-p))))
                       nil
                     (progn (forward-sexp 1)
                            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)))
                   (backward-prefix-chars))
                  (t
                   (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                   (backward-prefix-chars)))))
        (let ((normal-indent (current-column)))
          (cond ((elt state 3)
                 nil)
                ((and (integerp lisp-indent-offset) containing-sexp)
                 (goto-char containing-sexp)
                 (+ (current-column) lisp-indent-offset))
                (calculate-lisp-indent-last-sexp
                 (or
                  (and lisp-indent-function
                       (not retry)
                       (funcall lisp-indent-function indent-point state))
                  (and (save-excursion
                         (goto-char indent-point)
                         (skip-chars-forward " \t")
                         (looking-at ":"))
                       (save-excursion
                         (goto-char calculate-lisp-indent-last-sexp)
                         (backward-prefix-chars)
                         (while (not (or (looking-back "^[ \t]*\\|([ \t]+" (line-beginning-position))
                                         (and containing-sexp (>= (1+ containing-sexp) (point)))))
                           (forward-sexp -1)
                           (backward-prefix-chars))
                         (setq calculate-lisp-indent-last-sexp (point)))
                       (> calculate-lisp-indent-last-sexp
                          (save-excursion
                            (goto-char (1+ containing-sexp))
                            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                            (point)))
                       (let ((parse-sexp-ignore-comments t)
                             indent)
                         (goto-char calculate-lisp-indent-last-sexp)
                         (or (and (looking-at ":")
                                  (setq indent (current-column)))
                             (and (< (line-beginning-position) (prog2 (backward-sexp) (point)))
                                  (looking-at ":")
                                  (setq indent (current-column))))
                         indent))
                  normal-indent))
                (desired-indent)
                (t
                 normal-indent))))))

  (advice-add 'calculate-lisp-indent :override #'emacs-lisp--calculate-lisp-indent:override-a)

  (font-lock-add-keywords 'emacs-lisp-mode '((emacs-lisp--highlight-vars-and-faces . emacs-lisp--face))))

(use-package scheme
  :custom
  (scheme-program-name "guile"))

(use-package gdb-mi
  :custom
  (gdb-show-main t)
  (gdb-many-windows t)
  (gdb-debug-log-max 1024)
  (gdb-restore-window-configuration-after-quit t)
  (gdb-thread-buffer-verbose-names nil)
  (gdb-window-configuration-directory (directory-ensure user-emacs-directory "gdb/"))
  (gdb-max-source-window-count 1)
  (gdb-display-io-nopopup nil))

(use-package gud
  :config
  (defvar gud-overlay
    (let* ((overlay (make-overlay (point-min) (point-min))))
      (overlay-put overlay 'face 'highlight)
      overlay)
    "Overlay variable for GUD highlighting.")

  (advice-add 'gud-display-line :after (defun gud--display-overlay:after-a (true-file _line)
					 (let* ((overlay gud-overlay)
						(buffer (gud-find-file true-file)))
					   (with-current-buffer buffer
					     (move-overlay overlay (line-beginning-position) (line-end-position) (current-buffer))))))

  (add-hook 'kill-buffer-hook (defun gud--delete-overlay-h ()
				(when (derived-mode-p 'gud-mode)
				  (delete-overlay 'gud-overlay)))))

(use-package ediff
  :hook (ediff-before-setup . ediff--save-window-config-h)
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-split-setup-function #'ediff-setup-window-plain)
  :config
  (defvar ediff--saved-window-config nil)

  (defun ediff--save-window-config-h ()
    (setq ediff--saved-window-config (current-window-configuration)))

  (add-hook! (ediff-quit ediff-suspend) :depth 101
    (defun ediff--restore-window-config-h ()
      (when (window-configuration-p ediff--saved-window-config)
	(set-window-configuration ediff--saved-window-config)))))

(use-package sermge-mode
  :commands smerge-hydra/body
  :init (map! "gm" '(smerge-hydra/body :wk "sMerge"))
  :config
  (defun smerge-first ()
    "Got to the first occurrence."
    (interactive)
    (goto-char (point-min))
    (smerge-next))

  (defun smerge-last ()
    "Got to the last occurrence."
    (interactive)
    (goto-char (point-max))
    (smerge-prev))

  (with-eval-after-load 'hydra
    (defhydra smerge-hydra (:hint nil
				  :pre (if (not smerge-mode) (smerge-mode 1))
				  :post (smerge-auto-leave))
      "
                                                         [smerge]
  Movement   Keep           Diff              Other         │
  ╭─────────────────────────────────────────────────────────╯
  │  ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
  │  ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
  │  ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
  │  ^_j_ ↓^     [_a_] all        [_H_] highlight     [_n_] next in project
  │  ^_C-j_^     [_RET_] current  [_E_] ediff
  │  ^_G_^                                                 [_q_] quit
  ╰─────────────────────────────────────────────────────╯
"
      ("g" smerge-first)
      ("G" smerge-last)
      ("C-j" smerge-next)
      ("C-k" smerge-prev)
      ("j" next-line)
      ("k" previous-line)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("H" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("R" smerge-kill-current)
      ("n" (progn (smerge-vc-next-conflict) (recenter-top-bottom (/ (window-height) 8))))
      ("q" nil :color blue))))

(use-package compile
  :commands toggle-bury-compilation-buffer-if-successful
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output t)
  (compilation-always-kill t)
  (compilation-skip-visited t)
  (compilation-window-height 12)
  :init
  (defvar compilation-auto-bury-msg-level "warning"
    "Level of messages to consider OK to auto-bury the compilation buffer.")
  (defvar compilation-auto-bury-delay 3.0
    "The delay in seconds after which the compilation buffer is buried.")
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'compile-history))

  (defun compilation--bury-if-successful-h (buf str)
    "Bury the compilation buffer if it succeeds without warnings."
    (when (and
           (string-match "compilation" (buffer-name buf))
           (string-match "finished" str)
           (not (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward compilation-auto-bury-msg-level nil t)))))
      (run-at-time
       compilation-auto-bury-delay nil
       (lambda (b)
         (with-selected-window (get-buffer-window b)
           (kill-buffer-and-window))
         (unless (current-message)
           (message "Compilation finished without warnings.")))
       buf)))

  (defun toggle-bury-compilation-buffer-if-successful ()
    "Toggle auto-burying the successful compilation buffer."
    (interactive)
    (if (memq 'compilation--bury-if-successful-h compilation-finish-functions)
        (progn
          (message "Disabled burying compilation buffer.")
          (remove-hook 'compilation-finish-functions #'compilation--bury-if-successful-h))
      (message "Enabled burying compilation buffer.")
      (add-hook 'compilation-finish-functions #'compilation--bury-if-successful-h))))

(use-package eglot
  :straight t
  :hook (eglot-managed-mode . eglot-inlay-hints-mode)
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 0)
  (eglot-extend-to-xref t)
  (eglot-report-progress nil)
  :config
  (+map! :keymaps 'eglot-mode-map
         :infix "c"
         "fF" #'eglot-format-buffer
         "d" '(eglot-find-declaration :wk "Find declaration")
         "i" '(eglot-find-implementation :wk "Find implementation")
         "t" '(eglot-find-typeDefinition :wk "Find type definition")
         "a" '(eglot-code-actions :wk "Code actions")
         "r" '(nil :wk "refactor")
         "rr" '(eglot-rename :wk "Rename")
         "rR" '(eglot-code-action-rewrite :wk "Rewrite")
         "rf" '(eglot-code-action-quickfix :wk "Quick fix")
         "ri" '(eglot-code-action-inline :wk "Inline")
         "re" '(eglot-code-action-extract :wk "Extract")
         "ro" '(eglot-code-action-organize-imports :wk "Organize imports")
         "eq" '(eglot-shutdown :wk "Shutdown")
         "er" '(eglot-reconnect :wk "Reconnect")
         "eQ" '(eglot-shutdown-all :wk "Shutdown all")
         "w" '(eglot-show-workspace-configuration :wk "Eglot workspace config")))
(provide 'init-prog)
