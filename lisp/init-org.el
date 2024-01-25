;; -*- lexical-binding: t -*-
(use-package org
  :straight (:type built-in)
  :preface
  (setq org-directory nil
        org-fold-core-style 'overlays)
  :custom
  (org-auto-align-tags nil)
  (org-clock-persist-file (concat user-emacs-directory "org/clock-persist.el"))
  (org-cycle-hide-block-startup t)
  (org-edit-src-auto-save-idle-delay auto-save-timeout)
  (org-edit-src-content-indentation 0)
  (org-edit-src-turn-on-auto-save t)
  (org-ellipsis " ↩")
  (org-export-async-init-file (expand-file-name (concat user-emacs-directory "extras/me-org-export-async-init.el")))
  (org-export-in-background t)
  (org-export-with-broken-links 'mark)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts '{})
  (org-fold-catch-invisible-edits 'smart)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native script entities))
  (org-id-locations-file (concat user-emacs-directory "org/id-locations.el"))
  (org-insert-heading-respect-content t)
  (org-list-allow-alphabetical t)
  (org-log-done 'time)
  (org-persist-directory (directory-ensure user-emacs-directory "org/persist/"))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-preview-latex-image-directory (directory-ensure user-emacs-directory "org/preview/latex-image/"))
  (org-publish-timestamp-directory (directory-ensure user-emacs-directory "org/publish/timestamps/"))
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-startup-indented nil)
  (org-tags-column 0)
  (org-use-property-inheritance t)
  (org-use-sub-superscripts '{})
  :config
  (map-local! :keymaps 'org-mode-map
    "l" '(nil :wk "link")
    "ll" #'org-insert-link
    "e" #'org-export-dispatch
    "c" #'org-edit-src-code
    "s" '(nil :wk "babel-session")
    "sc" #'org-babel-switch-to-session-with-code
    "ss" #'org-babel-switch-to-session
    "sp" #'org-babel-pop-to-session
    "sP" #'org-babel-pop-to-session-maybe
    "sl" #'org-babel-load-in-session
    "sL" #'org-babel-load-in-session-maybe
    "si" #'org-babel-initiate-session
    "b" '(nil :wk "babel")
    "bt" #'org-babel-tangle
    "bd" #'org-babel-detangle
    "bf" #'org-babel-tangle-file)
  (map-local! :keymaps 'org-src-mode-map
    "s" #'org-edit-src-save
    "q" #'org-edit-src-abort
    "e" #'org-edit-src-exit)
  (nmap! :keymaps 'org-mode-map
    "RET" #'org-open-at-point)

  (defvar org-level-base-size 1.3)

  (dotimes (level 8)
    (let ((size (max 1.0 (* org-level-base-size (expt 0.9 level)))))
      (set-face-attribute
       (intern (format "org-level-%d" (1+ level))) nil
       :weight 'bold
       :height size)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   (cl-loop
    for lang in '(C R js dot awk sed sql org shell ditaa latex julia sqlite octave
                  maxima eshell scheme python fortran gnuplot plantuml makefile)
    collect (cons lang t)))

  (with-eval-after-load 'org-src
    (setq org-src-lang-modes
          (append
           '(("dot" . graphviz-dot))
           (delete (assoc "dot" org-src-lang-modes #'equal) org-src-lang-modes))))

  (with-eval-after-load 'plantuml-mode
    (setq org-plantuml-jar-path plantuml-jar-path
          org-plantuml-exec-mode plantuml-default-exec-mode
          org-plantuml-executable-path plantuml-executable-path))

  (with-no-warnings
    (custom-declare-face 'org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face 'org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face 'org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face 'org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "PROJ(p)"
           "LOOP(r)"
           "STRT(s)"
           "WAIT(w)"
           "HOLD(h)"
           "IDEA(i)"
           "|"
           "DONE(d)"
           "KILL(k)")
          (sequence
           "[ ](T)"
           "[-](S)"
           "[?](W)"
           "|"
           "[X](D)")
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]" . org-todo-active)
          ("STRT" . org-todo-active)
          ("[?]" . org-todo-onhold)
          ("WAIT" . org-todo-onhold)
          ("HOLD" . org-todo-onhold)
          ("PROJ" . org-todo-project)
          ("NO" . org-todo-cancel)
          ("KILL" . org-todo-cancel))))

(use-package org-agenda
  :custom
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

(use-package org-indent
  :after org
  :demand t)

(use-package ox
  :after org)

(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode))

(use-package evil-org-agenda
  :after evil-org
  :demand t
  :config
  (evil-org-agenda-set-keys))
(provide 'init-org)
