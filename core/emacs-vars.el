;;; emacs-vars.el --- Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defgroup config nil
  "GNU Emacs specific functionalities.")

(defgroup config-core nil
  "Core tweaks."
  :group 'config)

(defgroup config-ui nil
  "UI tweaks."
  :group 'config)

(defgroup config-edit nil
  "Editor tweaks."
  :group 'config)

(defgroup config-prog nil
  "Programming stuff."
  :group 'config)

(defgroup config-keymaps nil
  "Emacs keybinding."
  :group 'config)

(defgroup config-utils nil
  "Utility functions."
  :group 'config)

(defconst ignore-user-config
  (let* ((ignores (getenv "EMACS_IGNORE_USER_CONFIG"))
         (ignores (and ignores (downcase ignores))))
    (when ignores
      (if (string= ignores "all")
          '(early-config init-config modules config local/early-config local/init-config local/modules local/config)
        (mapcar #'intern (split-string ignores)))))
  "Ignore loading these user configuration files.
Accepted values are: early-config, init-config, modules, config,
local/early-config, local/init-config, local/modules and local/config.
This list is automatically constructed from the space-separated values in the
environment variable \"$EMACS_IGNORE_USER_CONFIG\".")

(defconst emacs-debug-p
  (and (or (getenv "EMACS_DEBUG") init-file-debug) t)
  "Emacs is started in debug mode.")

(defconst emacs-verbose-p
  (and (or (getenv "EMACS_VERBOSE") emacs-debug-p) t)
  "Emacs is started in verbose mode.")

(defconst emacs-always-demand-p
  (and (getenv "EMACS_ALWAYS_DEMAND") t)
  "Load all packages immediately, do not defer any package.")

(defconst emacs-not-lazy-p
  (or emacs-always-demand-p (daemonp) (and (getenv "EMACS_NOT_LAZY") t))
  "Load lazy packages (emacs-lazy-hook) immediately.")

(defcustom emacs-msg-level
  (let ((level (string-to-number (or (getenv "EMACS_MSG_LEVEL") ""))))
    (cond (emacs-verbose-p)
          ((> level 0) level)
          (t 1)))
  "Level or printed messages.
1 - `error!'
2 - `info!'
3 - `log!'"
  :group 'config-core
  :type '(choice
          (const :tag "Error" 1)
          (const :tag "Info" 2)
          (const :tag "Log" 3)
          (const :tag "Debug" 4)))

(defconst emacs-root-dir (abbreviate-file-name (file-name-directory (directory-file-name (file-name-directory (file-truename load-file-name))))))
(defconst emacs-core-dir (concat emacs-root-dir "core/"))
(defconst emacs-elisp-dir (concat emacs-root-dir "lisp/"))
(defconst emacs-modules-dir (concat emacs-root-dir "modules/"))
(defconst emacs-extras-dir (concat emacs-modules-dir "extras/"))
(defconst emacs-local-dir (concat emacs-root-dir "local/"))
(defconst emacs-cache-dir (concat emacs-root-dir "cache/"))
(defconst emacs-loaddefs-file (concat emacs-core-dir "emacs-loaddefs.el"))
(defconst emacs-config-dir (file-name-as-directory (or (getenv "EMACS_DIR")
                                                       (getenv "EMACSDIR")
                                                       (if (file-directory-p "~/.config/emacs") "~/.config/emacs") (concat emacs-root-dir "user-config/"))))

(defconst started-with-extra-args-p (and (cdr command-line-args) t)
  "Has Emacs been started with extras arguments? like a file name or so.")
(defconst os-linux-p (eq system-type 'gnu/linux)
  "Non-nil on GNU/Linux systems.")
(defconst os-bsd-p (and (memq system-type '(berkeley-unix gnu/kfreebsd)) t)
  "Non-nil on BSD systems.")
(defconst os-windows-p (and (memq system-type '(cygwin windows-nt ms-dos)) t)
  "Non-nil on Windows systems.")
(defconst os-darwin-p (eq system-type 'darwin)
  "Non-nil on MacOS systems.")

(defconst system-arch (intern (car (split-string system-configuration "-")))
  "The system's architecture read from `system-configuration'.
It return a symbol like `x86_64', `aarch64', `armhf', ...")

(defconst emacs-features
  (mapcar #'intern
          (mapcar (apply-partially #'string-replace "_" "-")
                  (mapcar #'downcase (split-string system-configuration-features))))
  "List of symbols representing Emacs enabled features.
Compiled from the `system-configuration-features'.")

(defcustom emacs-leader-key "SPC"
  "Emacs leader key."
  :group 'config-keymaps
  :type 'string)

(defcustom emacs-localleader-key "SPC m"
  "Emacs local leader key."
  :group 'config-keymaps
  :type 'string)

(defcustom emacs-global-leader-prefix "C-SPC"
  "Emacs general leader key."
  :group 'config-keymaps
  :type 'string)

(defcustom emacs-global-mode-prefix "C-SPC m"
  "Emacs general local leader key."
  :group 'config-keymaps
  :type 'string)

(defcustom emacs-theme 'doom-one-light
  "The theme of Emacs."
  :group 'config-ui
  :type 'symbol)

(defcustom emacs-disabled-packages nil
  "List of packages to be disabled when loading Emacs modules.
This can be useful if you want to enable a module but you don't want a package
of being enabled."
  :group 'config-core
  :type '(list symbol))

(defvar emacs-configured-packages nil
  "List of packages installed and configured by Emacs during startup.")

(defcustom emacs-after-loading-modules-hook nil
  "This hook will be run after loading Emacs modules.
It is used internally to remove the `use-package--check-if-disabled:around-a'
advice we set on `use-package' in `emacs-bootstrap'."
  :group 'config-core
  :type 'hook)

(defcustom emacs-after-setup-fonts-hook nil
  "Runs after setting Emacs fonts, runs at the end of `setup-fonts'."
  :group 'config-ui
  :type 'hook)

(defcustom emacs-after-load-theme-hook nil
  "Runs after loading Emacs theme, runs at the end of `load-theme-wrapper'."
  :group 'config-ui
  :type 'hook)

(defcustom emacs-after-startup-hook nil
  "This hook will be run after loading Emacs.

Emacs hooks will be run in this order:
1. `emacs-after-startup-hook'
2. `emacs-lazy-hook'"
  :group 'config-core
  :type 'hook)

(defcustom emacs-lazy-hook nil
  "This hook will be run after loading Emacs.

Emacs hooks will be run in this order:
1. `emacs-after-startup-hook'
2. `emacs-lazy-hook'"
  :group 'config-core
  :type 'hook)

(defvaralias 'emacs-build-functions-hook 'emacs-build-functions)
(defvar emacs-build-functions nil
  "Special hook for build functions that are run after completing package updates.")

(defcustom env-file (concat emacs-local-dir "system-env.el")
  "The file in which the environment variables will be saved."
  :group 'config-core
  :type 'file)

(defcustom env-deny-vars
  '(;; Unix/shell state that shouldn't be persisted
    "^HOME$" "^\\(OLD\\)?PWD$" "^SHLVL$" "^PS1$" "^R?PROMPT$" "^TERM\\(CAP\\)?$"
    "^USER$" "^GIT_CONFIG" "^INSIDE_EMACS$" "^SESSION_MANAGER$" "^_$"
    "^JOURNAL_STREAM$" "^INVOCATION_ID$" "^MANAGERPID$" "^SYSTEMD_EXEC_PID$"
    "^DESKTOP_STARTUP_ID$" "^LS_?COLORS$" "^$"
    ;; Python virtual environment
    "^VIRTUAL_ENV$"
    ;; KDE session
    "^KDE_\\(FULL_SESSION\\|APPLICATIONS_.*\\|SESSION_\\(UID\\|VERSION\\)\\)$"
    ;; X server, Wayland, or services' env  that shouldn't be persisted
    "^DISPLAY$" "^WAYLAND_DISPLAY" "^DBUS_SESSION_BUS_ADDRESS$" "^XAUTHORITY$"
    ;; Windows+WSL envvars that shouldn't be persisted
    "^WSL_INTEROP$"
    ;; XDG variables that are best not persisted.
    "^XDG_CURRENT_DESKTOP$" "^XDG_RUNTIME_DIR$"
    "^XDG_\\(VTNR\\|SEAT\\|SESSION_\\(TYPE\\|CLASS\\|ID\\|PATH\\|DESKTOP\\)\\)"
    ;; Socket envvars, like I3SOCK, GREETD_SOCK, SEATD_SOCK, SWAYSOCK, etc.
    "SOCK$"
    ;; SSH and GPG variables that could quickly become stale if persisted.
    "^SSH_\\(AUTH_SOCK\\|AGENT_PID\\)$" "^\\(SSH\\|GPG\\)_TTY$" "^GPG_AGENT_INFO$"
    ;; Emacs envvars
    "^EMACS\\(_?DIR\\|_\\(ALPHA\\|DEBUG\\|VERBOSE\\|NOT_LAZY\\|MSG_LEVEL\\|IGNORE_.*\\)\\)$")
  "Environment variables to omit.
Each string is a regexp, matched against variable names to omit from
`env-file' when saving environment variables in `env-save'."
  :group 'config-core
  :type '(repeat regexp))

(defun load-user-configs (&rest configs)
  "Load user configurations CONFIGS."
  (dolist (config configs)
    (unless (memq config emacs-ignore-user-config)
      (let ((config-path (format "%s%s.el" emacs-config-dir config)))
        (when (file-exists-p config-path) (load-wrapper config-path))))))

(defun load-wrapper (&rest filename-parts)
  "Load a file, the FILENAME-PARTS are concatenated to form the file name."
  (let ((filename (file-truename (apply #'file-name-concat filename-parts))))
    (if (file-exists-p filename)
        (if emacs-debug-p
            (load filename nil)
          (with-demoted-errors "[Emacs:LoadError] %s"
            (load filename nil (not emacs-verbose-p))))
      (message "[Emacs:Error] Cannot load \"%s\", the file doesn't exists." filename))))

(provide 'emacs-vars)
;;; emacs-vars.el ends here.
