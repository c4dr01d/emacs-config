;; -*- lexical-binding: t -*-
(defconst linux-p (eq system-type 'gnu/linux) "Non-nil on GNU/Linux systems.")
(defconst bsd-p (and (memq system-type '(kerkeley-unix gnu/kfreebsd)) t) "Non-nil on BSD systems.")
(defconst windows-p (and (memq system-type '(cygwin windows-nt ms-dos)) t) "Non-nil on Windows systems.")
(defconst mac-p (eq system-type 'darwin) "Non-nil on MacOS systems.")

(provide 'distro)
