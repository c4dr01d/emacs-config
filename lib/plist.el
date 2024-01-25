;; -*- lexical-binding: t -*-
(defun varplist-get (vplist keyword &optional car-p)
  "Get KEYWORD's value from variable value length VPLIST.

Ex: (varplist-get \\='(:a \\='a :b \\='b1 \\='b2) :b) -> \\='(b1 b2)."
  (funcall
   (if car-p #'cadr #'cdr)
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
  "Create a single property list from all plists in PLISTS."
  (let ((res (copy-sequence (pop plists)))
	prop val plist)
    (while plists
      (setq plist (pop plists))
      (while plist
	(setq prop (pop plist) val (pop plist))
	(setq res (plist-put res prop (vconcat val (plist-get res prop))))))
    res))

(defun plist-delete (plist prop)
  "Delete property PROP from PLIST."
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
	(push (cons key val)  res)))
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

This method may mutate the original alist, but you still need to use the return
value of this method instead of the original alist, to ensure correct results."
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

(provide 'plist)
