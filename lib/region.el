;; -*- lexical-binding: t -*-
(defun region-or-thing-at-point ()
  "Return the region or the thing at point."
  (when-let ((thing (ignore-errors
		      (or
		       (prog1 (thing-at-point 'region t)
			 (deactivate-mark))
		       (cl-some (apply-partially-right #'thing-at-point t)
				'(symbol email number string word))))))
    (string-join (string-lines thing))))
(provide 'region)
