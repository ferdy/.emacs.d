;;;; 05-functions.el

;;; This file contains all my custom functions

(defvar oldtags '("<i>" "</i>" "<b>" "</b>"))
(defvar newtags '("<em>" "</em>" "<strong>" "</strong>"))

;; Replace HTML tags with the ones used by WordPress editor
(defun custom/replace-html-tags ()
  "Replace HTML tags with the ones used by WordPress editor."
  (interactive)
  (custom/replace-string-matches-recursively oldtags newtags))

;; Replace OLDTAGS elements with NEWTAGS elements recursively
(defun custom/replace-string-matches-recursively (oldtags newtags)
  "Replace OLDTAGS elements with NEWTAGS elements recursively."
  (custom/only-strings-p oldtags)
  (custom/only-strings-p newtags)
  (and (not (eq (length oldtags)
		(length newtags)))
       (error ("Lists must have same length.")))
  (and (not (eq (car oldtags) nil))
       (not (eq (car newtags) nil))
       (save-excursion
	 (save-restriction
	   (save-match-data
	     (goto-char (point-min))
	     (replace-string (car oldtags)
			     (car newtags))
	     (custom/replace-string-matches-recursively (cdr oldtags)
						 (cdr newtags)))))))

;; Check if the given list contains only strings
(defun custom/only-strings-p (list)
  "Check if LIST contains only strings."
  (and (not (eq (car list) nil))
       (if (stringp (car list))
	   (not (custom/only-strings-p (cdr list)))
	 (error ("List must only contain strings.")))))
