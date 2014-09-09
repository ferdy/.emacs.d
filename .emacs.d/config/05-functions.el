;;;; 05-functions.el

;;; This file contains all my custom functions

;; Replace HTML tags with the ones used by WordPress editor.
(defvar oldtags '("<i>" "</i>" "<b>" "</b>") )
(defvar newtags '("<em>" "</em>" "<strong>" "</strong>") )

(defun custom/replace-html-tags ()
  "Replace HTML tags with the ones used by WordPress editor."
  (interactive)
  (custom/replace-matches-recursively oldtags newtags))

(defun custom/replace-matches-recursively (oldtags newtags)
  "Replace OLDTAGS elements with NEWTAGS elements recursively."
  (and (not (eq (car oldtags) nil))
       (not (eq (car newtags) nil))
       (save-excursion
	 (save-restriction
	   (save-match-data
	     (goto-char (point-min))
	     (replace-string (car oldtags)
			     (car newtags))
	     (custom/replace-matches-recursively (cdr oldtags)
						 (cdr newtags)))))))
