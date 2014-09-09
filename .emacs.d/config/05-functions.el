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
  (custom/lists-same-length-p oldtags newtags)
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

;; Check if two lists have the same length
(defun custom/lists-same-length-p (a b)
  "Check if lists A and B have the same length."
  (if (eq (length a)
	  (length b))
       t
    (error ("Lists must have same length."))))

;; Insert the name of the other buffer or window into the active buffer
(defun insert-other-buffer-file-name ()
  "Inserts the full filepath of a buffer into the current buffer."
  (interactive)
  (insert (buffer-file-name
	   (get-buffer
	    (ido-completing-read
	     "Select the buffer to get the filename from: "
	     (loop for buf being the buffers
		   when (buffer-file-name buf)
		   collect (buffer-name buf) into file-buffers
		   finally return file-buffers))))))
