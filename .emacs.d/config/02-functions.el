;;;; 02-functions.el

;;; This file contains various functions

;; Replace HTML tags with the ones used by WordPress editor
(defvar oldtags '("<i>" "</i>" "<b>" "</b>"))
(defvar newtags '("<em>" "</em>" "<strong>" "</strong>"))

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

;; Reverts (reloads from file) the current buffer without asking any questions
(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

;; Create scratch buffer, useful if I kill it by mistake
(defun create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (cd "~/")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;; Toggle image display on/off, especially useful in eww
(defvar-local custom/display-images t)

(defun custom/toggle-image-display ()
  "Toggle images display on current buffer."
  (interactive)
  (setq custom/display-images
        (null custom/display-images))
  (custom/backup-display-property custom/display-images))

(defun custom/backup-display-property (invert &optional object)
  "Move the 'display property at POS to 'display-backup.
Only applies if display property is an image.
If INVERT is non-nil, move from 'display-backup to 'display
instead.
Optional OBJECT specifies the string or buffer. Nil means current
buffer."
  (let* ((inhibit-read-only t)
         (from (if invert 'display-backup 'display))
         (to (if invert 'display 'display-backup))
         (pos (point-min))
         left prop)
    (while (and pos (/= pos (point-max)))
      (if (get-text-property pos from object)
          (setq left pos)
        (setq left (next-single-property-change pos from object)))
      (if (or (null left) (= left (point-max)))
          (setq pos nil)
        (setq prop (get-text-property left from object))
        (setq pos (or (next-single-property-change left from object)
                      (point-max)))
        (when (eq (car prop) 'image)
          (add-text-properties left pos (list from nil to prop) object))))))

;; Edit file with root privileges
(defun open-with-sudo ()
  "Find file as root if necessary."
  (unless (and buffer-file-name
	       (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(add-hook 'find-file-hook 'open-with-sudo)

;; Run a program in a term buffer, if it is already running switch to it
(defun custom/term-start-or-switch (prg &optional use-existing)
  "Run program PRG in a terminal buffer. If USE-EXISTING is non-nil
and PRG is already running, switch to that buffer instead of starting
a new instance."
  (interactive)
  (let ((bufname (concat "*" prg "*")))
    (when (not (and use-existing
		    (let ((buf (get-buffer bufname)))
		      (and buf (buffer-name (switch-to-buffer bufname))))))
      (ansi-term prg prg))))

;; Create a keybinding to start some terminal program
(defmacro custom/program-shortcut (name key &optional use-existing)
  "Macro to create a key binding KEY to start some terminal program PRG;
    if USE-EXISTING is true, try to switch to an existing buffer"
  `(global-set-key ,key
		   '(lambda()
		      (interactive)
		      (custom/term-start-or-switch ,name ,use-existing))))
