;;; 01-functions.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:
;; This file contains various useful functions.

;;; Code:
;; Replace HTML tags with the ones used by WordPress editor
(defvar my-oldtags '("<i>" "</i>" "<b>" "</b>"))
(defvar my-newtags '("<em>" "</em>" "<strong>" "</strong>"))

(defun custom/replace-html-tags ()
  "Replace HTML tags with the ones used by WordPress editor."
  (interactive)
  (custom/replace-string-matches-recursively my-oldtags my-newtags))

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
	     (while (search-forward (car oldtags) nil 'noerror)
	       (replace-match (car newtags)))
	     (custom/replace-string-matches-recursively (cdr oldtags)
							(cdr newtags)))))))

;; Check if the given list contains only strings
(defun custom/only-strings-p (list)
  "Check if LIST does contain only strings."
  (and (not (eq (car list) nil))
       (if (stringp (car list))
	   (not (custom/only-strings-p (cdr list)))
	 (error "List must only contain strings"))))

;; Check if two lists have the same length
(defun custom/lists-same-length-p (a b)
  "Check if lists A and B have the same length."
  (if (eq (length a)(length b)) t
    (error "Lists must have same length")))

;; Reverts (reloads from file) the current buffer without asking any questions
(defun revert-this-buffer ()
  "Revert current buffer."
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
 Optional OBJECT specifies the string or buffer.  Nil means current
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
  "Run program PRG in a terminal buffer.
If USE-EXISTING is non-nil and PRG is already running,
switch to that buffer instead of starting a new instance."
  (interactive)
  (let ((bufname (concat "*" prg "*")))
    (unless (and use-existing
		 (let ((buf (get-buffer bufname)))
		   (and buf (buffer-name (switch-to-buffer bufname))))))
    (ansi-term prg prg)))

;; Create a keybinding to start some terminal program
(defmacro custom/program-shortcut (name key &optional use-existing)
  "Macro to start some terminal program NAME with 'key-binding' KEY;
if USE-EXISTING is true, try to switch to an existing buffer"
  `(global-set-key ,key
		   '(lambda()
		      (interactive)
		      (custom/term-start-or-switch ,name ,use-existing))))

;; Copy current file name to clipboard
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; Open recent files with ido
(defun ido-recentf-open ()
  "Use 'ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; Get files size in dired
(defun dired-get-size ()
  "Quick and easy way to get file size in dired."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

;; Clear comint buffers
(defun comint-clear-buffer ()
  "Easily clear comint buffers."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; Open eshell buffer in the current directory
(defun eshell-here ()
  "Open a new shell in the directory of the buffer's file.
The eshell is renamed to match that directory to make multiple eshell
windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
	 (name   (car (last (split-string parent "/" t)))))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))

;; Kill buffers matching a regular expression with no confirmation
(defun custom/kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (cl-letf (((symbol-function 'kill-buffer-ask)
	     (lambda (buffer) (kill-buffer buffer))))
    (kill-matching-buffers regexp)))

(provide '01-functions)

;;; 01-functions.el ends here
