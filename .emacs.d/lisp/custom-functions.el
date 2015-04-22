;;; custom-functions.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

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

(defun custom/only-strings-p (list)
  "Check if LIST does contain only strings."
  (and (not (eq (car list) nil))
       (if (stringp (car list))
	   (not (custom/only-strings-p (cdr list)))
	 (error "List must only contain strings"))))

(defun custom/lists-same-length-p (a b)
  "Check if lists A and B have the same length."
  (if (eq (length a)(length b)) t
    (error "Lists must have same length")))

(defun revert-this-buffer ()
  "Revert current buffer without asking for confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

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

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun comint-clear-buffer ()
  "Easily clear comint buffers."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun custom/kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (cl-letf (((symbol-function 'kill-buffer-ask)
	     (lambda (buffer) (kill-buffer buffer))))
    (kill-matching-buffers regexp)))

;; Searching buffers with occur mode
(eval-when-compile
  (require 'cl))

(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their MAJOR-MODE is equal to MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (push buf buffer-mode-matches))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(defun open-with-sudo ()
  "Find file using `sudo' with TRAMP."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

(add-hook 'find-file-hook #'open-with-sudo)

(defun just-one-space-in-region (beg end)
  "Replace all whitespace in the region from BEG to END with single spaces."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(defun custom/isearch-delete ()
  "Delete the failed portion of the search string, or the last char if successful."
  (interactive)
  (with-isearch-suspended
   (setq isearch-new-string
         (substring
          isearch-string 0 (or (isearch-fail-pos) (1- (length isearch-string))))
         isearch-new-message
         (mapconcat 'isearch-text-char-description isearch-new-string ""))))

(defun custom/forward-paragraph (&optional n)
  "Advance N times just past next blank line."
  (interactive "p")
  (let ((m (use-region-p))
        (para-commands
         '(custom/forward-paragraph custom/backward-paragraph)))
    ;; only push mark if it's not active and we're not repeating.
    (or m
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; the actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min)))))
    ;; if mark wasn't active, I like to indent the line too.
    (unless m
      (indent-according-to-mode)
      ;; this looks redundant, but it's surprisingly necessary.
      (back-to-indentation))))

(defun custom/backward-paragraph (&optional n)
  "Go back up N times to previous blank line."
  (interactive "p")
  (custom/forward-paragraph (- n)))

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(defun push-mark-no-activate ()
  "Pushes 'point' to 'mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument.  The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "custom/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

(provide 'custom-functions)

;;; custom-functions.el ends here
