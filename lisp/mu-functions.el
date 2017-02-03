;;; mu-functions.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file contains various useful functions and macros.

;;; Code:

(defun mu-all-init-files (&optional with-packages)
  "Return a list of all Emacs Lisp files in my configuration.

If WITH-PACKAGES is given and non-nil include 3rd party
packages."
  (append (list user-init-file)
          (directory-files-recursively (locate-user-emacs-file "lisp/")
                                       (rx ".el" eos))
          (if with-packages
              (directory-files-recursively package-user-dir
                                           (rx ".el" eos))
            nil)))

;;;###autoload
(defun mu-count-config-lines (&optional with-packages)
  "Show a buffer with LoC statistics for my Emacs config.

If WITH-PACKAGES is given and non-nil include 3rd party packages
into the count."
  (interactive "P")
  (let ((cloc (executable-find "cloc")))
    (unless cloc
      (user-error "Please install cloc"))
    (with-current-buffer (get-buffer-create " *LoC Emacs configuration*")
      (text-mode)
      (read-only-mode)
      (view-mode)
      (let ((inhibit-read-only t)
            (files (mu-all-init-files with-packages)))
        (erase-buffer)
        (goto-char (point-min))
        (apply #'call-process cloc nil t t "--quiet" files))
      (pop-to-buffer (current-buffer)))))

(defmacro with-timer (&rest forms)
  "Run the given FORMS, counting and displaying the elapsed time."
  (declare (indent 0))
  (let ((nowvar (make-symbol "now"))
        (body `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (prog1 ,body
         (let ((elapsed (float-time (time-subtract (current-time) ,nowvar))))
           (when (> elapsed 0.001)
             (message "spent (%.3fs)" elapsed)))))))

(defun mu--string-to-acronym (string)
  "Convert STRING into an acronym.

An acronym must be uppercase and have each letter followed by a dot."
  (s-upcase (s-append "." (s-join "." (delete "" (s-split "" string))))))

;;;###autoload
(defun mu-word-to-acronym (arg)
  "Convert word at or next to point to its acronym.

With numerical argument ARG, convert the next ARG-1 words as well.
With negative argument, convert previous words."
  (interactive "p")
  ;; With negative argument, move back point firstly
  (when (< arg 0)
    ;; If point is in a word but not at the beginning of that word,
    ;; then move to the beginning
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when (and bounds (not (= (car bounds) (point))))
        (forward-word -1)))
    (forward-word arg))
  (dotimes (_ (abs arg))
    ;; Adjust point in case point isn't on any word
    (forward-word)
    (backward-word)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (str (buffer-substring beg end)))
          (delete-region beg end)
          (goto-char beg)
          (insert (mu--string-to-acronym str)))))))

;;;###autoload
(defun mu-touchpad-off ()
  "Disable touchpad using an external shell script.

It needs https://github.com/manuel-uberti/scripts/blob/master/touchpad-off
in `exec-path'.

Examples:
\(add-hook 'focus-in-hook #'mu-touchpad-off)"
  (interactive)
  (let ((toff (executable-find "touchpad-off")))
    (unless toff
      (user-error "Shell script touchpad-off not found"))
    (with-temp-buffer
      (shell-command toff t))))

;;;###autoload
(defun mu-touchpad-on ()
  "Enable touchpad using an external shell script.

It needs https://github.com/manuel-uberti/scripts/blob/master/touchpad-on
in `exec-path'.

Examples:
\(add-hook 'focus-out-hook #'mu-touchpad-on)
\(add-hook 'delete-frame-functions #'mu-touchpad-on)"
  (interactive)
  (let ((ton (executable-find "touchpad-on")))
    (unless ton
      (user-error "Shell script touchpad-on not found"))
    (with-temp-buffer
      (shell-command ton t))))

;;;###autoload
(defun mu-emacs-debug-version ()
  "Show version of Emacs and 7 characters of the commit hash."
  (interactive)
  (message
   (format "GNU Emacs %s (commit %s)"
           emacs-version
           (substring (emacs-repository-get-version) 0 7))))

(provide 'mu-functions)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-functions.el ends here
