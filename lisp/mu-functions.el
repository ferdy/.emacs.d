;;; mu-functions.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

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
  (let ((tokei (executable-find "tokei")))
    (unless tokei
      (user-error "Please install tokei"))
    (with-current-buffer (get-buffer-create " *LoC Emacs configuration*")
      (text-mode)
      (read-only-mode)
      (view-mode)
      (let ((inhibit-read-only t)
            (files (mu-all-init-files with-packages)))
        (erase-buffer)
        (goto-char (point-min))
        (apply #'call-process tokei nil t t files))
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
(defun mu-emacs-debug-version ()
  "Show version of Emacs and 7 characters of the commit hash."
  (interactive)
  (message
   (format "GNU Emacs %s (commit %s)"
           emacs-version
           (substring (emacs-repository-get-version) 0 7))))

;;;###autoload
(defun mu-face-at-point (pos)
  "Show face at POS (point)."
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(provide 'mu-functions)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-functions.el ends here
