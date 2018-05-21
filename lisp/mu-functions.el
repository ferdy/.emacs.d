;;; mu-functions.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file contains various useful functions and macros.

;;; Code:

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

;;;###autoload
(defun mu-display-version ()
  "Insert Emacs version info and system details at point.
It requires `s.el'."
  (interactive)
  (with-help-window "*version*"
    (with-current-buffer "*version*"
      (insert (emacs-version) "\n")
      (when (and (boundp 'emacs-repository-version)
                 (stringp emacs-repository-version))
        (insert "\nRepository revision: " emacs-repository-version "\n"))
      (let ((lsb (with-temp-buffer
                   (and (eq 0
                            (call-process "lsb_release" nil '(t nil) nil "-d"))
                        (buffer-string)))))
        (when (stringp lsb)
          (insert "\nSystem " (s-collapse-whitespace lsb) "\n")))
      (insert "\nWindowing system distributor `" (x-server-vendor)
              "', version "
              (mapconcat 'number-to-string (x-server-version) ".") "\n")
      (when (and system-configuration-options
                 (not (equal system-configuration-options "")))
        (insert "\nConfigured using:\n `configure "
                system-configuration-options "'\n\n")
        (fill-region (line-beginning-position -1) (point))))))

(provide 'mu-functions)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-functions.el ends here
