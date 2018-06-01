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
(defun mu-face-at-point (pos)
  "Show face at POS (point)."
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun mu--colour-theme ()
  "Get the currently applied colour theme."
  (replace-regexp-in-string
   "[\t\n\r ]+" ""
   (let ((rs (shell-command-to-string "rg load-theme")))
     (nth 1 (split-string rs "'")))))

(defun mu--os-version ()
  "Call `lsb_release' to retrieve OS version."
  (replace-regexp-in-string
   "Description:\\|[\t\n\r]+" ""
   (shell-command-to-string "lsb_release -d")))

(defun mu--gnome-version ()
  "Call `gnome-shell' to retrieve GNOME version."
  (replace-regexp-in-string
   "[\t\n\r]+" ""
   (shell-command-to-string "gnome-shell --version")))

;;;###autoload
(defun mu-env-info ()
  "Display Emacs version and system details in a temporary buffer."
  (interactive)
  (let ((buffer-name "*version*"))
    (with-help-window buffer-name
      (with-current-buffer buffer-name
        (insert (emacs-version) "\n\n")
        (insert "Repository revision: " emacs-repository-version "\n")
        (when (and system-configuration-options
                   (not (equal system-configuration-options "")))
          (insert "Configured using:\n"
                  system-configuration-options))
        (insert "\n\nEmacs uptime: " (emacs-uptime) "\n")
        (insert "Colour theme: " (mu--colour-theme) "\n")
        (insert "Operating system: " (mu--os-version) "\n")
        (insert "Window system: " (getenv "XDG_SESSION_TYPE") "\n")
        (insert "Desktop environment: " (mu--gnome-version))))))

(bind-key "C-c h v" #'mu-env-info)

(provide 'mu-functions)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-functions.el ends here
