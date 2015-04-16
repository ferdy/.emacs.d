;;; custom-files.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores dired and file settings.

;;; Code:

(use-package dired
  :bind (("C-c z" . dired-get-size)
         ("C-c C" . copy-file-name-to-clipboard))
  :config
  (progn
    (setq dired-auto-revert-buffer t ; Always revert Dired buffers on revisiting
          dired-listing-switches "-laGh1v --group-directories-first"
          global-auto-revert-non-file-buffers t ; Also auto refresh dired
          dired-recursive-copies 'always
          auto-revert-verbose nil) ; But be quiet about it

    ;; Make find-name-dired faster
    (require 'find-dired)
    (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

    ;; Better M-< and M->
    (defun dired-back-to-top ()
      (interactive)
      (beginning-of-buffer)
      (dired-next-line 2))

    (define-key dired-mode-map
      (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

    (defun dired-jump-to-bottom ()
      (interactive)
      (end-of-buffer)
      (dired-next-line -1))

    (define-key dired-mode-map
      (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

    (setq dired-dwim-target t) ; Use other pane as destination when copying

    ;; Open directory with sudo in dired
    (define-key dired-mode-map "!" 'sudired)

    (defun sudired ()
      "Open directory with sudo in dired."
      (interactive)
      (require 'tramp)
      (let ((dir (expand-file-name default-directory)))
        (if (string-match "^/sudo:" dir)
            (user-error "Already in sudo")
          (dired (concat "/sudo::" dir)))))

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
             (match-string 1))))))))

(use-package dired-x ; Enable some nice dired features
  :config
  (progn
    ;; Omit hidden files by default (C-x M-o to show them)
    (setq-default dired-omit-files-p t)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
    ;; Hide omit files messages
    (setq dired-omit-verbose nil)))

;; Note: to override dired+ faces, customize `font-lock-maximum-decoration'
;; adding an entry for `dired-mode' and setting decoration to default
(use-package dired+ ; Extend dired
  :ensure t
  :defer 5
  :config
  (progn
    ;; Reuse buffer for directories
    (diredp-toggle-find-file-reuse-dir 1)
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)))

(use-package bookmark+ ; Better bookmarks
  :ensure t
  :defer 10)

;; Set the directory where all backup and autosave files will be saved
(defvar backup-dir "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))

(setq view-read-only t) ; View read-only
(setq large-file-warning-threshold nil) ; No large file warning

(provide 'custom-files)

;;; custom-files.el ends here
