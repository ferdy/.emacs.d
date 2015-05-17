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
  :config (progn
            (setq dired-auto-revert-buffer t            ; Revert buffers on revisiting
                  dired-listing-switches
                  "-lFaGh1v --group-directories-first"  ; Add ls switches
                  global-auto-revert-non-file-buffers t ; Auto refresh dired
                  auto-revert-verbose nil               ; But be quiet about it
                  dired-dwim-target t                   ; Use other pane as target
                  dired-recursive-copies 'always        ; Copy dirs recursively
                  dired-recursive-deletes ' always      ; Delete dirs recursively
                  dired-ls-F-marks-symlinks t           ; -F marks links with @
                  dired-guess-shell-alist-user          ; Use LibreOffice when needed
                  '(("\\.ods\\'\\|\\.xls?\\'\\|\\.xlsx?\\'" "libreoffice")
                    ("\\.odt\\'\\|\\.doc?\\'\\|\\.docx?\\'" "libreoffice")))

            ;; 'a' reuses the current buffer, 'RET' opens a new one
            (put 'dired-find-alternate-file 'disabled nil)

            ;; '^' reuses the current buffer
            (add-hook 'dired-mode-hook
                      (lambda ()
                        (define-key dired-mode-map (kbd "^")
                          (lambda ()
                            (interactive)
                            (find-alternate-file "..")))))

            ;; Make find-name-dired faster
            (use-package find-dired
              :config (setq find-ls-option
                            '("-print0 | xargs -0 ls -ld" . "-ld")))

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

            ;; Open directory with sudo in dired
            (define-key dired-mode-map "!" 'sudired)

            (defun sudired ()
              "Open directory with sudo in dired."
              (interactive)
              (use-package tramp)
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
  :bind ("C-x C-j" . dired-jump)
  :config (setq dired-omit-verbose nil)) ; Be less verbose, Dired

(use-package ztree ; Text-tree utilities for directories
  :ensure t
  :bind (("C-c f z" . ztree-dir)
         ("C-c f d " . ztree-diff)))

(setq view-read-only t) ; View read-only
(setq large-file-warning-threshold nil) ; No large file warning

(provide 'custom-files)

;;; custom-files.el ends here
