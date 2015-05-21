;;; custom-files.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores dired and file settings.

;;; Code:
;; Assert the byte compiler that dired functions are defined, because we never
;; call them for non-dired buffers, so we can be sure that dired is always
;; loaded first.
(declare-function dired-get-marked-files "dired")
(declare-function dired-current-directory "dired")

(use-package dired
  :bind (("C-c z" . dired-get-size)
         ("C-c C" . custom/copy-filename-as-kill))
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

            ;; Enable dired-find-alternate-file
            (put 'dired-find-alternate-file 'disabled nil)

            ;; 'RET' reuses buffers if they are directories
            (defun find-file-reuse-dir-buffer ()
              "Like `dired-find-file', but reuse Dired buffers."
              (interactive)
              (set-buffer-modified-p nil)
              (let ((file (dired-get-file-for-visit)))
                (if (file-directory-p file)
                    (find-alternate-file file)
                  (find-file file))))

            (bind-key "RET" #'find-file-reuse-dir-buffer dired-mode-map)

            ;; '^' reuses the current buffer
            (defun up-directory ()
              "Change to parent directory."
              (interactive)
              (set-buffer-modified-p nil)
              (find-alternate-file ".."))

            (bind-key "^" #'up-directory dired-mode-map)

            ;; Make find-name-dired faster
            (use-package find-dired
              :config (setq find-ls-option
                            '("-print0 | xargs -0 ls -ld" . "-ld")))

            ;; Better M-< and M->
            (defun dired-back-to-top ()
              (interactive)
              (beginning-of-buffer)
              (dired-next-line 2))

            (bind-key (vector 'remap 'beginning-of-buffer) 'dired-back-to-top
                      dired-mode-map)

            (defun dired-jump-to-bottom ()
              (interactive)
              (end-of-buffer)
              (dired-next-line -1))

            (bind-key (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom
                      dired-mode-map)

            ;; Open directory with sudo in dired
            (defun sudired ()
              "Open directory with sudo in dired."
              (interactive)
              (use-package tramp)
              (let ((dir (expand-file-name default-directory)))
                (if (string-match "^/sudo:" dir)
                    (user-error "Already in sudo")
                  (dired (concat "/sudo::" dir)))))

            (bind-key "!" #'sudired dired-mode-map)

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
  :config (setq
           dired-omit-verbose nil ; Be less verbose, Dired
           ;; Omit dotfiles with C-x M-o
           dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package ztree ; Text-tree utilities for directories
  :ensure t
  :bind (("C-c f z" . ztree-dir)
         ("C-c f d " . ztree-diff)))

(setq view-read-only t) ; View read-only
(setq large-file-warning-threshold nil) ; No large file warning

;; Don't kill important buffers
(add-hook 'kill-buffer-query-functions
          #'custom/do-not-kill-important-buffers)

;; Autosave buffers when focus is lost
(add-hook 'focus-out-hook #'custom/force-save-some-buffers)

(provide 'custom-files)

;;; custom-files.el ends here
