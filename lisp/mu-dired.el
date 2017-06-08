;;; mu-dired.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Dired.

;;; Code:

(use-package dired                      ; File manager
  :defer t
  :bind (("<C-return>" . mu-open-in-external-app)
         ("C-c f g"    . mu-dired-get-size)
         ("C-c f f"    . find-name-dired)
         ("C-c f r"    . mu-dired-recent-dirs))
  :bind (:map dired-mode-map
              ("M-<up>"   . mu-dired-up)
              ("M-p"      . mu-dired-up)
              ("^"        . mu-dired-up)
              ("RET"      . find-file-reuse-dir-buffer)
              ("M-<down>" . mu-dired-down)
              ("M-n"      . mu-dired-down)
              ("!"        . mu-sudired)
              ("e"        . mu-ediff-files))
  :config
  (validate-setq dired-auto-revert-buffer t ; Revert buffers on revisiting
                 dired-listing-switches
                 "-lFaGh1v --group-directories-first"  ; Add ls switches
                 global-auto-revert-non-file-buffers t ; Auto refresh Dired
                 auto-revert-verbose nil               ; But be quiet about it
                 dired-dwim-target t              ; Use other pane as target
                 dired-recursive-copies 'always   ; Copy dirs recursively
                 dired-recursive-deletes ' always ; Delete dirs recursively
                 ;; -F marks links with @
                 dired-ls-F-marks-symlinks t)

  ;; Enable dired-find-alternate-file
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Handle long file names
  (add-hook 'dired-mode-hook #'toggle-truncate-lines))

(use-package find-dired                 ; Run `find' in Dired
  :config (validate-setq find-ls-option '("-exec ls -ld {} \\+" . "-ld")))

(use-package dired-x                    ; Enable some nice Dired features
  :bind ("C-x C-j" . dired-jump)
  :bind (:map dired-mode-map
              ("Y" . mu-dired-rsync))
  :config
  (validate-setq dired-omit-verbose nil ; Be less verbose, Dired
                 ;; Omit dotfiles with C-x M-o
                 dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
  ;; a very peculiar way of registering its lighter explicitly in
  ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
  ;; isn't there yet after dired-omit-mode is loaded.
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode))
                '((name . dired-omit-mode-diminish))))

(use-package dired-narrow               ; Live-narrowing of search results
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;;; Utilities and key bindings
;;;###autoload
(defun mu-dired-up ()
  "Go to previous directory."
  (interactive)
  (find-alternate-file ".."))

;;;###autoload
(defun mu-dired-down ()
  "Enter directory."
  (interactive)
  (dired-find-alternate-file))

;;;###autoload
(defun mu-open-in-external-app ()
  "Open the file where point is or the marked files in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list
          (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (let ((process-connection-type nil))
         (start-process "" nil "xdg-open" file-path))) file-list)))

;;;###autoload
(defun find-file-reuse-dir-buffer ()
  "Like `dired-find-file', but reuse Dired buffers."
  (interactive)
  (set-buffer-modified-p nil)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (find-alternate-file file)
      (find-file file))))

;;;###autoload
(defun mu-sudired ()
  "Open directory with sudo in Dired."
  (interactive)
  (require 'tramp)
  (let ((dir (expand-file-name default-directory)))
    (if (string-match "^/sudo:" dir)
        (user-error "Already in sudo")
      (dired (concat "/sudo::" dir)))))

;;;###autoload
(defun mu-dired-get-size ()
  "Quick and easy way to get file size in Dired."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

;;;###autoload
(defun mu-dired-rsync (dest)
  "Copy files with `rysnc' to DEST."
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; Store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        (mu-rsync-command
         "rsync -arvz --progress "))
    ;; Add all selected file names as arguments to the rsync command
    (dolist (file files)
      (setq mu-rsync-command
            (concat mu-rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; Append the destination
    (setq mu-rsync-command
          (concat mu-rsync-command
                  (shell-quote-argument dest)))
    ;; Run the async shell command
    (async-shell-command mu-rsync-command "*rsync*")
    ;; Finally, switch to that window
    (other-window 1)))

;;;###autoload
(defun mu-dired-recent-dirs ()
  "List recently used directories and open the selected one in dired."
  (interactive)
  (let ((recent-dirs
         (delete-dups
          (mapcar (lambda (file)
                    (if (file-directory-p file)
                        file
                      (file-name-directory file)))
                  recentf-list))))
    (let ((dir (ivy-read "Directory: "
                         recent-dirs
                         :re-builder #'ivy--regex
                         :sort nil
                         :initial-input nil)))
      (dired dir))))

;;;###autoload
(defun mu-ediff-files ()
  "Ediff files from Dired."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "No more than 2 files should be marked"))))

(provide 'mu-dired)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-dired.el ends here
