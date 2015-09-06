;;; custom-files.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores dired and file settings.

;;; Code:

(use-package files ; Core commands for files
  :bind (("C-c f z" . revert-buffer)
         ("C-c f /" . revert-buffer)))

(use-package recentf ; Manage recent files
  :init (recentf-mode)
  :defer t
  :config (setq recentf-max-saved-items 200
                recentf-max-menu-items 15
                recentf-exclude (list "/\\.git/.*\\'"
                                      "/elpa/.*\\'"
                                      "/tmp/"
                                      "/ssh:")))

(use-package dired ; File manager
  :defer t
  :bind (("C-c f s"    . dired-get-size)
         ("<C-return>" . custom/open-in-external-app)
         ("C-c f f"    . find-name-dired))
  :config (progn
            (setq dired-auto-revert-buffer t            ; Revert buffers on revisiting
                  dired-listing-switches
                  "-lFaGh1v --group-directories-first"  ; Add ls switches
                  global-auto-revert-non-file-buffers t ; Auto refresh dired
                  auto-revert-verbose nil               ; But be quiet about it
                  dired-dwim-target t                   ; Use other pane as target
                  dired-recursive-copies 'always        ; Copy dirs recursively
                  dired-recursive-deletes ' always      ; Delete dirs recursively
                  ;; -F marks links with @
                  dired-ls-F-marks-symlinks t)

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
              :config (setq find-ls-option '("-exec ls -ld {} \\+" . "-ld")))

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
                     (match-string 1))))))

            ;; Handle long file names
            (add-hook 'dired-mode-hook #'toggle-truncate-lines)))

(use-package dired-x ; Enable some nice dired features
  :bind ("C-x C-j" . dired-jump)
  :config (progn
            (setq dired-omit-verbose nil ; Be less verbose, Dired
                  ;; Omit dotfiles with C-x M-o
                  dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
            (add-hook 'dired-mode-hook #'dired-omit-mode)))

(setq view-read-only t) ; View read-only
(setq large-file-warning-threshold nil) ; No large file warning

(use-package focus-autosave-mode ; Autosave buffers when focus is lost
  :ensure t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package ffap ; Find files at point
  :defer t
  :config (setq ffap-machine-p-known 'reject)) ;; Do not ping random hosts

(use-package pdf-tools ; Better PDF support
  :ensure t
  :bind ("C-c M-g" . pdf-view-goto-page)
  :init (pdf-tools-install))

(use-package archive-mode ; Browse archive files
  :mode ("\\.\\(cbr\\)\\'" . archive-mode)) ; Enable .cbr support

(use-package csv-mode ; Better .csv files editing
  :ensure t
  :no-require t
  :mode "\\.csv\\'")

(use-package image+ ; Better image management
  :defer t
  :ensure t
  :init (with-eval-after-load 'image
          (imagex-global-sticky-mode 1)
          (imagex-auto-adjust-mode 1))
  :config (setq imagex-quiet-error t))

(use-package systemd ; Major mode for editing systemd units
  :ensure t
  :mode "\\.service\\'")

;;; Utilities and keybindings
(defun custom/current-file ()
  "Gets the \"file\" of the current buffer.
The file is the buffer's file name, or the `default-directory' in
`dired-mode'."
  (if (derived-mode-p 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun custom/copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.
With a zero prefix arg, copy the absolute file name.  With
\\[universal-argument], copy the file name relative to the
current Projectile project, or to the current buffer's
`default-directory', if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let ((file-name (custom/current-file))
           (name-to-copy
            (cond
             ((zerop (prefix-numeric-value arg)) file-name)
             ((consp arg)
              (let* ((projectile-require-project-root nil)
                     (directory (and (fboundp 'projectile-project-root)
                                     (projectile-project-root))))
                (file-relative-name file-name directory)))
             (t (file-name-nondirectory file-name)))))
      (progn
        (kill-new name-to-copy)
        (message "%s" name-to-copy))
    (user-error "This buffer is not visiting a file")))

(bind-key "C-c C" #'custom/copy-filename-as-kill) ; Copy current file name

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

(defun custom/open-in-external-app ()
  "Open the file where point is or the marked files in Dired in external
app. The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list
          (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (let ((process-connection-type nil))
         (start-process "" nil "xdg-open" file-path))) file-list)))

(provide 'custom-files)

;;; custom-files.el ends here
