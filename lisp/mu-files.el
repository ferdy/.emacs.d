;;; mu-files.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for various file types.

;;; Code:

(use-package files                      ; Core commands for files
  :bind (("C-c f z" . revert-buffer)
         ("C-c f /" . revert-buffer)
         ("C-c f l" . find-file-literally)))

(use-package recentf                    ; Manage recent files
  :demand t
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")

  (validate-setq recentf-max-saved-items 200
                 recentf-max-menu-items 15))

(use-package recentf                    ; Manage recent files
  :init (recentf-mode)
  :defer t
  :config
  (validate-setq recentf-max-saved-items 200
                 recentf-max-menu-items 15
                 recentf-exclude (list "/\\.git/.*\\'"
                                       "/elpa/.*\\'"
                                       "/tmp/"
                                       "/ssh:")))

(validate-setq view-read-only t)                 ; View read-only
(validate-setq large-file-warning-threshold nil) ; No large file warning

(use-package ffap                       ; Find files at point
  :defer t
  ;; Do not ping random hosts
  :config (validate-setq ffap-machine-p-known 'reject))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  ;; Ignore some additional directories and file extensions
  (dolist (name '(".cask"))
    ;; Ignore some additional directories
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (dolist (ext '(".fls" ".out" ".aux"))
    (add-to-list 'ignoramus-file-endings ext))

  (validate-setq ignoramus-file-basename-beginnings
                 '(
                   ".#"                                   ; emacs
                   "._"                                   ; thumbnails
                   "_cgo_export."                         ; go
                   ))

  (ignoramus-setup))

(use-package hardhat                    ; Protect user-writable files
  :ensure t
  :init (global-hardhat-mode)
  :diminish global-hardhat-mode)

(use-package sudo-edit                  ; Edit files as root, through Tramp
  :ensure t
  :defer t
  :bind ("C-c f s" . sudo-edit))

(use-package pdf-tools                  ; Better PDF support
  :ensure t
  :init (pdf-tools-install)
  :bind (:map pdf-view-mode-map
              ("M-w" . pdf-view-kill-ring-save)
              ("M-g" . pdf-view-goto-page)))

(use-package archive-mode                   ; Browse archive files
  :mode ("\\.\\(cbr\\)\\'" . archive-mode)) ; Enable .cbr support

(use-package csv-mode                   ; Better .csv files editing
  :ensure t
  :no-require t
  :mode "\\.csv\\'")

(use-package image-file                 ; Visit images as images
  :init (auto-image-file-mode))

(use-package image+                     ; Better image management
  :ensure t
  :after image
  :config
  (imagex-global-sticky-mode 1)
  (imagex-auto-adjust-mode 1)
  (validate-setq imagex-quiet-error t))

(use-package systemd                    ; Major mode for editing systemd units
  :ensure t
  :defer t
  :mode "\\.service\\'")

(use-package rst                        ; ReStructuredText
  :defer t
  :bind (:map rst-mode-map
              ("C-="     . nil)
              ;; For similarity with AUCTeX and Markdown
              ("C-c C-j" . rst-insert-list)
              ("M-RET"   . rst-insert-list)))

(use-package markdown-mode              ; Edit markdown files
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  ;; Do not hide URLs
  (setq-default markdown-hide-urls nil)

  ;; Process Markdown with Pandoc, using a custom stylesheet for nice output
  (let ((stylesheet (expand-file-name
                     (locate-user-emacs-file "etc/pandoc.css"))))
    (setq markdown-command
          (mapconcat #'shell-quote-argument
                     `("pandoc" "--toc" "--section-divs"
                       "--css" ,(concat "file://" stylesheet)
                       "--standalone" "-f" "markdown" "-t" "html5")
                     " ")))
  (add-hook 'markdown-mode-hook #'auto-fill-mode))

;;; Utilities and keybindings
(defun mu-current-file ()
  "Gets the \"file\" of the current buffer.

The file is the buffer's file name, or the `default-directory' in
`dired-mode'."
  (if (derived-mode-p 'dired-mode)
      default-directory
    (buffer-file-name)))

;;;###autoload
(defun mu-copy-filename-as-kill (&optional arg)
  "Copy the name of the currently visited file to kill ring.

With a zero prefix arg, copy the absolute file name.  With
\\[universal-argument] ARG, copy the file name relative to the
current Projectile project, or to the current buffer's
`default-directory', if the file is not part of any project.
Otherwise copy the non-directory part only."
  (interactive "P")
  (if-let ((file-name (mu-current-file))
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

;;;###autoload
(defun mu-delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;;###autoload
(defun mu-rename-this-file-and-buffer (new-name)
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

(bind-key "C-c f D" #'mu-delete-this-file)
(bind-key "C-c f R" #'mu-rename-this-file-and-buffer)
(bind-key "C-c f w" #'mu-copy-filename-as-kill)

;;; Additional bindings for built-ins
(bind-key "C-c f v d" #'add-dir-local-variable)
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

(defun mu-reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun mu-reload-dir-locals-for-all-buffers-in-this-directory ()
  "Reload dir-locals for all buffers in current buffer's `default-directory'."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (mu-reload-dir-locals-for-current-buffer)))))

(bind-key "C-c f v r" #'mu-reload-dir-locals-for-current-buffer)
(bind-key "C-c f v r" #'mu-reload-dir-locals-for-all-buffers-in-this-directory)

(provide 'mu-files)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-files.el ends here
