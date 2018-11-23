;;; mu-dired.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for Dired.

;;; Code:

(use-package dired                      ; File manager
  :defer t
  :bind (("<C-return>" . mu-open-in-external-app)
         ("C-c f f"    . find-name-dired))
  :bind (:map dired-mode-map
              ("M-p"         . mu-dired-up)
              ("^"           . mu-dired-up)
              ("<backspace>" . mu-dired-up)
              ("M-n"         . mu-dired-down)
              ("RET"         . find-file-reuse-dir-buffer)
              ("!"           . mu-sudired)
              ("<prior>"     . beginend-dired-mode-goto-beginning)
              ("<next>"      . beginend-dired-mode-goto-end))
  :config
  (setq dired-dwim-target t             ; Use other pane as target
        dired-listing-switches "-lFah1v --group-directories-first"
        dired-ls-F-marks-symlinks t      ; -F marks links with @
        dired-recursive-copies 'always   ; Copy dirs recursively
        dired-recursive-deletes ' always ; Delete dirs recursively
        dired-auto-revert-buffer t)      ; Revert buffers on revisiting

  ;; Enable dired-find-alternate-file
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Handle long file names
  (add-hook 'dired-mode-hook #'toggle-truncate-lines)

  (defun mu-dired-up ()
    "Go to previous directory."
    (interactive)
    (find-alternate-file ".."))

  (defun mu-dired-down ()
    "Enter directory."
    (interactive)
    (dired-find-alternate-file))

  (defun mu-open-in-external-app ()
    "Open the file(s) at point with an external application."
    (interactive)
    (let* ((file-list
            (dired-get-marked-files)))
      (mapc
       (lambda (file-path)
         (let ((process-connection-type nil))
           (start-process "" nil "xdg-open" file-path))) file-list)))

  (defun find-file-reuse-dir-buffer ()
    "Like `dired-find-file', but reuse Dired buffers."
    (interactive)
    (set-buffer-modified-p nil)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (find-alternate-file file)
        (find-file file))))

  (defun mu-sudired ()
    "Open directory with sudo in Dired."
    (interactive)
    (require 'tramp)
    (let ((dir (expand-file-name default-directory)))
      (if (string-match "^/sudo:" dir)
          (user-error "Already in sudo")
        (dired (concat "/sudo::" dir))))))

(use-package find-dired                 ; Run `find' in Dired
  :config
  ;; Prefer case-insensitive search
  (setq find-name-arg "-iname")

  (defun mu-find-by-date (dir args)
    "Find file in DIR with given ARGS and sort the result by date."
    (interactive (list (read-directory-name "Run find in directory: " nil "" t)
                       (read-string "Run find (with args): " find-args
                                    '(find-args-history . 1))))
    (setq find-ls-option '("-exec ls -lt {} + | cut -d ' ' -f5-" . "-lt"))
    (find-dired dir args)
    (setq find-ls-option '("-ls" . "-dilsb")))

  (defun mu-find-by-size (dir args)
    "Find file in DIR with given ARGS and sort the result by size."
    (interactive (list (read-directory-name "Run find in directory: " nil "" t)
                       (read-string "Run find (with args): " find-args
                                    '(find-args-history . 1))))
    (setq find-ls-option '("-exec ls -lSr {} + | cut -d ' ' -f5-" . "-lSr"))
    (find-dired dir args)
    (setq find-ls-option '("-ls" . "-dilsb"))))

(use-package dired-aux                  ; Other Dired customizations
  :after dired
  :config
  (setq dired-create-destination-dirs 'ask
        ;; Search only file names when point is on a file name
        dired-isearch-filenames 'dwim))

(use-package dired-x                    ; Enable some nice Dired features
  :bind ("C-x C-j" . dired-jump)
  :config
  (unbind-key "N" dired-mode-map)
  (bind-key "N" #'dired-create-empty-file dired-mode-map)

  (setq dired-clean-confirm-killing-deleted-buffers nil
        dired-omit-files (concat dired-omit-files "\\|^\\.+$\\|^\\..+$")
        dired-omit-verbose nil)

  (add-hook 'dired-mode-hook #'dired-omit-mode))

(use-package dired-narrow               ; Live-narrowing of search results
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package diredfl                    ; Add colours to Dired
  :ensure t
  :config (diredfl-global-mode))

(use-package dired-rsync                ; Allow rsync from dired buffers
  :ensure t
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package fd-dired                   ; Use fd from Dired
  :ensure t
  :defer t)

(provide 'mu-dired)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-dired.el ends here
