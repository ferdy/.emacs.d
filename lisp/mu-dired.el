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
         ("C-c f f"    . find-name-dired))
  :bind (:map dired-mode-map
              ("M-<up>"   . mu-dired-up)
              ("M-p"      . mu-dired-up)
              ("^"        . mu-dired-up)
              ("RET"      . find-file-reuse-dir-buffer)
              ("M-<down>" . mu-dired-down)
              ("M-n"      . mu-dired-down)
              ("!"        . mu-sudired))
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
  (add-hook 'dired-mode-hook #'toggle-truncate-lines)

  ;; Hide details
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package find-dired                 ; Run `find' in Dired
  :config (validate-setq find-ls-option '("-exec ls -ld {} \\+" . "-ld")))

(use-package dired-x                    ; Enable some nice Dired features
  :bind ("C-x C-j" . dired-jump)
  :config
  (validate-setq dired-omit-verbose nil ; Be less verbose, Dired
                 ;; Omit dotfiles with C-x M-o
                 dired-omit-files (concat dired-omit-files "^\\...+$")
                 dired-clean-confirm-killing-deleted-buffers nil)
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

(provide 'mu-dired)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-dired.el ends here
