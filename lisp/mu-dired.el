;;; mu-dired.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Dired.

;;; Code:

(use-package dired                      ; File manager
  :defer t
  :bind (("C-c f s"    . dired-get-size)
         ("<C-return>" . mu-open-in-external-app)
         ("C-c f f"    . find-name-dired))
  :config
  (progn
    (setq dired-auto-revert-buffer t    ; Revert buffers on revisiting
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

    ;; Reuse buffers if they are directories
    (defun find-file-reuse-dir-buffer ()
      "Like `dired-find-file', but reuse Dired buffers."
      (interactive)
      (set-buffer-modified-p nil)
      (let ((file (dired-get-file-for-visit)))
        (if (file-directory-p file)
            (find-alternate-file file)
          (find-file file))))

    ;; Better keybinding for moving between directories
    (bind-keys :map dired-mode-map
               ("M-<up>"   . (lambda ()
                               (interactive)
                               (find-alternate-file "..")))
               ("M-p"      . (lambda ()
                               (interactive)
                               (find-alternate-file "..")))
               ("^"        . (lambda ()
                               (interactive)
                               (find-alternate-file "..")))
               ("RET"      . find-file-reuse-dir-buffer)
               ("M-<down>" . (lambda ()
                               (interactive)
                               (dired-find-alternate-file)))
               ("M-n"      . (lambda ()
                               (interactive)
                               (dired-find-alternate-file))))

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
      (require 'tramp)
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

(use-package dired-x                    ; Enable some nice dired features
  :bind ("C-x C-j" . dired-jump)
  :config
  (progn
    (setq dired-omit-verbose nil        ; Be less verbose, Dired
          ;; Omit dotfiles with C-x M-o
          dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
    (add-hook 'dired-mode-hook #'dired-omit-mode)

    ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
    ;; a very peculiar way of registering its lighter explicitly in
    ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
    ;; isn't there yet after dired-omit-mode is loaded.
    (add-function :after (symbol-function 'dired-omit-startup)
                  (lambda () (diminish 'dired-omit-mode))
                  '((name . dired-omit-mode-diminish)))))

;;; Utilities and keybindings
;;;###autoload
(defun mu-open-in-external-app ()
  "Open the file where point is or the marked files in Dired in external
app. The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list
          (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (let ((process-connection-type nil))
         (start-process "" nil "xdg-open" file-path))) file-list)))

(provide 'mu-dired)

;;; mu-dired.el ends here
