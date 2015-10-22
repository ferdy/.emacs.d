;;; mu-media.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for media utilities.

;;; Code:

(use-package camcorder ; Record movements from within Emacs
  :ensure t
  :bind ("C-c t c" . camcorder-mode)
  :config (setq camcorder-output-directory "~/videos")
  :diminish (camcorder-mode ". ⓒ"))

(use-package volume ; Control audio volume from Emacs
  :ensure t
  :defer t)

(use-package bongo ; Play music with Emacs
  :ensure t
  :bind (("C-c a b" . bongo)
         ("C-c f b" . mu-bongo-add-from-dired))
  ;; Don't use the mode line
  :init (setq bongo-display-playback-mode-indicator nil
              bongo-mode-line-indicator-parent nil)
  :config
  (progn
    (defun mu-bongo-add-from-dired ()
      "Add marked files to Bongo library"
      (interactive)
      (let (file-point file (files nil))
        (dired-map-over-marks
         (setq file-point (dired-move-to-filename)
               file (dired-get-filename)
               files (append files (list file)))
         nil t)
        (save-excursion
          (set-buffer bongo-default-library-buffer-name)
          (mapc 'bongo-insert-file files))))))

(provide 'mu-media)

;;; mu-media.el ends here
