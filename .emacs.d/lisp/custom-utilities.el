;;; custom-utilities.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configurations for random utilities.

;;; Code:

(use-package pdf-tools ; Better PDF support
  :defer t
  :init (pdf-tools-install))

(use-package interleave ; Take notes in org files while reading PDFs
  :ensure t
  :defer t)

(use-package emacsshot ; Take a screenshot from within Emacs
  :ensure t
  :bind (("<print>" . emacsshot-snap-frame))
  :no-require t)

(use-package camcorder ; Record movements from within Emacs
  :ensure t
  :no-require t
  :init (setq camcorder-window-id-offset -2))

(use-package archive-mode
  :defer t
  :mode ("\\.\\(cbr\\)\\'" . archive-mode)) ; Enable .cbr support

(use-package proced ; Manage processes
  :defer t
  :config
  (progn
    ;; Auto-update proced buffer
    (defun proced-settings ()
      (proced-toggle-auto-update 1))

    (add-hook 'proced-mode-hook 'proced-settings)))

(use-package csv-mode ; Better .csv files editing
  :ensure t
  :no-require t
  :mode "\\.csv\\'")

(use-package lice ; License and header template
  :ensure t
  :no-require t)

(use-package unkillable-scratch ; Unkillable Scratch buffer
  :ensure t
  :defer t
  :init (unkillable-scratch 1))

(use-package esup ; Emacs Startup Profiler
  :ensure t
  :commands esup
  :disabled t)

(use-package bury-successful-compilation ; Bury compilation buffer after
  :ensure t                              ; successful compilation
  :init (bury-successful-compilation 1))

;;; Bugs management
(use-package bug-reference
  :no-require t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
               (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package bug-hunter ; Find bugs in Emacs configuration
  :ensure t)

(provide 'custom-utilities)

;;; custom-utilities.el ends here
