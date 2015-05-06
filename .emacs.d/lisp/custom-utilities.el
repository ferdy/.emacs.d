;;; custom-utilities.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configurations for random utilities.

;;; Code:

(use-package info
  :defer t
  :bind ("C-h C-i" . info-lookup-symbol)
  :config
  ;; Fix `Info-quoted' face by going back to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-constant-face))

;; Let apropos commands perform more extensive searches than default
(setq apropos-do-all t)

(use-package calendar
  :defer t
  :config (setq calendar-week-start-day 1)); Start on Monday

(use-package time
  :bind (("C-c u i" . emacs-init-time)
         ("C-c u u" . emacs-uptime)
         ("C-c u t" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
        display-time-world-list '(("Europe/Rome" "Rome")
                                  ("Europe/London" "London")
                                  ("Asia/Hong_Kong" "Hong Kong")
                                  ("Asia/Tokyo" "Tokyo"))))

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
  :config (progn
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
  :commands esup)

(use-package writeroom-mode ; Distraction-free interface
  :ensure t
  :commands writeroom-mode)

(use-package speed-type ; Practice touch-typing
  :ensure t
  :commands speed-type-text)

;; Bury compilation buffer after successful compilation
(use-package bury-successful-compilation
  :ensure t
  :init (bury-successful-compilation 1))

(use-package command-log-mode ; Show event history and command history
  :ensure t
  :commands (command-log-mode))

;;; Bugs management
(use-package bug-reference
  :no-require t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
               (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package bug-hunter ; Find bugs in Emacs configuration
  :ensure t)

(provide 'custom-utilities)

;;; custom-utilities.el ends here
