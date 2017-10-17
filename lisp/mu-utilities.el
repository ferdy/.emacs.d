;;; mu-utilities.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for random utilities.

;;; Code:

(use-package info                       ; Info, the documentation browser
  :bind ("C-h C-i" . info-lookup-symbol)
  :config
  ;; Fix `Info-quoted' face by going back to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
                      :inherit font-lock-constant-face))

(use-package helpful                    ; A better *help* buffer
  :ensure t
  :bind (("C-c h a" . helpful-at-point)
         ("C-c h C" . helpful-callable)
         ("C-c h c" . helpful-command)
         ("C-c h f" . helpful-function)
         ("C-c h k" . helpful-key)
         ("C-c h m" . helpful-macro)
         ("C-c h s" . helpful-symbol)
         ("C-c h v" . helpful-variable)))

(use-package calendar                   ; Display a calendar
  :bind ("C-c a t c" . calendar)
  :config (setq calendar-week-start-day 1)) ; Start on Monday

(use-package time                       ; Display time
  :bind ("C-c a t t" . display-time-world)
  :config (validate-setq
           display-time-world-time-format "%H:%M %Z, %d. %b"
           display-time-world-list '(("Europe/Rome" "Rome")
                                     ("Europe/London" "London")
                                     ("Asia/Hong_Kong" "Hong Kong")
                                     ("Asia/Tokyo" "Tokyo"))))

(use-package calc                       ; Calculator
  :bind (("C-c a m q" . quick-calc)
         ("C-c a m c" . calc)))

(use-package proced                     ; Manage processes
  :bind ("C-c a a p" . proced)
  :config
  ;; Auto-update proced buffer
  (defun proced-settings ()
    (proced-toggle-auto-update 1))
  (add-hook 'proced-mode-hook 'proced-settings))

(use-package pandoc-mode                ; Easily control Pandoc in Emacs
  :ensure t
  :bind ("C-c t P" . pandoc-mode)
  :config (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(use-package list-environment           ; List process environment variables
  :ensure t
  :bind ("C-c a a l" . list-environment))

(use-package keychain-environment       ; Load keychain environment variables
  :ensure t
  :bind ("C-c a a s" . keychain-environment-refresh))

(provide 'mu-utilities)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-utilities.el ends here
