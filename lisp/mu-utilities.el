;;; mu-utilities.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

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

(use-package info-colors                ; Extra colours for Info-mode
  :ensure t
  :config (add-hook 'Info-selection-hook #'info-colors-fontify-node))

(use-package helpful                    ; A better *help* buffer
  :ensure t
  :bind (("C-c h a" . helpful-at-point)
         ("C-c h b" . helpful-kill-buffers)
         ("C-c h m" . helpful-macro)
         ("C-c h s" . helpful-symbol)
         ("C-h f" . helpful-callable)
         ("C-h C" . helpful-command)
         ("C-h F" . helpful-function)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package calendar                   ; Display a calendar
  :config (setq calendar-week-start-day 1)) ; Start on Monday

(use-package time                       ; Display time
  :config
  (validate-setq
   display-time-world-time-format "%H:%M %Z, %d. %b"
   display-time-world-list '(("Europe/Rome" "Rome")
                             ("Europe/London" "London")
                             ("Asia/Hong_Kong" "Hong Kong")
                             ("Asia/Tokyo" "Tokyo"))))

(use-package proced                     ; Manage processes
  :defer t
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
  :defer t)

(use-package keychain-environment       ; Load keychain environment variables
  :ensure t
  :defer t)

(provide 'mu-utilities)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-utilities.el ends here
