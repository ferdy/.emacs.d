;;; mu-whitespace.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This files store my configuration for white spaces handling.

;;; Code:

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :hook (after-init-hook . global-whitespace-cleanup-mode))

(use-package shrink-whitespace          ; Better whitespace removal
  :ensure t
  :bind ("M-SPC" . shrink-whitespace))

(use-package hungry-delete              ; Delete all whitespaces
  :ensure t
  :bind (("C-c <backspace>" . hungry-delete-backward)
         ("C-c <deletechar>" . hungry-delete-forward)))

(setq-default show-trailing-whitespace t)

(defun mu-no-trailing-whitespace ()
  "Turn off display of trailing whitespace in the current buffer."
  (setq show-trailing-whitespace nil))

(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'mu-no-trailing-whitespace))

(provide 'mu-whitespace)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-whitespace.el ends here
