;;; mu-whitespace.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This files store my configuration for white spaces handling.

;;; Code:

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :defer t
  :config (add-hook 'after-init-hook #'global-whitespace-cleanup-mode))

(use-package hungry-delete              ; Delete all whitespaces
  :ensure t
  :bind (("C-c <backspace>" . hungry-delete-backward)
         ("C-c <deletechar>" . hungry-delete-forward)))

(setq-default show-trailing-whitespace t)

(defun mu-no-trailing-whitespace ()
  "Turn off display of trailing whitespace in the current buffer."
  (setq show-trailing-whitespace nil))

(dolist (hook '(calendar-mode
                cider-repl-mode-hook
                comint-mode-hook
                compilation-mode-hook
                eww-mode-hook
                minibuffer-setup-hook
                special-mode-hook
                term-mode-hook
                Info-mode-hook))
  (add-hook hook #'mu-no-trailing-whitespace))

(provide 'mu-whitespace)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-whitespace.el ends here
