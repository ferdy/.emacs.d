;;; mu-whitespace.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This files store my configuration for white spaces handling.

;;; Code:

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :config (add-hook 'after-init-hook #'global-whitespace-cleanup-mode))

(use-package hungry-delete              ; Delete all whitespaces
  :ensure t
  :bind (("C-c <backspace>" . hungry-delete-backward)
         ("C-c <deletechar>" . hungry-delete-forward)))

(provide 'mu-whitespace)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-whitespace.el ends here
