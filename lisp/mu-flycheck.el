;;; mu-flycheck.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti <manuel dot uberti at inventati dot org>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Flycheck and related extensions.

;;; Code:

;;; Syntax checking
;; Requires: chktex
(use-package flycheck                   ; On-the-fly syntax checker
  :ensure t
  :bind ("C-c t e" . flycheck-mode)
  :init
  (defun mu-flycheck-set-load-path-for-user-configuration ()
    "Set Flycheck load path for files in user configuration."
    (when (and (buffer-file-name)
               (flycheck-in-user-emacs-directory-p (buffer-file-name)))
      (setq-local flycheck-emacs-lisp-load-path
                  (cons (locate-user-emacs-file "lisp/")
                        flycheck-emacs-lisp-load-path))))

  (defun mu-discard-undesired-html-tidy-error (err)
    "Discard ERR if it is undesired."
    ;; A non-nil result means to inhibit further processing (i.e. highlighting)
    ;; of the error
    (and (eq (flycheck-error-checker err) 'html-tidy)
         ;; Only allow warnings about missing tags, or unexpected end tags being
         ;; discarded
         (not (string-match-p (rx (or "missing" "discarding"))
                              (flycheck-error-message err)))))

  ;; Don't highlight undesired errors from html tidy
  (add-hook 'flycheck-process-error-functions
            #'mu-discard-undesired-html-tidy-error)
  (add-hook 'flycheck-mode-hook
            #'mu-flycheck-set-load-path-for-user-configuration)

  (global-flycheck-mode))

(use-package flycheck-package          ; Check package conventions with Flycheck
  :ensure t
  :after flycheck
  :config (flycheck-package-setup))

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package flycheck-vale              ; Flycheck setup for Vale
  :ensure t
  :after flycheck
  :config (flycheck-vale-setup))

(provide 'mu-flycheck)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-flycheck.el ends here
