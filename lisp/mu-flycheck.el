;;; mu-flycheck.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Flycheck and related extensions.

;;; Code:

;;; Syntax checking
;; Requires: chktex
(use-package flycheck                   ; On-the-fly syntax checker
  :ensure t
  :bind (("C-c e"   . mu-flycheck-errors/body)
         ("C-c t e" . flycheck-mode))
  :init
  (defhydra mu-flycheck-errors ()
    "Flycheck errors."
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")
    ("f" flycheck-first-error "first")
    ("l" flycheck-list-errors "list")
    ("w" flycheck-copy-errors-as-kill "copy message"))

  (defun mu-flycheck-set-load-path-for-user-configuration ()
    "Set Flycheck load path for files in user configuration."
    (when (and (buffer-file-name)
               (flycheck-in-user-emacs-directory-p (buffer-file-name)))
      (setq-local flycheck-emacs-lisp-load-path
                  (cons (locate-user-emacs-file "lisp/")
                        flycheck-emacs-lisp-load-path))))

  (defun mu-discard-undesired-html-tidy-error (err)
    "Discard ERR if it is undesired.

Tidy is very verbose, so we prevent Flycheck from highlighting
most errors from HTML Tidy."
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

  ;; Enable Flycheck for programming buffers only
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (validate-setq flycheck-standard-error-navigation nil
                 flycheck-display-errors-function
                 #'flycheck-display-error-messages-unless-error-list))

(use-package flycheck-package          ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :after flycheck
  :config (flycheck-package-setup))

(use-package flycheck-cask              ; Setup Flycheck by Cask projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-clojure           ; Backend for Clojure
  :ensure t
  :defer t
  :after flycheck
  :init (flycheck-clojure-setup))

(use-package flycheck-rust              ; Flycheck setup for Rust
  :ensure t
  :defer t
  :after rust-mode
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'mu-flycheck)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-flycheck.el ends here
