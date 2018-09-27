;;; mu-flycheck.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for Flycheck and related extensions.

;;; Code:

;;; Syntax checking
;; Requires: chktex
(use-package flycheck                   ; On-the-fly syntax checker
  :ensure t
  :bind (("C-c t e" . flycheck-mode)
         ("C-c e"   . hydra-flycheck/body))
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
  
  (defhydra hydra-flycheck (:color blue)
    "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
    ("q" nil)
    ("<" flycheck-previous-error :color pink)
    (">" flycheck-next-error :color pink)
    ("?" flycheck-describe-checker)
    ("M" flycheck-manual)
    ("d" flycheck-disable-checker)
    ("f" flycheck-buffer)
    ("l" flycheck-list-errors)
    ("m" flycheck-mode)
    ("s" flycheck-select-checker)
    ("v" flycheck-verify-setup))

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
  :hook (flycheck-mode . flycheck-vale-setup)
  :config (validate-setq flycheck-vale-modes
                         '(text-mode org-mode markdown-mode rst-mode)))

(provide 'mu-flycheck)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-flycheck.el ends here
