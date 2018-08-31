;;; mu-coq.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for Coq and Proof General.

;;; Code:

(use-package proof-general              ; Generic interface for proof assistants
  :ensure t
  :disabled t
  :mode  ("\\.v\\'" . coq-mode))

(use-package company-coq                ; Extensions for Proof General
  :ensure t
  :hook (coq-mode-hook . company-coq-mode)
  :config
  (validate-setq company-coq-features/prettify-symbols-in-terminals t)

  (setq-default proof-silence-compatibility-warning t
                proof-splash-enable nil
                proof-three-window-mode-policy 'hybrid
                coq-compile-parallel-in-background t)

  (put #'company-coq-fold 'disabled nil)

  (with-eval-after-load 'company-coq
    (setq-default company-coq-live-on-the-edge t
                  company-coq-dynamic-autocompletion t
                  company-coq-initial-fold-state 'bullets
                  company-coq-extra-symbols-cmd "SearchAbout -\"____\""
                  company-coq-features/prettify-symbols-in-terminal t)

    (bind-key "<f10>" #'coq-compile-before-require-toggle company-coq-map)
    (bind-key "C-c RET" #'company-coq-proof-goto-point company-coq-map)))

(provide 'mu-coq)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-coq.el ends here
