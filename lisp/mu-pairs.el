;;; mu-smartparens.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my pairs balancing configuration.

;;; Code:

(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :hook ((clojure-mode             . smartparens-strict-mode)
         (emacs-lisp-mode          . smartparens-strict-mode)
         (inferior-emacs-lisp-mode . smartparens-strict-mode)
         (scheme-mode              . smartparens-strict-mode))
  :init
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  :config
  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-hybrid-kill-entire-symbol nil
        sp-message-width nil
        sp-autoskip-closing-pair 'always)

  (sp-with-modes sp--lisp-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil)))

(use-package smartparens-config         ; Configure Smartparens
  :ensure smartparens
  :after smartparens
  :bind (:map smartparens-mode-map
              ;; Movement and navigation
              ("C-M-f"       . sp-forward-sexp)
              ("C-M-b"       . sp-backward-sexp)
              ("C-M-u"       . sp-backward-up-sexp)
              ("C-M-d"       . sp-down-sexp)
              ("C-M-p"       . sp-backward-down-sexp)
              ("C-M-n"       . sp-up-sexp)
              ;; Deleting and killing
              ("C-M-k"       . sp-kill-sexp)
              ("C-M-w"       . sp-copy-sexp)
              ;; Depth changing
              ("M-r"         . sp-raise-sexp)
              ;; Wrap with
              ("M-("         . sp-wrap-round)
              ("C-è"         . sp-wrap-square)
              ("C-à"         . sp-wrap-curly)
              ("M-\""        . mu-sp-wrap-double-quote)
              ;; Barfage & Slurpage
              ("C-)"         . sp-forward-slurp-sexp)
              ("C-<right>"   . sp-forward-slurp-sexp)
              ("C-}"         . sp-forward-barf-sexp)
              ("C-<left>"    . sp-forward-barf-sexp)
              ("C-("         . sp-backward-slurp-sexp)
              ("C-M-<left>"  . sp-backward-slurp-sexp)
              ("C-{"         . sp-backward-barf-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ;; Miscellaneous commands
              ("M-S"         . sp-split-sexp)
              ("M-J"         . sp-join-sexp)
              ("C-M-t"       . sp-transpose-sexp))
  :bind (:map smartparens-strict-mode-map
              ("M-q" . sp-indent-defun))
  :config (defun mu-sp-wrap-double-quote ()
            "Wrap following sexp in double quotes."
            (interactive)
            (sp-wrap-with-pair "\"")))

(with-eval-after-load 'smartparens
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)
  (sp-local-pair 'clojure-mode "'" nil :actions nil)
  (sp-local-pair 'clojure-mode "`" nil :actions nil)
  (sp-local-pair 'scheme-mode "'" nil :actions nil)
  (sp-local-pair 'scheme-mode "`" nil :actions nil)
  (sp-local-pair 'inferior-scheme-mode "'" nil :actions nil)
  (sp-local-pair 'inferior-scheme-mode "`" nil :actions nil)

  (sp-local-pair 'LaTeX-mode "\"" nil :actions nil)
  (sp-local-pair 'LaTeX-mode "'" nil :actions nil)
  (sp-local-pair 'LaTeX-mode "`" nil :actions nil)
  (sp-local-pair 'latex-mode "\"" nil :actions nil)
  (sp-local-pair 'latex-mode "'" nil :actions nil)
  (sp-local-pair 'latex-mode "`" nil :actions nil)
  (sp-local-pair 'TeX-mode "\"" nil :actions nil)
  (sp-local-pair 'TeX-mode "'" nil :actions nil)
  (sp-local-pair 'TeX-mode "`" nil :actions nil)
  (sp-local-pair 'tex-mode "\"" nil :actions nil)
  (sp-local-pair 'tex-mode "'" nil :actions nil)
  (sp-local-pair 'tex-mode "`" nil :actions nil))

(use-package syntactic-close            ; Automatically insert closing delimiter
  :ensure t
  :bind ("C-c x c" . syntactic-close))

;; Look for unbalanced parens when saving
(add-hook 'after-save-hook 'check-parens nil t)

(provide 'mu-pairs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-pairs.el ends here
