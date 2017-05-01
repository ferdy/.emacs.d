;;; mu-smartparens.el --- Part of my Emacs Setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my pairs balancing configuration.

;;; Code:

(use-package lispy                      ; vi-like Paredit
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  clojure-mode-hook
                  lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook (lambda () (lispy-mode 1))))

  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy))

;; Look for unbalanced parens when saving
(add-hook 'after-save-hook 'check-parens nil t)

(provide 'mu-pairs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-pairs.el ends here
