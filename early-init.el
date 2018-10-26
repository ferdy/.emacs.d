;;; early-init.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file is loaded before init.el.

;;; Code:

;; Package Settings
(customize-set-variable
 'package-archives '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
                     ("MELPA"    . "https://melpa.org/packages/")
                     ("ORG"      . "https://orgmode.org/elpa/")))

;; Allow more than 800Kb cache during init
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defun mu-set-gc-threshold ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' to their default values."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

;; Reset default values
(add-hook 'emacs-startup-hook #'mu-set-gc-threshold)

(provide 'early-init)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; early-init.el ends here
