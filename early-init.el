;;; early-init.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file is loaded before init.el.

;;; Code:

(require 'package)

(setq package-archives
      '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")
        ("ORG"      . "https://orgmode.org/elpa/")))

(provide 'early-init)
;;; early-init.el ends here
