;;; mu-bugs.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for bugs management utilities.

;;; Code:

(use-package bug-reference              ; Buttonize bug references
  :no-require t
  :init
  (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
         (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package bug-hunter                 ; Find bugs in Emacs configuration
  :ensure t
  :commands bug-hunter-file)

(provide 'mu-bugs)

;;; mu-bugs.el ends here
