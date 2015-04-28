;;; custom-search.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores searching customizations.

;;; Code:

(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

(use-package wgrep
  :ensure t
  :defer t)

(use-package visual-regexp ; Display regular expressions
  :ensure t
  :defer t
  :bind (("C-c r" . vr/query-replace)
         ("C-c R" . vr/replace)))

(use-package anzu ; Show search results number in mode-line
  :ensure t
  :init (global-anzu-mode)
  :config (progn
            (setq anzu-cons-mode-line-p nil
                  anzu-mode-lighter "")
            (setcar (cdr (assq 'isearch-mode minor-mode-alist))
                    '(:eval (anzu--update-mode-line)))))

(use-package ace-jump-mode ; Jump to characters in buffers
  :ensure t
  :bind (("C-c j" . ace-jump-mode)
         ("C-c J" . ace-jump-mode-pop-mark))
  :config (ace-jump-mode-enable-mark-sync)) ; Sync marks with built-in commands

(provide 'custom-search)

;;; custom-search.el ends here
