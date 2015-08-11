;;; custom-search.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores searching customizations.

;;; Code:

;; Ignore directories during grep
(with-eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))

;; Truncate lines during grep
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

(use-package isearch ; Search buffers
  :defer t
  :config (setq isearch-allow-scroll t))

(use-package anzu ; Position/matches count for isearch
  :ensure t
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :init (global-anzu-mode)
  :diminish anzu-mode)

(use-package wgrep ; Editable grep buffer
  :ensure t
  :defer t)

(use-package wgrep-ag ; Wgrep for ag
  :ensure t
  :defer t)

(use-package visual-regexp ; Display regular expressions
  :ensure t
  :bind (("C-c r" . vr/query-replace)
         ("C-c R" . vr/replace)))

(use-package ag ; Search code in files/projects
  :ensure t
  :bind (("C-c M-s"     . ag)
         ("C-x M-s"     . ag-files)
         ("C-c C-x M-s" . ag-project))
  :config
  (setq ag-reuse-buffers t ; Don't spam buffer list with ag buffers
        ag-highlight-search t ; Highlight results
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

(provide 'custom-search)

;;; custom-search.el ends here
