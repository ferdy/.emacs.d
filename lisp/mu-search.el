;;; mu-search.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores searching customizations and utilities.

;;; Code:

;; Ignore directories during grep
(with-eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))

;; Truncate lines during grep
(add-hook 'grep-mode-hook #'toggle-truncate-lines)

(use-package "isearch"                  ; Search buffers
  ;; Defer because `isearch' is not a feature and we don't want to `require' it
  :defer t
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature.  For the
  ;; same reason we have to use `:init', but isearch is always loaded anyways.
  :init
  (progn
    (diminish 'isearch-mode)

    (setq isearch-allow-scroll t        ; Scroll during search
          ;; Use character-folding in query-replace
          replace-character-fold t)))

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :init (global-anzu-mode)
  :diminish anzu-mode)

(use-package wgrep                      ; Editable grep buffer
  :ensure t
  :defer t)

(use-package wgrep-ag                   ; Wgrep for ag
  :ensure t
  :defer t)

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package replace-pairs              ; Query-replace pairs of things
  :ensure t
  :bind (("C-c s q" . query-replace-pairs)
         ("C-c s p" . replace-pairs)))

(use-package ag                         ; Search code in files/projects
  :ensure t
  :bind (("C-c s a" . ag)
         ("C-c s d" . ag-dired-regexp)
         ("C-c s D" . ag-dired)
         ("C-c s f" . ag-files)
         ("C-c s k" . ag-kill-other-buffers)
         ("C-c s K" . ag-kill-buffers))
  :config
  (setq ag-reuse-buffers t            ; Don't spam buffer list with ag buffers
        ag-highlight-search t         ; Highlight results
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                          (projectile-project-root)))))

(provide 'mu-search)

;;; mu-search.el ends here
