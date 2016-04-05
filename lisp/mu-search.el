;;; mu-search.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

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
  :bind (:map isearch-mode-map
              ("<C-return>" . mu-isearch-exit-other-end))
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature.  For the
  ;; same reason we have to use `:init', but isearch is always loaded anyways.
  :init
  (diminish 'isearch-mode)

  (setq isearch-allow-scroll t        ; Scroll during search
        ;; Use character-folding in query-replace
        replace-character-fold t)

  (defun mu-isearch-exit-other-end ()
    "Exit isearch, at the opposite end of the string."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end)))

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
                                          (projectile-project-root))))
  ;; Set ag arguments
  (setq ag-arguments (list "--smart-case" "--nogroup" "--column")
        ag-arguments (append '("--follow") ag-arguments)))

(use-package wgrep                      ; Editable grep buffer
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'grep
    (bind-key "C-x C-q" #'wgrep-change-to-wgrep-mode grep-mode-map))

  (with-eval-after-load 'wgrep
    (bind-key "C-c C-c" #'wgrep-finish-edit grep-mode-map)))

(use-package wgrep-ag                   ; Wgrep for ag
  :ensure t
  :commands (wgrep-ag-setup)
  :config
  (add-hook 'ag-mode-hook #'wgrep-ag-setup)

  (bind-key "C-x s" #'wgrep-save-all-buffers wgrep-mode-map)

  (with-eval-after-load 'ag
    (bind-key "C-x C-q" #'wgrep-change-to-wgrep-mode ag-mode-map))

  (with-eval-after-load 'wgrep
    (bind-key "C-c C-c" #'wgrep-finish-edit ag-mode-map)))

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package replace-pairs              ; Query-replace pairs of things
  :ensure t
  :bind (("C-c s q" . query-replace-pairs)
         ("C-c s p" . replace-pairs)))

(provide 'mu-search)

;;; mu-search.el ends here
