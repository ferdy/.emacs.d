;;; mu-search.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

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

(use-package "isearch" ; Search buffers
  ;; Defer because `isearch' is not a feature and we don't want to `require' it
  :defer t
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature.  For the
  ;; same reason we have to use `:init', but isearch is always loaded anyways.
  :init
  (progn
    (diminish 'isearch-mode)
    ;; Scroll during search
    (setq isearch-allow-scroll t)

    ;; Better backspace during isearch
    (defun mu-isearch-delete ()
      "Delete non-matching text or the last character."
      (interactive)
      (if (= 0 (length isearch-string))
          (ding)
        (setq isearch-string
              (substring isearch-string
                         0
                         (or (isearch-fail-pos)
                             (1- (length isearch-string)))))
        (setq isearch-message
              (mapconcat #'isearch-text-char-description
                         isearch-string "")))
      (if isearch-other-end (goto-char isearch-other-end))
      (isearch-search)
      (isearch-push-state)
      (isearch-update))

    (bind-key [remap isearch-delete-char] #'mu-isearch-delete)))

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

(use-package visual-regexp ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package ag ; Search code in files/projects
  :ensure t
  :bind (("C-c s a" . ag)
         ("C-c s d" . ag-dired-regexp)
         ("C-c s D" . ag-dired)
         ("C-c s f" . ag-files)
         ("C-c s k" . ag-kill-other-buffers)
         ("C-c s K" . ag-kill-buffers))
  :config
  (setq ag-reuse-buffers t ; Don't spam buffer list with ag buffers
        ag-highlight-search t ; Highlight results
        ;; Use Projectile to find the project root
        ag-project-root-function (lambda (d) (let ((default-directory d))
                                               (projectile-project-root)))))

(provide 'mu-search)

;;; mu-search.el ends here
