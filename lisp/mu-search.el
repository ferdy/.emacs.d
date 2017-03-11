;;; mu-search.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
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

  (validate-setq
   ;; Scroll during search
   isearch-allow-scroll t
   ;; Fold unicode characters to ASCII while searching
   search-default-mode #'char-fold-to-regexp)

  (defun mu-isearch-exit-other-end ()
    "Exit isearch, at the opposite end of the string."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end)))

(use-package wgrep                      ; Editable grep buffer
  :ensure t
  :defer t
  :config)

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

(use-package ez-query-replace           ; Better query replace
  :ensure t
  :bind (([remap query-replace] . ez-query-replace)
         ("C-c M-%" . ez-query-replace-repeat)))

(provide 'mu-search)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-search.el ends here
