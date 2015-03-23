;;; custom-completion.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for everything completion related.

;;; Code:

;; In `completion-at-point', do not pop up silly completion buffers for less
;; than five candidates. Cycle instead.
(setq completion-cycle-threshold 5)

(use-package hippie-exp ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          lunaryorn-try-complete-lisp-symbol-without-namespace)))

(use-package company
  :ensure t
  :defer t
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)

    (global-company-mode)))

(use-package company-math
  :ensure t
  :defer t
  :config
  (progn
    ;; local configuration for TeX modes
    (defun my-latex-mode-setup ()
      "Add company-math backends."
      (setq-local company-backends
                  (append '(company-math-symbols-latex
                            company-math-symbols-unicode
                            company-latex-commands)
                          company-backends)))
    (add-hook 'TeX-mode-hook 'my-latex-mode-setup)))

(provide 'custom-completion)

;;; custom-completion.el ends here
