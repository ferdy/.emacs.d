;;; custom-completion.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for everything completion related.

;;; Code:

(use-package yasnippet ; Snippets
  :ensure t
  :init (yas-global-mode)
  :config (setq yas-verbosity 1 ; No need to be so verbose
                yas-wrap-around-region t)
  :diminish (yas-minor-mode . " Ⓨ"))

(use-package abbrev ; Save abbreviations
  :init (abbrev-mode)
  :config (setq save-abbrevs t)
  :diminish abbrev-mode)

;; In `completion-at-point', do not pop up completion buffers for less
;; than five candidates. Cycle instead.
(setq completion-cycle-threshold 5)

(use-package hippie-exp ; Powerful expansion and completion
  :bind ([remap dabbrev-expand] . hippie-expand)
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
          try-complete-lisp-symbol)))

(use-package pcomplete-extension ; Enhanced completion in (e)shell buffers
  :ensure t
  :init (with-eval-after-load 'eshell '(require pcomplete-extension)))

(use-package company ; Auto-completion
  :ensure t
  :init (global-company-mode)
  :config
  (progn
    (setq company-tooltip-align-annotations t
          company-tooltip-flip-when-above t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)

    ;; Don't complete numbers
    (push (apply-partially #'cl-remove-if
                           (lambda (c)
                             (or (string-match-p "[^\x00-\x7F]+" c)
                                 (string-match-p "[0-9]+" c)
                                 (if (equal major-mode "org")
                                     (>= (length c) 15)))))
          company-transformers))
  :diminish company-mode)

(use-package company-statistics ; Show likelier candidates on top
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (company-statistics-mode)))

(use-package company-quickhelp ; Show help in tooltip
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (company-quickhelp-mode)))

(use-package company-math ; Backend for math symbols
  :ensure t
  :init (with-eval-after-load 'company
          ;; Add backends for math characters
          (add-to-list 'company-backends 'company-math-symbols-unicode)
          (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package company-web ; Backend for web development
  :ensure t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-web-html)))

(use-package company-auctex ; Backend for AUCTeX
  :ensure t
  :init
  (progn
    (add-hook 'company-mode-hook #'yas-minor-mode)
    (company-auctex-init)))

(use-package sly-company ; Backend for Sly
  :ensure t
  :init (add-hook 'sly-mode-hook 'sly-company-mode))

(provide 'custom-completion)

;;; custom-completion.el ends here
