;;; mu-completion.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for autocompletion tools.

;;; Code:

(use-package yasnippet                  ; Snippets
  :ensure t
  :bind ("C-c y" . mu-yasnippet/body)
  :config
  (validate-setq yas-verbosity 1        ; No need to be so verbose
                 yas-wrap-around-region t)

  (yas-reload-all)
  (add-hook 'text-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)

  (defhydra mu-yasnippet (:hint nil)
    "
YASnippets (quit with _q_)
^Modes^      ^Load/Visit^     ^Actions^
^-----^------^----------^-----^-------^--------
_g_: global  _d_: directory   _i_: insert
_m_: minor   _f_: file        _t_: tryout
_e_: extra   _l_: list        _n_: new
         _a_ll
"
    ("q" nil)
    ("d" yas-load-directory)
    ("e" yas-activate-extra-mode)
    ("i" yas-insert-snippet)
    ("f" yas-visit-snippet-file)
    ("n" yas-new-snippet)
    ("t" yas-tryout-snippet)
    ("l" yas-describe-tables)
    ("g" yas-global-mode)
    ("m" yas-minor-mode)
    ("a" yas-reload-all))
  :diminish yas-minor-mode)

(use-package abbrev                     ; Save abbreviations
  :init (abbrev-mode)
  :config (validate-setq save-abbrevs t)
  :diminish abbrev-mode)

;; In `completion-at-point', do not pop up completion buffers for less
;; than five candidates. Cycle instead.
(validate-setq completion-cycle-threshold 5)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (validate-setq hippie-expand-try-functions-list
                 '(try-expand-dabbrev
                   try-expand-dabbrev-all-buffers
                   try-expand-dabbrev-from-kill
                   try-complete-file-name-partially
                   try-complete-file-name
                   try-expand-all-abbrevs
                   try-expand-list
                   try-complete-lisp-symbol-partially
                   try-complete-lisp-symbol)))

(use-package pcomplete-extension        ; Enhance completion in (e)shell
  :ensure t
  :after eshell)

(use-package company                    ; Auto-completion
  :ensure t
  :init (global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (validate-setq company-tooltip-align-annotations t
                 company-tooltip-flip-when-above t
                 ;; Easy navigation to candidates with M-<n>
                 company-show-numbers t)
  :diminish company-mode)

(use-package company-statistics         ; Show likelier candidates on top
  :ensure t
  :after company
  :config (company-statistics-mode))

(use-package company-quickhelp          ; Show help in tooltip
  :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package company-math               ; Backend for math symbols
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package company-web                ; Backend for web development
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-web-html))

(use-package company-auctex             ; Backend for AUCTeX
  :ensure t
  :init
  (add-hook 'company-mode-hook #'yas-minor-mode)
  (company-auctex-init))

(use-package sly-company                ; Backend for Sly
  :ensure t
  :init (add-hook 'sly-mode-hook 'sly-company-mode))

(use-package company-shell              ; Company support for shell functions
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-shell))

(use-package company-anaconda           ; Python backend for Company
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package company-restclient         ; Company support for restclient
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-restclient))

(provide 'mu-completion)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-completion.el ends here
