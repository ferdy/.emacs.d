;;; custom-latex.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my LaTeX configuration.

;;; Code:

;;; LaTeX
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode))

;; TeX editing
(use-package tex
  :ensure auctex
  :defer t
  :config (progn
            (setq TeX-parse-self t ; Parse documents to provide completion
                  TeX-auto-save t ; Automatically save style information
                  TeX-electric-sub-and-superscript t ; Insert braces after
                                        ; sub- and superscripts in math mode
                  TeX-quote-after-quote t ; Don't insert magic quotes right away
                  TeX-clean-confirm nil ; No confirmation when cleaning
                  TeX-source-correlate-mode t ; Search with SyncTeX
                  TeX-source-correlate-method 'synctex)

            (setq-default TeX-master nil ; Ask for the master file
                          TeX-engine 'luatex ; Use luatex
                          TeX-PDF-mode t)

            ;; Move to chktex
            (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")

            ;; Tell Emacs how to parse TeX files
            (add-hook 'tex-mode-hook
                      #'(lambda () (setq ispell-parser 'tex)))))

(use-package tex-buf
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style
  :ensure auctex
  :defer t
  :config (setq LaTeX-csquotes-close-quote "}" ; Enable support for csquotes
                LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode
  :ensure auctex
  :defer t
  :config (font-lock-add-keywords 'latex-mode
                                  `((,(rx "\\"
                                          symbol-start
                                          "fx" (1+ (or (syntax word)
                                                       (syntax symbol)))
                                          symbol-end)
                                     . font-lock-warning-face))))

(use-package latex
  :ensure auctex
  :defer t
  :config (progn
            ;; No language-specific hyphens please
            (setq LaTeX-babel-hyphen nil
                  LaTeX-command-style
                  '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

            (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))) ; Easy math input

(use-package latex-extra
  :ensure t
  :defer t
  :config (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

(use-package auctex-latexmk
  :ensure t
  :defer t
  :init (with-eval-after-load 'latex
          (auctex-latexmk-setup)))

(use-package bibtex
  :defer t
  :config (progn
            ;; Run prog mode hooks for bibtex
            (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
            ;; Use a modern BibTeX dialect
            (bibtex-set-dialect 'biblatex)))

(use-package reftex
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (progn
    ;; Plug into AUCTeX
    (setq reftex-plug-into-AUCTeX t
          ;; Automatically derive labels, and prompt for confirmation
          reftex-insert-label-flags '(t t))

    ;; Provide basic RefTeX support for biblatex
    (unless (assq 'biblatex reftex-cite-format-builtin)
      (add-to-list 'reftex-cite-format-builtin
                   '(biblatex "The biblatex package"
                              ((?\C-m . "\\cite[]{%l}")
                               (?t . "\\textcite{%l}")
                               (?a . "\\autocite[]{%l}")
                               (?p . "\\parencite{%l}")
                               (?f . "\\footcite[][]{%l}")
                               (?F . "\\fullcite[]{%l}")
                               (?x . "[]{%l}")
                               (?X . "{%l}"))))
      (setq reftex-cite-format 'biblatex)))
  :diminish reftex-mode)

(provide 'custom-latex)

;;; custom-latex.el ends here
