;;; mu-latex.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my LaTeX configuration.

;;; Code:

(use-package tex-site ; Startup LaTeX mode
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode))

(use-package tex ; TeX editing
  :ensure auctex
  :defer t
  :config
  (progn
    (setq TeX-clean-confirm nil ; No confirmation when cleaning
          ;; Parse documents to provide completion
          TeX-parse-self t
          ;; Automatically save style information
          TeX-auto-save t
          ;; Insert braces after sub- and superscripts in math mode
          TeX-electric-sub-and-superscript t
          ;; Don't insert magic quotes right away
          TeX-quote-after-quote t
          ;; Provide forward and inverse search with SyncTeX
          TeX-source-correlate-mode t
          TeX-source-correlate-method 'synctex)

    (setq-default TeX-master nil     ; Ask for the master file
                  TeX-engine 'luatex ; Use luatex
                  TeX-PDF-mode t)

    ;; Move to chktex
    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")

    ;; Tell Emacs how to parse TeX files
    (add-hook 'tex-mode-hook
              #'(lambda () (setq ispell-parser 'tex)))

    ;; Update PDF buffers after successful LaTeX runs
    (defun mu-pdf-view-revert-buffer-maybe (file)
      (when-let ((buf (find-buffer-visiting file)))
        (with-current-buffer buf
          (when (derived-mode-p 'pdf-view-mode)
            (pdf-view-revert-buffer nil t)))))

    (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
              #'mu-pdf-view-revert-buffer-maybe)))

(use-package tex-buf ; External commands for AUCTeX
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style ; Customizable variables for AUCTeX style files
  :ensure auctex
  :defer t
  :config (setq LaTeX-csquotes-close-quote "}" ; Enable support for csquotes
                LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold ; Fold TeX macros
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode ; Major mode for TeX files
  :ensure auctex
  :defer t
  :config (font-lock-add-keywords 'latex-mode
                                  `((,(rx "\\"
                                          symbol-start
                                          "fx" (1+ (or (syntax word)
                                                       (syntax symbol)))
                                          symbol-end)
                                     . font-lock-warning-face))))

(use-package latex ; Support for LaTeX documents
  :ensure auctex
  :defer t
  :config
  (progn
    ;; No language-specific hyphens please
    (setq LaTeX-babel-hyphen nil
          LaTeX-command-style
          '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))) ; Easy math input

(use-package latex-extra ; Useful functionalities to LaTeX-mode
  :ensure t
  :defer t
  :config (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

(use-package auctex-latexmk ; Add LatexMk support to AUCTeX
  :ensure t
  :defer t
  :init (with-eval-after-load 'latex
          (auctex-latexmk-setup)))

(use-package bibtex ; Edit and validate BibTeX files
  :defer t
  :config
  (progn
    ;; Run prog mode hooks for bibtex
    (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
    ;; Use a modern BibTeX dialect
    (bibtex-set-dialect 'biblatex)))

(use-package reftex ; Minor mode for \label, \ref, \cite, \index in LaTeX
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
                               (?t    . "\\textcite{%l}")
                               (?a    . "\\autocite[]{%l}")
                               (?p    . "\\parencite{%l}")
                               (?f    . "\\footcite[][]{%l}")
                               (?F    . "\\fullcite[]{%l}")
                               (?x    . "[]{%l}")
                               (?X    . "{%l}"))))
      (setq reftex-cite-format 'biblatex)))
  :diminish reftex-mode)

(provide 'mu-latex)

;;; mu-latex.el ends here
