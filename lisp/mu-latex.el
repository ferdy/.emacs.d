;;; mu-latex.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my LaTeX configuration.

;;; Code:

(use-package tex                        ; TeX editing
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (validate-setq
   TeX-clean-confirm nil         ; No confirmation when cleaning
   TeX-parse-self t              ; Parse documents to provide completion
   TeX-auto-save t               ; Automatically save style information
   ;; Insert braces after sub- and superscripts in math mode
   TeX-electric-sub-and-superscript t
   TeX-electric-math '("\\(" . "\\)")
   ;; Don't insert magic quotes right away
   TeX-quote-after-quote t
   ;; Provide forward and inverse search with SyncTeX
   TeX-source-correlate-mode t
   TeX-source-correlate-method 'synctex)

  (setq-default
   ;; Ask for the master file
   TeX-master nil
   ;; Use luatex
   TeX-engine 'luatex)

  ;; Move to chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")

  ;; Tell Emacs how to parse TeX files
  (add-hook 'tex-mode-hook
            (lambda () (setq ispell-parser 'tex)))

  (validate-setq
   ;; Use pdf-tools to open PDF files
   TeX-view-program-selection '((output-pdf "PDF Tools"))
   TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-function
            #'TeX-revert-document-buffer))

(use-package tex-buf                    ; External commands for AUCTeX
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (validate-setq TeX-save-query nil))

(use-package tex-style           ; Customizable variables for AUCTeX style files
  :ensure auctex
  :defer t
  :config
  (validate-setq
   ;; Enable support for csquotes
   LaTeX-csquotes-close-quote "}"
   LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; Fold TeX macros
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode                   ; Major mode for TeX files
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
                          `((,(rx "\\"
                                  symbol-start
                                  "fx" (1+ (or (syntax word)
                                               (syntax symbol)))
                                  symbol-end)
                             . font-lock-warning-face))))

(use-package latex                      ; LaTeX editing
  :ensure auctex
  :defer t
  :config
  (validate-setq
   ;; Teach TeX folding about KOMA script sections
   TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                       (,(rx (0+ space) "\\subsection*{") 3)
                       (,(rx (0+ space) "\\subsubsection*{") 4)
                       (,(rx (0+ space) "\\minisec{") 5))
   ;; No language-specific hyphens please
   LaTeX-babel-hyphen "")

  (validate-setq LaTeX-command-style
                 '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)) ; Easy math input

(use-package latex-extra                ; Useful functionalities to LaTeX-mode
  :ensure t
  :defer t
  :config (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

(use-package auctex-latexmk             ; Add LatexMk support to AUCTeX
  :ensure t
  :defer t
  :after latex
  :config (auctex-latexmk-setup))

(use-package bibtex                     ; Edit and validate BibTeX files
  :defer t
  :config
  ;; Run prog mode hooks for bibtex
  (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  ;; Use a modern BibTeX dialect
  (bibtex-set-dialect 'biblatex))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (defun mu-reftex-find-ams-environment-caption (environment)
    "Find the caption of an AMS ENVIRONMENT."
    (let ((re (rx-to-string `(and "\\begin{" ,environment "}"))))
      ;; Go to the beginning of the label first
      (re-search-backward re)
      (goto-char (match-end 0)))
    (if (not (looking-at (rx (zero-or-more space) "[")))
        (error "Environment %s has no title" environment)
      (let ((beg (match-end 0)))
        ;; Move point onto the title start bracket and move to the end, skipping
        ;; any other brackets in between, and eventually extract the text
        ;; between the brackets
        (goto-char (1- beg))
        (forward-list)
        (buffer-substring-no-properties beg (1- (point))))))

  (validate-setq
   reftex-plug-into-AUCTeX t ; Plug into AUCTeX
   ;; Automatically derive labels, and prompt for confirmation
   reftex-insert-label-flags '(t t)
   reftex-label-alist
   '(
     ;; Additional label definitions for RefTeX.
     ("definition" ?d "def:" "~\\ref{%s}"
      mu-reftex-find-ams-environment-caption
      ("definition" "def.") -3)
     ("theorem" ?h "thm:" "~\\ref{%s}"
      mu-reftex-find-ams-environment-caption
      ("theorem" "th.") -3)
     ("example" ?x "ex:" "~\\ref{%s}"
      mu-reftex-find-ams-environment-caption
      ("example" "ex") -3)
     ;; Algorithms package
     ("algorithm" ?a "alg:" "~\\ref{%s}"
      "\\\\caption[[{]" ("algorithm" "alg") -3)))
  :diminish reftex-mode)

(use-package latex-unicode-math-mode    ; Input method for Unicode math symbols
  :ensure t
  :defer t
  :init (add-hook 'LaTeX-mode-hook 'latex-unicode-mode)
  :diminish latex-unicode-mode)

(provide 'mu-latex)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-latex.el ends here
