;;; mu-latex.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my LaTeX configuration.

;;; Code:

(use-package tex                        ; TeX editing
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config (setq TeX-auto-save t
                TeX-electric-math '("\\(" . "\\)")
                TeX-electric-sub-and-superscript t
                TeX-parse-self t
                TeX-quote-after-quote t
                TeX-source-correlate-method 'synctex
                TeX-source-correlate-mode t
                TeX-clean-confirm nil)

  (setq-default TeX-engine'luatex
                TeX-master nil)

  ;; Move to chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")

  ;; Tell Emacs how to parse TeX files
  (add-hook 'tex-mode-hook
            (lambda () (setq ispell-parser 'tex)))

  (setq TeX-source-correlate-start-server t
        TeX-view-program-selection '((output-pdf "PDF Tools")))

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package tex-buf                    ; External commands for AUCTeX
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style           ; Customizable variables for AUCTeX style files
  :ensure auctex
  :defer t
  :config (setq LaTeX-csquotes-close-quote "}"
                LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold                   ; Fold TeX macros
  :ensure auctex
  :hook (TeX-mode . TeX-fold-mode))

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
  ;; Teach TeX folding about KOMA script sections
  (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                            (,(rx (0+ space) "\\subsection*{") 3)
                            (,(rx (0+ space) "\\subsubsection*{") 4)
                            (,(rx (0+ space) "\\minisec{") 5))
        ;; No language-specific hyphens please
        LaTeX-babel-hyphen "")

  (setq LaTeX-command-style
        '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)) ; Easy math input

(use-package latex-extra                ; Useful functionalities to LaTeX-mode
  :ensure t
  :hook (LaTeX-mode . latex-extra-mode))

(use-package auctex-latexmk             ; Add LatexMk support to AUCTeX
  :ensure t
  :defer t
  :after latex
  :config (auctex-latexmk-setup))

(use-package bibtex                     ; Edit and validate BibTeX files
  :hook (bibtext-mode . (lambda () (run-hooks 'prog-mode-hook)))
  :config (bibtex-set-dialect 'biblatex))

(use-package reftex                     ; TeX/BibTeX cross-reference management
  :defer t
  :hook (LaTeX-mode . reftex-mode)
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

  (setq reftex-insert-label-flags '(t t)
        reftex-label-alist '(("definition" ?d "def:" "~\\ref{%s}"
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
                              "\\\\caption[[{]" ("algorithm" "alg") -3))))

(use-package latex-unicode-math-mode    ; Input method for Unicode math symbols
  :ensure t
  :hook (LaTeX-mode . latex-unicode-mode))

(provide 'mu-latex)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-latex.el ends here
