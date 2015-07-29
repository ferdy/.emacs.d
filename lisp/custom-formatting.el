;;; custom-formatting.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for formatting utilities.

;;; Code:

(use-package rst ; ReStructuredText
  :defer t
  :config (progn
            ;; Indent with 3 spaces after all kinds of literal blocks
            (setq rst-indent-literal-minimized 3
                  rst-indent-literal-normal 3)

            (bind-key "C-=" nil rst-mode-map)
            ;; For similarity with AUCTeX and Markdown
            (bind-key "C-c C-j" #'rst-insert-list rst-mode-map)
            (bind-key "M-RET" #'rst-insert-list rst-mode-map)))

(use-package markdown-mode ; Edit markdown files
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (progn
    ;; Process Markdown with Pandoc, using a custom stylesheet for nice output
    (let ((stylesheet (expand-file-name
                       (locate-user-emacs-file "etc/pandoc.css"))))
      (setq markdown-command
            (mapconcat #'shell-quote-argument
                       `("pandoc" "--toc" "--section-divs"
                         "--css" ,(concat "file://" stylesheet)
                         "--standalone" "-f" "markdown" "-t" "html5")
                       " ")))

    ;; No filling in GFM, because line breaks are significant.
    (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
    ;; Use visual lines instead
    (add-hook 'gfm-mode-hook #'visual-line-mode)
    (add-hook 'gfm-mode-hook #'lunaryorn-whitespace-style-no-long-lines)

    (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)

    ;; Fight my habit of constantly pressing M-q.  We should not fill in GFM
    ;; Mode.
    (bind-key "M-q" #'ignore gfm-mode-map)))

(use-package pandoc-mode ; Easily control Pandoc in Emacs
  :ensure t
  :commands pandoc-mode
  :config (progn
            (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
            (setq org-pandoc-output-format 'odt)))

(provide 'custom-formatting)

;;; custom-formatting.el ends here
