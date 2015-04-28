;;; custom-formatting.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for formatting utilities.

;;; Code:

(use-package markdown-mode ; Edit markdown files
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("/itsalltext/.*" . gfm-mode))
  :config (progn
            ;; Use Pandoc to process Markdown
            (setq markdown-command "pandoc -s -f markdown -t html5")

            ;; No filling in GFM, because line breaks are significant.
            (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
            ;; Use visual lines instead
            (add-hook 'gfm-mode-hook #'visual-line-mode)))

(use-package pandoc-mode ; Easily control Pandoc in Emacs
  :ensure t
  :commands pandoc-mode
  :config (progn
            (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
            (setq org-pandoc-output-format 'odt)))

(provide 'custom-formatting)

;;; custom-formatting.el ends here
