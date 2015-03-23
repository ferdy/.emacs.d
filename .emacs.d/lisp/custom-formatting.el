;;; custom-formatting.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for formatting utilities.

;;; Code:

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (progn
            ;; Use Pandoc to process Markdown
            (setq markdown-command "pandoc -s -f markdown -t html5")

            ;; Use visual-line-mode
            (add-hook 'markdown-mode-hook #'visual-line-mode)))

;; Requires: pandoc
(use-package pandoc-mode
  :ensure t
  :defer 5
  :config (progn
            (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
            (setq org-pandoc-output-format 'odt)))


(provide 'custom-formatting)

;;; custom-formatting.el ends here
