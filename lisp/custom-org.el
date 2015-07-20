;;; custom-org.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for everything Org mode related.

;;; Code:

(use-package org ; The almighty Org
  :ensure t
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :defer t
  :init (setq org-emphasis-regexp-components ; Fix markup for ' and "
              '("     ('\"{“”"
                "-   .,!?;''“”\")}/\\“”"
                "    \r\n,"
                "."
                1))
  :config (progn
            (setq org-src-fontify-natively t
                  org-log-done 'time
                  org-export-with-smart-quotes t
                  ;; Turn off preamble and postamble in HTML export
                  org-html-preamble nil
                  org-html-postamble nil
                  org-export-html-style-default ""
                  org-export-html-style-include-default nil
                  org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
                  org-default-notes-file "~/org/organizer.org"
                  org-agenda-start-on-weekday nil
                  org-agenda-include-diary t
                  org-agenda-use-time-grid t)

            ;; Use visual-line-mode
            (add-hook 'org-mode-hook #'visual-line-mode)

            (defun myorg-update-parent-cookie ()
              "Update parent nodes when child is removed."
              (when (equal major-mode 'org-mode)
                (save-excursion
                  (ignore-errors
                    (org-back-to-heading)
                    (org-update-parent-todo-statistics)))))

            (defadvice org-kill-line (after fix-cookies activate)
              "Update parent node."
              (myorg-update-parent-cookie))

            (defadvice kill-whole-line (after fix-cookies activate)
              "Update parent node."
              (myorg-update-parent-cookie))

            (bind-key "\"" #'custom/round-quotes org-mode-map)

            (defun custom/round-quotes (italicize)
              "Insert “” and leave point in the middle.
With prefix argument ITALICIZE, insert /“”/ instead (meant for
org-mode).
If inside a code-block, simply calls `self-insert-command'."
              (interactive "P")
              (if (and (derived-mode-p 'org-mode) (org-in-src-block-p))
                  (call-interactively 'self-insert-command)
                (if (looking-at "”[/=_\\*]?")
                    (goto-char (match-end 0))
                  (when italicize
                    (insert "//")
                    (forward-char -1))
                  (insert "“”")
                  (forward-char -1))))

            (bind-key "'" #'custom/apostrophe org-mode-map)

            (defun custom/apostrophe (opening)
              "Insert ’ in prose or `self-insert-command' in code.
With prefix argument OPENING, insert ‘’ instead and leave
point in the middle.
Inside a code-block, simply calls `self-insert-command'."
              (interactive "P")
              (if (and (derived-mode-p 'org-mode)
                       (org-in-block-p '("src" "latex" "html")))
                  (call-interactively #'self-insert-command)
                (if (looking-at "['’][=_/\\*]?")
                    (goto-char (match-end 0))
                  (if (null opening)
                      (insert "’")
                    (insert "‘’")
                    (forward-char -1)))))

            ;; Use Org-mode for .eml files (useful for Thunderbird plugin)
            (add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))

            ;; Strike out DONE items
            (defun custom/modify-org-done-face ()
              (setq org-fontify-done-headline t)
              (set-face-attribute 'org-done nil :strike-through t)
              (set-face-attribute 'org-headline-done nil
                                  :strike-through t
                                  :foreground "light gray"))

            (eval-after-load "org"
              (add-hook 'org-add-hook 'custom/modify-org-done-face))

            ;; Defines TODO workflow states and different faces
            (setq org-todo-keywords
                  '("TODO(t)" "INVIO DOCS(i)" "STAMPE(s)" "FATTURE(f)"
                    "PHONE(p)" "MEETING(m)" "INFORMAZIONI(n)" "PREVENTIVO(v)"
                    "RINVIO LEZIONI(r)" "|" "CANCELLED(c)" "DONE(x)"))))

(use-package autoinsert ; Auto insert custom text
  :init (progn
          (auto-insert-mode)
          (define-auto-insert '("\\.org\\'" . "Org skeleton")
            '(
              "Short description: "
              "#+STARTUP: showall\n"
              > _ \n \n)))
  :config (setq auto-insert-query nil))

(use-package metaweblog ; Access metaweblog based weblogs
  :ensure t
  :defer t)

(use-package xml-rpc ; Clientside XML-RPC
  :ensure t
  :defer t)

(use-package htmlize ; Convert buffer text and decorations to HTML
  :ensure t
  :defer t)

(use-package org2blog ; Blog from Org mode to Wordpress
  :ensure t
  :defer t
  :bind ("C-c w o" . org2blog/wp-new-entry)
  :init (use-package org2blog-autoloads)
  :config (progn
            (setq org2blog/wp-use-sourcecode-shortcode t
                  org2blog/wp-sourcecode-langs
                  '("bash" "javascript" "php" "text"
                    "xml" "sh" "elisp" "lisp" "lua")
                  org2blog/wp-blog-alist
                  '(("informatica.boccaperta.com"

                     :url "http://informatica.boccaperta.com/xmlrpc.php"
                     :username "manuel")))))

(use-package toc-org ; Table of contents for Org files
  :ensure t
  :defer t
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package ox-pandoc ; Export Org documents via Pandoc
  :ensure t
  :config (progn
            (setq org-pandoc-options '((standalone . t)) ; default options
                  ;; special settings for beamer-pdf and latex-pdf exporters
                  org-pandoc-options-for-beamer-pdf
                  '((latex-engine . "lualatex"))
                  org-pandoc-options-for-latex-pdf
                  '((latex-engine . "lualatex")))

            ;; Use external css for html5
            (let ((stylesheet (expand-file-name
                               (locate-user-emacs-file "etc/pandoc.css"))))
              (setq org-pandoc-options-for-html5
                    `((css . ,(concat "file://" stylesheet)))))))

(use-package ox-reveal ; Slideshows with Reveal.js
  :ensure t
  :config (setq org-reveal-root "file:///home/manuel/reveal.js"))

(use-package org-bullets ; Bullets as UTF-8 characters
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config (setq org-bullets-bullet-list
                '("◉" "○" "●" "▶")))

(provide 'custom-org)

;;; custom-org.el ends here
