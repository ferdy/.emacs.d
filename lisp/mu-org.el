;;; mu-org.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my Org-mode configuration.

;;; Code:

(use-package org                        ; The almighty Org
  :ensure t
  :bind (("C-c a o a" . org-agenda-list)
         ("C-c a o c" . org-capture)
         ("C-c a o l" . org-store-link)
         ("C-c a o f" . org-cycle-agenda-files)
         ("C-c a o s" . org-search-view)
         ("C-c a o t" . org-todo-list))
  :init (setq org-emphasis-regexp-components ; Fix markup for ' and "
              '("     ('\"{“”"
                "-   .,!?;''“”\")}/\\“”"
                "    \r\n,"
                "."
                1))
  :config
  (progn
    (setq org-src-fontify-natively t
          org-log-done 'time
          org-export-with-smart-quotes t
          org-hide-emphasis-markers t
          ;; Turn off preamble and postamble in HTML export
          org-html-preamble nil
          org-html-postamble nil
          org-export-html-style-default ""
          org-export-html-style-include-default nil
          org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
          org-agenda-start-on-weekday nil
          org-agenda-include-diary t
          org-agenda-use-time-grid t
          ;; Follow links by pressing ENTER on them
          org-return-follows-link t)

    (setq org-directory (expand-file-name "~/org/")
          org-default-notes-file
          (expand-file-name "organizer.org" org-directory))

    ;; Use visual-line-mode
    (add-hook 'org-mode-hook #'visual-line-mode)

    (defun mu-org-update-parent ()
      "Update parent nodes when child is removed."
      (when (equal major-mode 'org-mode)
        (save-excursion
          (ignore-errors
            (org-back-to-heading)
            (org-update-parent-todo-statistics)))))

    (defadvice org-kill-line (after fix-cookies activate)
      "Update parent node."
      (mu-org-update-parent))

    (defadvice kill-whole-line (after fix-cookies activate)
      "Update parent node."
      (mu-org-update-parent))

    ;; Use Org-mode for .eml files (useful for Thunderbird plugin)
    (add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))

    ;; Define TODO workflow states
    (setq org-todo-keywords
          '("TODO(t)" "WAITING(w)" "INFO(i)"
            "|" "CANCELLED(c)" "DONE(x)"))

    ;; Define custom commands
    (setq org-agenda-custom-commands
          '(("P" "Personal Projects" ((Tags "PERSONAL")))
            ("B" "Boccaperta" ((agenda)
                               (tags-todo "BOCCAPERTA")))
            ("F" "FAV" ((agenda)
                        (tags-todo "FAV")))))

    (setq org-latex-pdf-process         ; Use LuaTex for PDF export
          "latexmk -pdflatex='lualatex -shell-escape
-interaction nonstopmode' -pdf -f  %f")

    ;; Disable whitespace highlighting of overlong lines in Org Mode
    (add-hook 'org-mode-hook
              #'mu-whitespace-style-no-long-lines)

    ;; Use F12 to toggle image visualization
    (bind-key "<f12>"
              (lambda () (interactive) (org-toggle-inline-images t))
              org-mode-map)

    (defun mu-org-ispell ()
      "Configure `ispell-skip-region-alist' for `org-mode'."
      (make-local-variable 'ispell-skip-region-alist)
      (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
      (add-to-list 'ispell-skip-region-alist '("~" "~"))
      (add-to-list 'ispell-skip-region-alist '("=" "="))
      (add-to-list 'ispell-skip-region-alist
                   '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

    (add-hook 'org-mode-hook #'mu-org-ispell)))

(use-package org-indent ; Dynamic indentation for Org-mode
  :ensure org
  :bind ("C-c t o" . org-indent-mode)
  :init (add-hook 'org-mode-hook #'org-indent-mode)
  :diminish org-indent-mode)

(use-package autoinsert                 ; Auto insert custom text
  :init
  (progn
    (auto-insert-mode)
    (define-auto-insert '("\\.org\\'" . "Org skeleton")
      '("Short description: "
        "#+STARTUP: showall\n"
        > _ \n \n)))
  :config (setq auto-insert-query nil))

(use-package metaweblog                 ; Access metaweblog based weblogs
  :ensure t
  :defer t)

(use-package xml-rpc                    ; Clientside XML-RPC
  :ensure t
  :defer t)

(use-package htmlize               ; Convert buffer text and decorations to HTML
  :ensure t
  :defer t)

(use-package org2blog                   ; Blog from Org mode to Wordpress
  :ensure t
  :bind (("C-c a o w" . org2blog/wp-new-entry)
         ("C-c a o p" . org2blog/wp-post-buffer))
  :config
  (progn
    (setq org2blog/wp-use-sourcecode-shortcode t
          org2blog/wp-sourcecode-langs
          '("bash" "javascript" "php" "text"
            "xml" "sh" "elisp" "lisp" "lua")
          org2blog/wp-blog-alist
          '(("informatica.boccaperta.com"
             :url "http://informatica.boccaperta.com/xmlrpc.php"
             :username "manuel")
            ("filmsinwords"
             :url "https://filmsinwords.wordpress.com/xmlrpc.php"
             :username "manueluberti")))))

(use-package org2blog-autoloads         ; Autoloads from `ox-wp.el'
  :ensure org2blog
  :after org2blog)

;; ox-pandoc needs latest Pandoc.
;; If latest Pandoc is not available via apt-get,
;; install it from here: https://github.com/jgm/pandoc/releases/latest
(use-package ox-pandoc                  ; Export Org documents via Pandoc
  :ensure t
  :config
  (progn
    (setq org-pandoc-options '((standalone . t)) ; Default options
          ;; Special settings for beamer-pdf and latex-pdf exporters
          org-pandoc-options-for-beamer-pdf
          '((latex-engine . "lualatex"))
          org-pandoc-options-for-latex-pdf
          '((latex-engine . "lualatex")))

    ;; Use external css for html5
    (let ((stylesheet (expand-file-name
                       (locate-user-emacs-file "etc/pandoc.css"))))
      (setq org-pandoc-options-for-html5
            `((css . ,(concat "file://" stylesheet)))))))

(use-package ox-reveal                  ; Slideshows with Reveal.js
  :ensure t
  :config (setq org-reveal-root "file:///home/manuel/reveal.js"
                ;; Hide some controls
                org-reveal-control nil
                org-reveal-progress nil
                org-reveal-overview nil
                org-reveal-slide-number nil))

(use-package org-bullets                ; Bullets as UTF-8 characters
  :ensure t
  :init (add-hook 'org-mode-hook #'org-bullets-mode)
  :config (setq org-bullets-bullet-list
                '("◉" "◎" "●" "○" "►" "◇")))

(use-package org-pdfview                ; Link to PDF files
  :ensure t
  :after org)

(use-package interleave             ; Take notes in org files while reading PDFs
  :ensure t
  :bind ("C-c a o i" . interleave))

(use-package ox-mediawiki               ; Export to mediawiki format
  :ensure t
  :after org)

;;; Utilities and keybindings
(bind-key "<f5>"                        ; Open organizer file
          (lambda () (interactive) (find-file "~/org/organizer.org")))

(provide 'mu-org)

;;; mu-org.el ends here
