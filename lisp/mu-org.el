;;; mu-org.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my Org-mode configuration.

;;; Code:

(use-package org                        ; The almighty Org
  :ensure t
  :bind (("C-c o a" . org-agenda-list)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o f" . org-cycle-agenda-files)
         ("C-c o s" . org-search-view)
         ("C-c o t" . org-todo-list))
  :bind (:map org-mode-map
              ("<return>" . mu-org-return)
              ;; Prefer Isearch to Swiper in Org files
              ("C-s"      . isearch-forward)
              ("C-r"      . isearch-backward))
  :config
  (validate-setq org-emphasis-regexp-components ; Fix markup for ' and "
                 '("     ('\"{“”"
                   "-   .,!?;''“”\")}/\\“”"
                   "    \r\n,"
                   "."
                   1))
  (validate-setq org-src-fontify-natively t
                 org-log-done 'time
                 org-hide-emphasis-markers t
                 ;; Follow links by pressing ENTER on them
                 org-return-follows-link t)

  (validate-setq org-directory (expand-file-name "~/org/")
                 org-default-notes-file
                 (expand-file-name "organizer.org" org-directory))

  ;; Use visual-line-mode
  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; Use Org-mode for .eml files (useful for Thunderbird plugin)
  (add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))

  ;; Define TODO workflow states
  (validate-setq org-todo-keywords
                 '("TODO(t)" "WAITING(w)" "INFO(i)"
                   "|" "CANCELLED(c)" "DONE(x)"))

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

  (add-hook 'org-mode-hook #'mu-org-ispell)

  (defun mu-org-return ()
    "Disable `org-return-follows-link' if at bol or eol."
    (interactive)
    (let* ((follow org-return-follows-link)
           (org-return-follows-link (and follow (not (or (bolp) (eolp))))))
      (org-return)))

  ;; Free C-c $ (see: mu-languages.el)
  (unbind-key "C-c $" org-mode-map))

(use-package ox
  :ensure org
  :config
  (validate-setq org-export-with-smart-quotes t))

(use-package ox-html
  :ensure org
  :config
  ;; Turn off preamble and postamble in HTML export
  (validate-setq org-html-preamble nil
                 org-html-postamble nil))

(use-package ox-latex
  :ensure org
  :config
  (validate-setq org-latex-pdf-process         ; Use LuaTex for PDF export
                 '("latexmk -pdflatex='lualatex -shell-escape
-interaction nonstopmode' -pdf -f  %f")))

(use-package org-indent ; Dynamic indentation for Org-mode
  :ensure org
  :bind ("C-c t o" . org-indent-mode)
  :init (add-hook 'org-mode-hook #'org-indent-mode)
  :diminish org-indent-mode)

(use-package autoinsert                 ; Auto insert custom text
  :init
  (auto-insert-mode)
  (define-auto-insert '("\\.org\\'" . "Org skeleton")
    '("Short description: "
      "#+STARTUP: showall\n"
      > _ \n \n))
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
  :bind (("C-c o w" . org2blog/wp-new-entry)
         ("C-c o p" . org2blog/wp-post-buffer))
  :config
  (setq org2blog/wp-use-sourcecode-shortcode t
        org2blog/wp-sourcecode-langs
        '("bash" "javascript" "php" "text"
          "xml" "sh" "elisp" "lisp" "lua")
        org2blog/wp-blog-alist
        '(("filmsinwords"
           :url "https://filmsinwords.wordpress.com/xmlrpc.php"
           :username "manueluberti"))))

(use-package org2blog-autoloads         ; Autoloads from `ox-wp.el'
  :ensure org2blog
  :after org2blog)

;; ox-pandoc needs latest Pandoc.
;; If latest Pandoc is not available via apt-get,
;; install it from here: https://github.com/jgm/pandoc/releases/latest
(use-package ox-pandoc                  ; Export Org documents via Pandoc
  :ensure t
  :config
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
          `((css . ,(concat "file://" stylesheet))))))

(use-package ox-reveal                  ; Slideshows with Reveal.js
  :ensure t
  :config
  (setq org-reveal-root "file:///home/manuel/reveal.js"
        ;; Hide some controls
        org-reveal-control nil
        org-reveal-progress nil
        org-reveal-overview nil
        org-reveal-slide-number nil))

(use-package org-bullets                ; Bullets as UTF-8 characters
  :ensure t
  :init (add-hook 'org-mode-hook #'org-bullets-mode)
  :config
  (setq org-bullets-bullet-list
        '("◉" "◎" "●" "○" "►" "◇")))

(use-package org-pdfview                ; Link to PDF files
  :ensure t
  :after org)

(use-package interleave             ; Take notes in org files while reading PDFs
  :ensure t
  :bind ("C-c o i" . interleave))

(use-package ox-mediawiki               ; Export to mediawiki format
  :ensure t
  :after org)

;;; Utilities and keybindings
(bind-key "<f5>"                        ; Open organizer file
          (lambda () (interactive) (find-file "~/org/organizer.org")))

(provide 'mu-org)

;;; mu-org.el ends here
