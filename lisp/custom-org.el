;;; custom-org.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for everything Org mode related.

;;; Code:

(use-package org ; The almighty Org
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
  :config (progn
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

            (with-eval-after-load "org"
              (add-hook 'org-add-hook 'custom/modify-org-done-face))

            ;; Define TODO workflow states and different faces
            (setq org-todo-keywords
                  '("TODO(t)" "INVIO DOCS(i)" "STAMPE(s)" "FATTURE(f)"
                    "PHONE(p)" "MEETING(m)" "INFORMAZIONI(n)" "PREVENTIVO(v)"
                    "RINVIO LEZIONI(r)" "|" "CANCELLED(c)" "DONE(x)"))

            ;; Define custom commands
            (setq org-agenda-custom-commands
                  '(("P" "Project List" ((tags "PROJECT")))
                    ("B" "Boccaperta" ((agenda)
                                       (tags-todo "BOCCAPERTA")))
                    ("F" "FAV" ((agenda)
                                (tags-todo "FAV")))))

            ;; Embed Youtube videos
            (org-add-link-type
             "yt"
             (lambda (handle)
               (browse-url (concat "https://www.youtube.com/embed/" handle)))
             (lambda (path desc backend)
               (cl-case backend
                 (html (format "<iframe width=\"440\" height=\"335\"
src=\"https://www.youtube.com/embed/%s\" frameborder=\"0\"
allowfullscreen>%s</iframe>"
                               path (or desc "")))
                 (latex (format "\href{%s}{%s}" path (or desc "video"))))))

            (setq org-latex-pdf-process ; Use LuaTex for PDF export
                  "latexmk -pdflatex='lualatex -shell-escape
-interaction nonstopmode' -pdf -f  %f")

            ;; Surround current symbol with emphasize markers
            (defun org-emphasize-dwim (&optional char)
              (interactive)
              (unless (region-active-p)
                (backward-word)
                (mark-word))
              (org-emphasize char))

            (bind-key [remap org-emphasize] #'org-emphasize-dwim
                      org-mode-map)

            ;; Disable whitespace highlighting of overlong lines in Org Mode
            (add-hook 'org-mode-hook
                      #'custom/whitespace-style-no-long-lines)

            ;; Use F12 to toggle image visualization
            (bind-key "<f12>"
                      (lambda () (interactive) (org-toggle-inline-images t))
                      org-mode-map)

            (defun custom/org-ispell ()
              "Configure `ispell-skip-region-alist' for `org-mode'."
              (make-local-variable 'ispell-skip-region-alist)
              (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
              (add-to-list 'ispell-skip-region-alist '("~" "~"))
              (add-to-list 'ispell-skip-region-alist '("=" "="))
              (add-to-list 'ispell-skip-region-alist
                           '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

            (add-hook 'org-mode-hook #'custom/org-ispell)))

(use-package autoinsert ; Auto insert custom text
  :init (progn
          (auto-insert-mode)
          (define-auto-insert '("\\.org\\'" . "Org skeleton")
            '("Short description: "
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
  :bind (("C-c a o w" . org2blog/wp-new-entry)
         ("C-c a o p" . org2blog/wp-post-buffer))
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
  :config (setq org-reveal-root "file:///home/manuel/reveal.js"
                ;; Hide some controls
                org-reveal-control nil
                org-reveal-progress nil
                org-reveal-overview nil
                org-reveal-slide-number nil))

(use-package org-bullets ; Bullets as UTF-8 characters
  :ensure t
  :init (add-hook 'org-mode-hook #'org-bullets-mode)
  :config (setq org-bullets-bullet-list
                '("◉" "○" "●" "▶")))

(use-package org-pdfview ; Link to PDF files
  :ensure t
  :init (with-eval-after-load 'org '(require org-pdfview)))

(use-package interleave ; Take notes in org files while reading PDFs
  :ensure t
  :defer t)

;;; Utilities and keybindings
(bind-key "<f5>" ; Open organizer file
          (lambda () (interactive) (find-file "~/org/organizer.org")))

(provide 'custom-org)

;;; custom-org.el ends here
