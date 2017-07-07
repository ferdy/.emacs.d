;;; mu-org.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my Org-mode configuration.

;;; Code:

(use-package org                        ; The almighty Org
  :ensure t
  :bind (("C-c o a" . org-agenda-list)
         ("C-c o c" . org-capture)
         ("C-c o i" . mu-insert-checkbox)
         ("C-c o l" . org-store-link)
         ("C-c o f" . org-cycle-agenda-files)
         ("C-c o s" . org-search-view)
         ("C-c o t" . org-todo-list)
         :map org-mode-map
         ("RET" . mu-org-return))
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

  ;; Use Org-mode for .eml files (useful for Thunderbird plugin)
  (add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))

  ;; Use Org structures and tables in message mode
  (add-hook 'message-mode-hook #'turn-on-orgtbl)
  (add-hook 'message-mode-hook #'turn-on-orgstruct++)

  ;; Define TODO workflow states
  (validate-setq
   org-todo-keywords '("TODO(t)" "WAITING(w)" "|" "CANCELLED(c)" "DONE(d)"))

  ;; Define Agenda files for GTD
  (validate-setq
   org-agenda-files '("~/org/gtd/gtd.org"
                      "~/org/gtd/inbox.org"
                      "~/org/gtd/tickler.org"))

  ;; Define refile targets for GTD
  (setq
   org-refile-targets '(("~/org/gtd/gtd.org" :maxlevel . 3)
                        ("~/org/gtd/someday.org" :level . 1)
                        ("~/org/gtd/tickler.org" :maxlevel . 2)))
  (setq
   org-agenda-custom-commands
   '(("o" "At the office" tags-todo "@office"
      ((org-agenda-overriding-header "Office")
       (org-agenda-skip-function
        #'mu-org-agenda-skip-all-siblings-but-first)))))

  (defun mu-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))

  ;; Disable whitespace highlighting of overlong lines in Org Mode
  (add-hook 'org-mode-hook #'mu-whitespace-style-no-long-lines)

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
  (unbind-key "C-c $" org-mode-map)

  (defun mu-insert-checkbox ()
    "Insert a bullet point with a checkbox."
    (interactive)
    (insert "- [ ] ")))

(use-package org-capture                ; Fast note taking in Org
  :config
  (setq
   org-capture-templates '(("t" "Todo [inbox]" entry
                            (file+headline "~/org/gtd/inbox.org" "Tasks")
                            "* TODO %i%?")
                           ("T" "Tickler" entry
                            (file+headline "~/org/gtd/tickler.org" "Tickler")
                            "* %i%? \n %U"))))

(use-package ox
  :ensure org
  :config (validate-setq org-export-with-smart-quotes t))

(use-package ox-html
  :ensure org
  :config
  ;; Turn off preamble and postamble in HTML export
  (validate-setq org-html-preamble nil
                 org-html-postamble nil))

(use-package ox-latex
  :ensure org
  :config
  ;; Use LuaTex for PDF export
  (validate-setq org-latex-pdf-process
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
  :config (validate-setq auto-insert-query nil))

;; ox-pandoc needs latest Pandoc.
;; If latest Pandoc is not available via apt-get,
;; install it from here: https://github.com/jgm/pandoc/releases/latest
(use-package ox-pandoc                  ; Export Org documents via Pandoc
  :ensure t
  :config
  (validate-setq org-pandoc-options '((standalone . t)) ; Default options
                 ;; Special settings for beamer-pdf and latex-pdf exporters
                 org-pandoc-options-for-beamer-pdf
                 '((latex-engine . "lualatex"))
                 org-pandoc-options-for-latex-pdf
                 '((latex-engine . "lualatex")))

  ;; Use external css for html5
  (let ((stylesheet (expand-file-name
                     (locate-user-emacs-file "etc/pandoc.css"))))
    (validate-setq org-pandoc-options-for-html5
                   `((css . ,(concat "file://" stylesheet))))))

(use-package ox-reveal                  ; Slideshows with Reveal.js
  :ensure t
  :config
  (validate-setq org-reveal-root "file:///home/manuel/reveal.js"
                 ;; Hide some controls
                 org-reveal-control ""
                 org-reveal-progress nil
                 org-reveal-overview nil
                 org-reveal-slide-number ""))

(use-package org-bullets                ; Bullets as UTF-8 characters
  :ensure t
  :init (add-hook 'org-mode-hook #'org-bullets-mode)
  :config
  (validate-setq org-bullets-bullet-list
                 '("◉" "○" "●" "►" "◇" "◎")))

(use-package org-pdfview                ; Link to PDF files
  :ensure t
  :after org)

;;; Utilities and keybindings
(bind-key "<f5>"                        ; Open organizer file
          (lambda () (interactive) (find-file "~/org/organizer.org")))

(provide 'mu-org)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-org.el ends here
