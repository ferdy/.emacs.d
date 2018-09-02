;;; mu-org.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my Org-mode configuration.

;;; Code:

(use-package org                        ; The almighty Org
  :ensure t
  :bind (("C-c o b" . mu-insert-checkbox)
         ("C-c o l" . org-store-link)
         :map org-mode-map
         ("RET" . mu-org-return))
  :config
  (validate-setq
   org-src-fontify-natively t
   org-edit-timestamp-down-means-later t
   org-catch-invisible-edits 'show
   org-log-done 'time
   org-hide-emphasis-markers t
   org-highlight-latex-and-related '(latex)
   org-return-follows-link t
   org-directory (expand-file-name "~/org/")
   org-default-notes-file (expand-file-name "gtd/gtd.org" org-directory))

  ;; Use Org-mode for .eml files (useful for Thunderbird plugin)
  (add-to-list 'auto-mode-alist '("\\.eml\\'" . org-mode))

  ;; Use Org structures and tables in message mode
  (add-hook 'message-mode-hook #'turn-on-orgtbl)
  (add-hook 'message-mode-hook #'turn-on-orgstruct++)

  ;; Define TODO workflow states
  (validate-setq
   org-todo-keywords '("TODO(t)" "WAITING(w)" "|" "CANCELLED(c)" "DONE(d)"))

  ;; Define Agenda files for GTD
  (validate-setq org-agenda-files '("~/org/gtd/gtd.org"
                                    "~/org/gtd/gym.org"
                                    "~/org/gtd/inbox.org"
                                    "~/org/gtd/tickler.org"))

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


  (unbind-key "C-c $" org-mode-map)      ; Free C-c $ (see: mu-languages.el)
  (unbind-key "C-'" org-mode-map)        ; Free C-' (see: mu-editing.el)
  (unbind-key "S-<return>" org-mode-map) ; Free S-RET (see: mu-editing.el)

  (defun mu-insert-checkbox ()
    "Insert a bullet point with a checkbox."
    (interactive)
    (insert "- [ ] ")))

(use-package org-agenda                 ; Dynamic task and appointment lists
  :after org
  :bind (("C-c o a" . org-agenda-list)
         ("C-c o s" . org-search-view)
         ("C-c o t" . org-todo-list))
  :config
  (validate-setq org-agenda-restore-windows-after-quit t)

  ;; Use a single full frame for org-agenda
  (with-eval-after-load 'org-agenda
    (fullframe org-agenda-list mu-pop-window-configuration))

  ;; Re-align tags when window shape changes
  (with-eval-after-load 'org-agenda
    (add-hook 'org-agenda-mode-hook
              (lambda () (add-hook 'window-configuration-change-hook
                              'org-agenda-align-tags nil t)))))

(use-package org-capture                ; Fast note taking
  :after org
  :bind ("C-c o c" . org-capture)
  :config
  (setq
   org-capture-templates '(("t" "Todo [inbox]" entry
                            (file+headline "~/org/gtd/inbox.org" "Tasks")
                            "* TODO %i%?")
                           ("T" "Tickler" entry
                            (file+headline "~/org/gtd/tickler.org" "Tickler")
                            "* %i%? \n %^t"))))

(use-package org-faces                  ; Faces definitions
  :after org
  :config
  ;; Force title font size to override theme setting
  (set-face-attribute 'org-document-title nil :height 1.0))

(use-package ox
  :ensure org
  :config
  (validate-setq
   org-export-with-smart-quotes t
   org-export-coding-system 'utf8))

(use-package ox-html
  :ensure org
  :config
  (validate-setq
   ;; Turn off preamble and postamble in HTML export
   org-html-preamble nil
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
  :init (add-hook 'org-mode-hook #'org-indent-mode))

(use-package ox-pandoc                  ; Export Org documents via Pandoc
  :ensure t
  :config
  ;; Use external css for html5
  (let ((stylesheet (expand-file-name
                     (locate-user-emacs-file "etc/pandoc.css"))))
    (validate-setq org-pandoc-options-for-html5
                   `((css . ,(concat "file://" stylesheet))))))

(use-package org-bullets                ; Bullets as UTF-8 characters
  :ensure t
  :init (add-hook 'org-mode-hook #'org-bullets-mode)
  :config (validate-setq org-bullets-bullet-list '("◉" "○" "●" "►" "◇" "◎")))

(use-package org-pdfview                ; Link to PDF files
  :ensure t
  :after org)

(use-package org-cliplink               ; Insert links from the clipboard
  :ensure t
  :bind ("C-c o i" . org-cliplink))

;;; Utilities and key bindings
(bind-key "<f6>"                        ; Open organizer file
          (lambda ()
            (interactive)
            (find-file "~/org/gtd/gtd.org")))

(provide 'mu-org)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-org.el ends here
