;;; custom-org.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for everything org-mode related.

;;; Code:

;;; Org
(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :defer t
  :config
  (progn
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

    ;; Update parent nodes when child is removed
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

    (define-key org-mode-map "\"" #'custom/round-quotes)

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

    (define-key org-mode-map "'" #'custom/apostrophe)

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

    ;; Strike out DONE items
    (defun custom/modify-org-done-face ()
      (setq org-fontify-done-headline t)
      (set-face-attribute 'org-done nil :strike-through t)
      (set-face-attribute 'org-headline-done nil
                          :strike-through t
                          :foreground "light gray"))

    (eval-after-load "org"
      (add-hook 'org-add-hook 'custom/modify-org-done-face))))

;; Auto insert custom text upon opening an org file
(auto-insert-mode)
(setq auto-insert-query nil)
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.org\\'" . "Org skeleton")
     '(
       "Short description: "
       "#+STARTUP: showall\n"
       > _ \n \n)))

;; Org2blog
(use-package metaweblog
  :ensure t
  :defer t)

(use-package xml-rpc
  :ensure t
  :defer t)

(use-package htmlize
  :ensure t
  :defer t)

(use-package org2blog
  :ensure t
  :defer t
  :init (require 'org2blog-autoloads)
  :config (progn
            (setq org2blog/wp-use-sourcecode-shortcode t
                  org2blog/wp-sourcecode-langs
                  '("bash" "javascript" "php" "text"
                    "xml" "sh" "elisp" "lisp" "lua")
                  org2blog/wp-blog-alist
                  '(("informatica.boccaperta.com"

                     :url "http://informatica.boccaperta.com/xmlrpc.php"
                     :username "manuel")))))

(use-package org-tree-slide ; Slides via org-mode
  :ensure t
  :no-require t
  :config
  (progn
    (define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)
    (define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
    (define-key org-tree-slide-mode-map (kbd "<f9>")
      'org-tree-slide-move-previous-tree)
    (define-key org-tree-slide-mode-map (kbd "<f10>")
      'org-tree-slide-move-next-tree)
    (define-key org-tree-slide-mode-map (kbd "<f11>")
      'org-tree-slide-content)

    (setq org-tree-slide-skip-outline-level 4)
    (org-tree-slide-narrowing-control-profile)
    (setq org-tree-slide-skip-done nil)))

(provide 'custom-org)

;;; custom-org.el ends here
