;;; mu-highlight.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for highlighting utilities.

;;; Code:

(use-package paren                      ; Highlight paired delimiters
  :init (show-paren-mode)
  :config
  (defun show-paren-clear-highlight ()
    "Turn off any previous paren highlighting."
    (delete-overlay show-paren--overlay)
    (delete-overlay show-paren--overlay-1))

  (defun mu-show-paren-update-on-insert ()
    ;; A command with `delete-selection' property probably inserts text.
    (if (get this-command 'delete-selection)
        (show-paren-function)
      (show-paren-clear-highlight)))

  (add-hook 'post-command-hook #'mu-show-paren-update-on-insert))

(use-package diff-hl                    ; Show changes in fringe
  :ensure t
  :defer 10
  :init
  ;; Highlight changes to the current file in the fringe
  (global-diff-hl-mode)
  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

(use-package symbol-overlay             ; Highlight symbols
  :ensure t
  :bind (:map symbol-overlay-mode-map
              ("M-h" . symbol-overlay-put)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev))
  :init
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)

  (dolist (hook '(html-mode-hook css-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook #'symbol-overlay-mode)))

(use-package hl-todo                    ; Highlight TODO and similar keywords
  :ensure t
  :init (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package rainbow-mode               ; Highlight colors
  :ensure t
  :bind ("C-c t R" . rainbow-mode)
  :init (add-hook 'css-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters         ; Highlight parens
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package hi-lock                    ; Custom regexp highlights
  :init (global-hi-lock-mode))

(provide 'mu-highlight)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-highlight.el ends here
