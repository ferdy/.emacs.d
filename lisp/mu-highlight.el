;;; mu-highlight.el --- Part of my Emacs setup   -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for highlighting utilities.

;;; Code:

(use-package paren                      ; Highlight paired delimiters
  :init (show-paren-mode)
  :config
  (validate-setq
   show-paren-when-point-inside-paren t
   show-paren-when-point-in-periphery t))

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
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev))
  :init (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
          (add-hook hook #'symbol-overlay-mode))
  :diminish symbol-overlay-mode)

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
