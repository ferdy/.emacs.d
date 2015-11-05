;;; mu-highlight.el --- Part of my Emacs setup   -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for highlighting utilities.

;;; Code:

(use-package paren ; Highlight paired delimiters
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(use-package diff-hl ; Show changes in fringe
  :ensure t
  :defer 10
  :init
  (progn
    ;; Highlight changes to the current file in the fringe
    (global-diff-hl-mode)
    ;; Highlight changed files in the fringe of Dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    ;; Fall back to the display margin, if the fringe is unavailable
    (unless (display-graphic-p)
      (diff-hl-margin-mode))))

(use-package highlight-symbol ; Highlight and jump to symbols
  :ensure t
  :bind (("C-c s %" . highlight-symbol-query-replace)
         ("C-c n n" . highlight-symbol-next-in-defun)
         ("C-c n p" . highlight-symbol-prev-in-defun))
  :init
  (progn
    ;; Navigate occurrences of the symbol under point with M-n and M-p
    (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
    ;; Highlight symbol occurrences
    (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config (setq highlight-symbol-idle-delay 0.4 ; Almost immediately
                ;; Immediately after navigation
                highlight-symbol-on-navigation-p t)
  :diminish highlight-symbol-mode)

(use-package highlight-numbers ; Fontify number literals
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package rainbow-mode ; Highlight colors
  :ensure t
  :bind ("C-c t R" . rainbow-mode)
  :config (add-hook 'css-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters ; Highlight parens
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package hi-lock ; Custom regexp highlights
  :init (global-hi-lock-mode))

(use-package whitespace ; Highlight bad whitespace
  :commands whitespace-mode
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'mu-whitespace-mode-local))
  :config
  ;; Highlight tabs, empty lines at beg/end, trailing whitespaces and overlong
  ;; portions of lines via faces.  Also indicate tabs via characters
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil) ; Use `fill-column' for overlong lines
  :diminish (whitespace-mode . " ⓦ"))

(use-package focus ; Dim the text of surrounding sections
  :ensure t
  :bind ("C-c t h" . focus-mode))

(use-package beacon ; Highlight cursor when moving in buffers and windows
  :ensure t
  :init (beacon-mode 1)
  :config
  (progn
    (setq beacon-color "#93a1a1")
    ;; Don't blink on specific major modes
    (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
    (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode)
    ;; Don't blink on next-line/previous-line at the top/bottom of the window
    (add-to-list 'beacon-dont-blink-commands 'next-line)
    (add-to-list 'beacon-dont-blink-commands 'previous-line)
    (add-to-list 'beacon-dont-blink-commands 'sx-question-list-next)
    (add-to-list 'beacon-dont-blink-commands 'sx-question-list-previous))
  :diminish beacon-mode)

;;; Utilities and keybindings
(defun mu-whitespace-style-no-long-lines ()
  "Configure `whitespace-mode' for Org.
Disable the highlighting of overlong lines."
  (setq-local whitespace-style (-difference whitespace-style
                                            '(lines lines-tail))))

(defun mu-whitespace-mode-local ()
  "Enable `whitespace-mode' after local variables where set up."
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

(provide 'mu-highlight)

;;; mu-highlight.el ends here
