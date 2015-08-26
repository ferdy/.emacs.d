;;; custom-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the interface customizations.

;;; Code:

;;; Fonts setup
;; Dinamically change font size based upon screen resolution
(if window-system
    (progn
      (if (> (x-display-pixel-width) 1800)
          (set-frame-font "Source Code Pro-16" nil t)
        (set-frame-font "Source Code Pro-13" nil t))))

(defun custom/configure-fonts (frame)
  "Set up fonts for FRAME.
Set the default font, and configure various overrides for
symbols, greek letters, as well as fall backs for."

  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))

  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Symbola")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Gentium Plus")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Source Code Pro")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Symbola")
                    frame 'append))

(when-let (frame (selected-frame))
  (custom/configure-fonts frame))
(add-hook 'after-make-frame-functions #'custom/configure-fonts)

;;; Interface
;;Toggle all frames maximized and fullscreen
(modify-all-frames-parameters '((fullscreen . maximized)))

(setq echo-keystrokes 0.1) ; Faster echo keystrokes

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Turn off annoying settings
(blink-cursor-mode -1)
(tooltip-mode -1)

;; Disable annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Disable startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(column-number-mode) ; Turn on column-number-mode

;;; Theme
(use-package solarized ; Default theme
  :ensure solarized-theme
  :config (progn
            (setq solarized-use-variable-pitch nil ; Avoid all font-size changes
                  solarized-distinct-doc-face t ; Make doc faces stand out more
                  solarized-scale-org-headlines nil ; Don't scale Org headlines
                  ;; Underline below the font bottomline instead of the baseline
                  x-underline-at-descent-line t)
            (load-theme 'solarized-light 'no-confirm)))

(use-package darktooth-theme ; Preferred dark theme
  :ensure t
  :disabled t
  :init (load-theme 'darktooth 'no-confirm))

;;; Utilities
(setq history-length 1000) ; Store more history

;; Disable tabs, but give them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

(setq-default line-spacing 0.1) ; Increase line-spacing (default 0)

(setq tab-always-indent 'complete) ; Make Tab complete if the line is indented

;; Configure a reasonable fill column and enable automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Same for region casing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; C-specific Indentation
(setq c-default-style "linux"
      c-basic-offset 4)

(use-package page-break-lines ; Better looking break lines
  :ensure t
  :defer t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package nlinum ; Line numbers in display margin
  :ensure t
  :bind ("C-c t l" . nlinum-mode))

;;; Mode line
(use-package smart-mode-line ; Better mode-line
  :ensure t
  :init (progn
          ;; Hide some modes
          (use-package rich-minority)
          (setq rm-blacklist
                (format "^ \\(%s\\)$"
                        (mapconcat #'identity
                                   '("Wrap"
                                     "WSC.*"
                                     "yas"
                                     "cWip"
                                     "sWip")
                                   "\\|"))
                sml/theme 'automatic
                sml/mode-width 'full
                sml/no-confirm-load-theme t)
          (sml/setup))
  :config (progn
            ;; More abbreviations
            (add-to-list 'sml/replacer-regexp-list
                         '("^~/githubs/" ":Git:") t)
            (add-to-list 'sml/replacer-regexp-list
                         '("^:Doc:boccaperta/" ":Ba:") t)
            (add-to-list 'sml/replacer-regexp-list
                         '("^:Doc:books/" ":Bks:") t)
            (add-to-list 'sml/replacer-regexp-list
                         '("^~/projects/" ":Prj:") t)))

(provide 'custom-style)

;;; custom-style.el ends here
