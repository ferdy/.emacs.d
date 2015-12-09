;;; mu-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my look'n'feel configuration.

;;; Code:

;;; Fonts setup
;; These are the fonts in use:
;; - Source Code Pro: https://github.com/adobe-fonts/source-code-pro
;; - DejaVu Sans Mono: https://packages.debian.org/sid/fonts-dejavu
;; - Fira Sans: https://github.com/mozilla/Fira
;; - Symbola: https://packages.debian.org/sid/ttf-ancient-fonts
;; - XITS Math: https://github.com/khaledhosny/xits-math
;; - Gentium Plus: https://packages.debian.org/sid/fonts-sil-gentiumplus

(defun mu-setup-main-fonts (default-height variable-pitch-height)
  "Set up default fonts.
Use DEFAULT-HEIGHT for default face and VARIABLE-PITCH-HEIGHT
for variable-pitch face."
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans"
                      :height variable-pitch-height
                      :weight 'regular))

;; Dinamically change font size based upon screen resolution
(when window-system
  (if (> (x-display-pixel-width) 1800)
      (mu-setup-main-fonts 155 155)
    (mu-setup-main-fonts 125 125)))

(defun mu-configure-fonts (frame)
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
    (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Symbola")
                    frame 'append))

(-when-let (frame (selected-frame))
  (mu-configure-fonts frame))
(add-hook 'after-make-frame-functions #'mu-configure-fonts)

;;; Interface
(use-package frame ; Frames
  :bind ("C-c w f" . toggle-frame-fullscreen)
  :init
  (progn
    ;; Kill `suspend-frame'
    (global-set-key (kbd "C-z") nil)
    (global-set-key (kbd "C-x C-z") nil))
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

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

;; Disable startup echo area message
(fset 'display-startup-echo-area-message #'ignore)

(column-number-mode) ; Turn on column-number-mode

;;; Theme
(use-package solarized ; Default theme
  :ensure solarized-theme
  :config
  (progn
    (setq solarized-use-variable-pitch nil  ; Avoid all font-size changes
          solarized-distinct-doc-face t     ; Make doc faces stand out more
          solarized-scale-org-headlines nil ; Don't scale Org headlines
          solarized-use-more-italic t       ; Use italic more often
          solarized-use-less-bold t         ; Less bold, italic is enough
          ;; Underline below the font bottomline instead of the baseline
          x-underline-at-descent-line t)

    (load-theme 'solarized-light 'no-confirm)))

(use-package darktooth ; Another beautiful dark theme
  :ensure darktooth-theme
  :defer t)

(use-package page-break-lines ; Better looking break lines
  :ensure t
  :defer t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

;;; Mode line
(use-package smart-mode-line ; Better mode-line
  :ensure t
  :init
  (progn
    (setq sml/theme nil ; Let Solarized take care of the mode-line
          sml/mode-width 'full
          sml/no-confirm-load-theme t)

    (sml/setup))
  :config
  (progn
    ;; More abbreviations
    (add-to-list 'sml/replacer-regexp-list
                 '("^~/githubs/" ":Git:") t)
    (add-to-list 'sml/replacer-regexp-list
                 '("^:Doc:boccaperta/" ":Ba:") t)
    (add-to-list 'sml/replacer-regexp-list
                 '("^:Doc:books/" ":Bks:") t)
    (add-to-list 'sml/replacer-regexp-list
                 '("^~/projects/" ":Prj:") t)))

(use-package rich-minority
  :ensure smart-mode-line
  :after smart-mode-line
  :config (setq rm-blacklist
                (format "^ \\(%s\\)$"
                        (mapconcat #'identity
                                   '("Wrap"
                                     "WSC.*"
                                     "cWip"
                                     "sWip")
                                   "\\|"))))

;;; Utilities and keybindings
(setq custom-safe-themes t) ; Treat themes as safe

(setq history-length 1000) ; Store more history

(setq-default line-spacing 0.1) ; Increase line-spacing (default 0)

;; Configure a reasonable fill column and enable automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function)

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

(provide 'mu-style)

;;; mu-style.el ends here
