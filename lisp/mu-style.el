;;; mu-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my look'n'feel configuration.

;;; Code:

;;; Fonts setup
;; Fonts used:
;; - Iosevka (https://github.com/be5invis/Iosevka)
;; - Fira Sans (https://github.com/mozilla/Fira/)
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 150)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 145
                    :weight 'regular)

;;; Interface
(use-package frame                      ; Frames
  :bind ("C-c w f" . toggle-frame-fullscreen)
  :init
  ;; Kill `suspend-frame'
  (unbind-key "C-z")
  (unbind-key "C-x C-z")
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(validate-setq echo-keystrokes 0.1)              ; Faster echo keystrokes

;; Avoid showing ?? in the mode line when we have long lines.
(validate-setq line-number-display-limit-width 10000)

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Turn off annoying settings
(blink-cursor-mode -1)
(tooltip-mode -1)

;; Cursor stretches to the current glyph's width
(validate-setq x-stretch-cursor t)

;; Disable annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(validate-setq kill-buffer-query-functions
               (remq 'process-kill-buffer-query-function
                     kill-buffer-query-functions))

;; Disable startup messages
(validate-setq ring-bell-function #'ignore
               inhibit-startup-screen t
               initial-scratch-message nil)

;; Disable startup echo area message
(fset 'display-startup-echo-area-message #'ignore)

(validate-setq x-gtk-use-system-tooltips nil) ; Use Emacs tooltips
(validate-setq history-length 1000)           ; Store more history
(setq-default line-spacing 0.2)         ; Increase line-spacing (default 0)

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

;; Hide the cursor in inactive windows
(setq-default cursor-in-non-selected-windows t)

;; Should make Emacs snappier
(add-hook 'focus-out-hook #'garbage-collect)

(use-package iso-transl                 ; Fix dead characters
  :demand t)

(use-package page-break-lines           ; Better looking break lines
  :ensure t
  :defer t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

;;; Prettify symbols
(global-prettify-symbols-mode 1)

;; Unprettify symbols with point on them and symbols
;; right next to point
(validate-setq prettify-symbols-unprettify-at-point 'right-edge)

(use-package ansi-color                 ; Colorize ANSI escape sequences
  :defer t
  :config
  (defun mu-colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point-max'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook #'mu-colorize-compilation))

;; Underline below the font bottomline instead of the baseline
(validate-setq x-underline-at-descent-line t)

(use-package stripe-buffer              ; Add stripes to a buffer
  :ensure t
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))

;;; Theme
(validate-setq custom-safe-themes t)    ; Treat themes as safe

(use-package solarized                  ; Default theme
  :ensure solarized-theme
  :config
  (validate-setq
   solarized-use-variable-pitch nil  ; Disable variable pitch fonts
   solarized-distinct-doc-face t     ; Make doc faces stand out more
   solarized-use-more-italic t       ; Use italic more often
   solarized-use-less-bold t         ; Less bold, italic is enough
   ;; Avoid all font-size changes
   solarized-height-minus-1 1.0
   solarized-height-plus-1 1.0
   solarized-height-plus-2 1.0
   solarized-height-plus-3 1.0
   solarized-height-plus-4 1.0)

  (load-theme 'solarized-light 'no-confirm))

;;; The mode-line
(line-number-mode)
(column-number-mode)

;; Show buffer position percentage starting from top
(validate-setq mode-line-percent-position '(-3 "%o"))

;; Increase mode-line size with a border (box) of the same colour and
;; reduce font size by tweaking height
(set-face-attribute 'mode-line nil
                    :inverse-video nil
                    :height 0.9
                    :box '(:line-width 6 :color "#eee8d5" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :inverse-video nil
                    :height 0.9
                    :box '(:line-width 6 :color "#fdf6e3" :style nil))

;;; Utilities and key bindings
(defun mu-reset-fonts ()
  "Reset fonts to my preferences."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Iosevka"
                      :height 150)
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans"
                      :height 145
                      :weight 'regular))

(bind-key "C-c t f" #'mu-reset-fonts)

(provide 'mu-style)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-style.el ends here
