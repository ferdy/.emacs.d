;;; mu-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my look'n'feel configuration.

;;; Code:

;;; Fonts setup
(defun mu-setup-main-fonts (default-height variable-pitch-height)
  "Set up default fonts.
Use DEFAULT-HEIGHT for default face and VARIABLE-PITCH-HEIGHT
for variable-pitch face."
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family "DejaVu Sans"
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
  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Symbola")
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
(use-package frame                      ; Frames
  :bind ("C-c w f" . toggle-frame-fullscreen)
  :init
  ;; Kill `suspend-frame'
  (unbind-key "C-z")
  (unbind-key "C-x C-z")
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(setq echo-keystrokes 0.1)              ; Faster echo keystrokes

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

(column-number-mode)                    ; Turn on column-number-mode

(setq x-gtk-use-system-tooltips nil)    ; Use Emacs tooltips
(setq history-length 1000)              ; Store more history
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

;; C-specific Indentation
(setq c-default-style "linux"
      c-basic-offset 4)

(use-package page-break-lines           ; Better looking break lines
  :ensure t
  :defer t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

;;; Prettify symbols
(global-prettify-symbols-mode 1)

;; Unprettify symbols with point on them and symbols
;; right next to point
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Improve LaTeX equations with font-lock
(defface mu-unimportant-latex-face
  '((t :height 0.7
       :inherit font-lock-comment-face))
  "Face used on less relevant math commands.")

(font-lock-add-keywords
 'latex-mode
 `((,(rx (or (and "\\" (or (any ",.!;")
                           (and (or "left" "right"
                                    "big" "Big")
                                symbol-end)))
             (any "_^")))
    0 'mu-unimportant-latex-face prepend))
 'end)

;;; Theme
(setq custom-safe-themes t)             ; Treat themes as safe

(use-package solarized                  ; My default theme
  :ensure solarized-theme
  :config
  (setq solarized-use-variable-pitch nil  ; Disable variable pitch fonts
        solarized-distinct-doc-face t     ; Make doc faces stand out more
        solarized-use-more-italic t       ; Use italic more often
        solarized-use-less-bold t         ; Less bold, italic is enough
        ;; Underline below the font bottomline instead of the baseline
        x-underline-at-descent-line t
        ;; Avoid all font-size changes
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)

  (load-theme 'solarized-light 'no-confirm))

;;; Mode line
(use-package which-func                 ; Current function name
  :init (which-func-mode)
  :config
  (defun mu-which-func-current ()
    "Get current function name and set it to max 20 characters width."
    (if-let (current (or (gethash (selected-window) which-func-table)))
        (truncate-string-to-width current 20 nil nil "…")
      which-func-unknown))

  (setq which-func-unknown "⊥"
        which-func-format
        `((:propertize (" ➤ " (:eval (mu-which-func-current)))
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))

(use-package smart-mode-line            ; Better mode-line
  :ensure t
  :init
  (sml/setup)

  (setq sml/theme nil
        sml/mode-width 'full
        sml/no-confirm-load-theme t)

  (setq-default mode-line-end-spaces
                (append mode-line-end-spaces
                        '(:eval "%I ")))
  :config
  ;; More abbreviations
  (add-to-list 'sml/replacer-regexp-list
               '("^~/githubs/" ":Git:") t)
  (add-to-list 'sml/replacer-regexp-list
               '("^:Doc:boccaperta/" ":Ba:") t)
  (add-to-list 'sml/replacer-regexp-list
               '("^:Doc:books/" ":Bks:") t)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/projects/" ":Prj:") t))

(provide 'mu-style)

;;; mu-style.el ends here
