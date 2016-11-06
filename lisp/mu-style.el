;;; mu-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my look'n'feel configuration.

;;; Code:

;;; Fonts setup
;; Fonts used:
;; - Source Code Pro (https://github.com/adobe-fonts/source-code-pro)
;; - Fira Sans (https://github.com/mozilla/Fira/)
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
      (mu-setup-main-fonts 150 160)
    (mu-setup-main-fonts 130 140)))

(defun mu-configure-fonts (frame)
  "Set up fonts for FRAME.

Configure various overrides for symbols, greek letters,
as well as fall backs for."
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

(validate-setq echo-keystrokes 0.1)              ; Faster echo keystrokes

;; Avoid showing ?? in the mode line when we have long lines.
(validate-setq line-number-display-limit-width 10000)

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

;;; The mode line
(line-number-mode)
(column-number-mode)

(setq-default mode-line-position
              '((-3 "%p") (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode
                 ("%l" (column-number-mode ":%c")))))

(defvar mu-projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             (concat " " (projectile-project-name))))
    face font-lock-constant-face)
  "Mode line format for Projectile.")
(put 'mu-projectile-mode-line 'risky-local-variable t)

(validate-setq
 flycheck-mode-line
 '(:eval
   (pcase flycheck-last-status-change
     (`not-checked nil)
     (`no-checker (propertize " -" 'face 'warning))
     (`running (propertize " ✷" 'face 'success))
     (`errored (propertize " !" 'face 'error))
     (`finished
      (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
             (no-errors (cdr (assq 'error error-counts)))
             (no-warnings (cdr (assq 'warning error-counts)))
             (face (cond (no-errors 'error)
                         (no-warnings 'warning)
                         (t 'success))))
        (propertize (format " %s/%s" (or no-errors 0) (or no-warnings 0))
                    'face face)))
     (`interrupted " -")
     (`suspicious '(propertize " ?" 'face 'warning)))))

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                (projectile-mode mu-projectile-mode-line)
                (vc-mode (:propertize (:eval vc-mode) face italic))
                " "
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; And the modes, which we don't really care for anyway
                " " mode-line-misc-info mode-line-modes mode-line-end-spaces)
              mode-line-remote
              '(:eval
                (when-let (host (file-remote-p default-directory 'host))
                  (propertize (concat "@" host) 'face
                              '(italic warning)))))

(provide 'mu-style)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-style.el ends here
