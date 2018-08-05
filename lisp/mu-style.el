;;; mu-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my look'n'feel configuration.

;;; Code:

;;; Fonts setup
;; Fonts used:
;; - Iosevka (https://github.com/be5invis/Iosevka)
;; - Fira Sans (https://github.com/mozilla/Fira/)
(set-face-attribute 'default nil
                    :family "Iosevka SS08"
                    :height 145
                    :weight 'regular)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 145
                    :weight 'regular)

(use-package default-text-scale         ; Adjust the font size in all frames
  :ensure t
  :bind (("C-M-+" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
         ("C-M-0" . default-text-scale-reset))
  :init (default-text-scale-mode))

;;; Interface
(use-package frame                      ; Frames
  :bind ("C-c w f" . toggle-frame-fullscreen)
  :init
  ;; Kill `suspend-frame'
  (unbind-key "C-x C-z")
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(validate-setq echo-keystrokes 0.1)     ; Faster echo keystrokes

;; Avoid showing ?? in the mode line when we have long lines.
(validate-setq line-number-display-limit-width 10000)

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Do not pop up *Messages* when clicking on the minibuffer
(bind-key [mouse-1] #'ignore minibuffer-inactive-mode-map)

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
(validate-setq
 ring-bell-function #'ignore
 inhibit-startup-screen t
 initial-scratch-message nil)

;; Disable startup echo area message
(fset 'display-startup-echo-area-message #'ignore)

(validate-setq
 x-gtk-use-system-tooltips nil          ; Use Emacs tooltips
 history-length 1000)                   ; Store more history

;; Do not truncate lines
(validate-setq
 truncate-lines nil
 truncate-partial-width-windows nil)

(setq-default line-spacing 0.2)         ; Increase line-spacing (default 0)

;; Configure a reasonable fill column and enable automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function)

;; Hide the cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil)

(use-package iso-transl                 ; Fix dead characters
  :demand t)

(use-package page-break-lines           ; Better looking break lines
  :ensure t
  :defer t
  :init (global-page-break-lines-mode))

(use-package prog-mode                  ; Generic programming modes settings
  :config
  ;; Prettify symbols
  (global-prettify-symbols-mode 1)

  ;; Unprettify symbols with point on them and next to them
  (validate-setq prettify-symbols-unprettify-at-point 'right-edge))

(use-package ansi-color                 ; Colorize ANSI escape sequences
  :defer t
  :config
  (defun mu-colorize-compilation ()
    "Colourize from `compilation-filter-start' to `point-max'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook #'mu-colorize-compilation))

;; Underline below the font bottomline instead of the baseline
(validate-setq x-underline-at-descent-line t)

;;; Theme
(validate-setq custom-safe-themes t)    ; Treat themes as safe

(use-package color-theme-sanityinc-tomorrow ; Default theme
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night 'no-confirm)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line nil :overline line)
    (set-face-attribute 'mode-line-inactive nil :overline line)
    (set-face-attribute 'mode-line-inactive nil :underline line)
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :box nil)))

(use-package dimmer                     ; Highlight selected buffer
  :ensure t
  :init (dimmer-mode)
  :config (setq-default dimmer-fraction 0.20))

;;; The mode line
;; Slightly decrease the font in the mode line
(set-face-attribute 'mode-line nil
                    :family "Iosevka SS08"
                    :height 130
                    :weight 'regular)

(line-number-mode)
(column-number-mode)

;; Show buffer position percentage starting from top
(validate-setq mode-line-percent-position '(-3 "%o"))

;; Custom Eyebrowse mode-line indicator
(defvar mu-eyebrowse-mode-line
  '(:propertize
    (:eval
     (when (bound-and-true-p eyebrowse-mode)
       (let* ((num (eyebrowse--get 'current-slot))
              (tag (when num
                     (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
              (str (concat
                    " "
                    (if (and tag (< 0 (length tag)))
                        tag
                      (when num (int-to-string num)))
                    " ")))
         str)))
    face (:inherit fringe :underline nil))
  "Mode line format for Eyebrowse.")

(put 'mu-eyebrowse-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e"
                mu-eyebrowse-mode-line
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                (vc-mode vc-mode)
                (multiple-cursors-mode mc/mode-line)
                " " mode-line-modes
                mode-line-end-spaces))

(defmacro rename-modeline (package-name mode new-name)
  "Rename PACKAGE-NAME with MODE into NEW-NAME in the mode line."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")

(use-package minions                    ; A minor-mode menu for the mode line
  :ensure t
  :init (minions-mode)
  :config (validate-setq minions-direct '(cider-mode
                                          flycheck-mode
                                          overwrite-mode)))

(use-package moody                      ; Tabs and ribbons for the mode line
  :ensure t
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;;; Utilities and key bindings
(defun mu-reset-fonts ()
  "Reset fonts to my preferences."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Iosevka SS08"
                      :height 145
                      :weight 'regular)
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans"
                      :height 145
                      :weight 'regular)
  (set-face-attribute 'mode-line nil
                      :family "Iosevka SS08"
                      :height 130
                      :weight 'regular))

(bind-key "C-c t f" #'mu-reset-fonts)

(provide 'mu-style)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-style.el ends here
