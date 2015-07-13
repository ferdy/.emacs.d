;;; custom-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the interface customizations.

;;; Code:

;;; Fonts setup
(set-frame-font "Source Code Pro-13") ; Default font

;; Additional fonts for special characters and fallbacks
(set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'append)
(set-fontset-font t 'mathematical (font-spec :family "XITS Math") nil 'append)
(set-fontset-font t 'greek (font-spec :family "Gentium Plus") nil 'append)
(set-fontset-font t nil (font-spec :family "Symbola") nil 'append)

(use-package dynamic-fonts ; Select best available font
  :ensure t
  :disabled t
  :config (progn
            (setq dynamic-fonts-preferred-monospace-fonts
                  '("Source Code Pro"
                    "DejaVu Sans Mono")
                  dynamic-fonts-preferred-monospace-point-size 13
                  dynamic-fonts-preferred-proportional-fonts
                  '("Fira Sans Book"
                    "DejaVu Sans Book")
                  dynamic-fonts-preferred-proportional-point-size 13)
            (dynamic-fonts-setup)))

(use-package unicode-fonts ; Map Unicode blocks to fonts
  :ensure t
  :disabled t
  :config (progn
            (setq unicode-fonts-skip-font-groups '(low-quality-glyphs)
                  unicode-fonts-use-prepend t)
            (unicode-fonts-setup)))

;;; Interface
;; Toggle all frames maximized and fullscreen
(modify-all-frames-parameters '((fullscreen . maximized)))

;; Don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties '(read-only
				     t
				     point-entered
				     minibuffer-avoid-prompt
				     face
				     minibuffer-prompt))

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

;; Automatically close some buffers on exit
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	     (let ((completions "*Completions*"))
	       (and (get-buffer completions)
		    (kill-buffer completions)))))

(column-number-mode) ; Turn on column-number-mode

(use-package uniquify ; Unique buffer names
  :config (setq uniquify-buffer-name-style
                'post-forward uniquify-separator ":"))

;;; Theme
(use-package solarized ; Default theme
  :ensure solarized-theme
  :defer t
  :init (progn
          (setq solarized-use-variable-pitch nil ; Avoid all font-size changes
                ;; Don't add too much colours to the fringe
                solarized-emphasize-indicators nil
                ;; Don't change size of org-mode headlines
                solarized-scale-org-headlines nil
                ;; Underline below the font bottomline instead of the baseline
                x-underline-at-descent-line t)
          (load-theme 'solarized-light 'no-confirm)))

;;; Utilities
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
                                     "yas")
                                   "\\|"))
                sml/theme 'automatic
                sml/no-confirm-load-theme t)
          (sml/setup))
  :config (progn
            ;; More abbreviations
            (add-to-list 'sml/replacer-regexp-list '("^~/githubs/" ":Git:") t)
            (add-to-list 'sml/replacer-regexp-list
                         '("^:Doc:boccaperta/" ":Ba:") t)))

(provide 'custom-style)

;;; custom-style.el ends here
