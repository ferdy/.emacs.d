;;; custom-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the interface customizations.

;;; Code:

;;; Fonts
;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Set font fallback
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 14
                               :weight 'normal)))

(use-package unicode-fonts ; Map Unicode blocks to fonts
  :ensure t
  :disabled t
  :init (unicode-fonts-setup))

;;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 1000)

;; Undo scrolling
(defvar unscroll-point (make-marker)
  "Cursor position for next call to 'unscroll'.")

(defvar unscroll-window-start (make-marker)
  "Window start for next call to 'unscroll'.")

(defvar unscroll-hscroll nil
  "Hscroll for next call to 'unscroll'.")

(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)

(defun unscroll-maybe-remember ()
  "Remember where we started scrolling."
  (if (not (get last-command 'unscrollable))
      (progn
	(set-marker unscroll-point (point))
	(set-marker unscroll-window-start (window-start))
	(setq unscroll-hscroll (window-hscroll)))))

(defadvice scroll-up (before remember-for-unscroll
			     activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll
			       activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-left (before remember-for-unscroll
			       activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-right (before remember-for-unscroll
				activate compile)
  "Remember where we started from, for 'unscroll'."
  (unscroll-maybe-remember))

(defun unscroll ()
  "Revert to 'unscroll-point' and 'unscroll-window-start'."
  (interactive)
  (if (not unscroll-point)
      (error "Cannot unscroll yet"))
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

;;; Interface
;; Toggle all frames maximized and fullscreen
(modify-all-frames-parameters '((fullscreen . maximized)))

(blink-cursor-mode -1) ; Turn off blinking cursor

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

;; Disable annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

;; Disable startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(tooltip-mode -1) ; Disable tooltips

;; Automatically close some buffers on exit
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	     (let ((completions "*Completions*"))
	       (and (get-buffer completions)
		    (kill-buffer completions)))))

(column-number-mode) ; Turn on column-number-mode

(use-package linum+ ; Better line numbers
  :disabled t
  :load-path "various"
  :config (progn
            (setq linum+-dynamic-format " %%%dd")

            ;; Linum+ resets linum-format to "smart" when it's loaded, we have
            ;; to use a eval-after-nload hook to set it to "dynamic".
            (eval-after-load "linum+" '(progn (setq linum-format 'dynamic)))))

(use-package recentf ; Manage recent files
  :init (recentf-mode)
  :defer t
  :config (setq recentf-max-saved-items 200
                recentf-max-menu-items 15
                recentf-exclude (list "/\\.git/.*\\'"
                                      "/elpa/.*\\'"
                                      "/tmp/"
                                      "/ssh:")))

(use-package uniquify ; Unique buffer names
  :config (setq uniquify-buffer-name-style
                'post-forward uniquify-separator ":"))

;;; Theme
(use-package solarized
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

(use-package elec-pair
  :init (progn
          ;; Enable electric-pair only for certain modes
          (defvar my-electic-pair-modes '(emacs-lisp-mode
                                          clojure-mode))

          (defun my-inhibit-electric-pair-mode (char)
            (not (member major-mode my-electic-pair-modes)))

          (setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)
          
          (electric-pair-mode)))

(use-package page-break-lines ; Better looking break lines
  :ensure t
  :defer t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package transpose-frame ; Easily swap frames
  :ensure t
  :bind (("C-c t t" . transpose-frame)
         ("C-c t h" . flop-frame)
         ("C-c t v" . flip-frame)))

(use-package winner ; Undo and redo window configurations
  :init (winner-mode))

;;; Highlightings
(use-package paren ; Highlight paired delimiters
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

(use-package diff-hl ; Show changes in fringe
  :ensure t
  :defer 10
  :init (progn
          ;; Highlight changes to the current file in the fringe
          (global-diff-hl-mode)
          ;; Highlight changed files in the fringe of Dired
          (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
          ;; Fall back to the display margin, if the fringe is unavailable
          (unless (display-graphic-p)
            (diff-hl-margin-mode))))

(use-package highlight-symbol ; Highlight and jump to symbols
  :ensure t
  :defer t
  :bind (("C-c s %" . highlight-symbol-query-replace)
         ("C-c s n" . highlight-symbol-next-in-defun)
         ("C-c s o" . highlight-symbol-occur)
         ("C-c s p" . highlight-symbol-prev-in-defun))
  :init (progn
          ;; Navigate occurrences of the symbol under point with M-n and M-p
          (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
          ;; Highlight symbol occurrences
          (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config (setq highlight-symbol-idle-delay 0.4 ; Almost immediately
                ;; Immediately after navigation
                highlight-symbol-on-navigation-p t)
  :diminish highlight-symbol-mode)

(use-package rainbow-mode ; Highlight colors
  :ensure t
  :defer t
  :config (add-hook 'css-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters ; Highlight parens
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

(use-package hl-line ; Highlight current line
  :init (global-hl-line-mode 1))

;;; Mode line
(use-package smart-mode-line ; Better mode-line
  :ensure t
  :init (progn
          ;; Hide some modes
          (require 'rich-minority)
          (setq rm-blacklist
                (format "^ \\(%s\\)$"
                        (mapconcat #'identity
                                   '("Projectile.*"
                                     "Wrap"
                                     "WSC.*"
                                     "Helm")
                                   "\\|")))
          (sml/setup)))

(provide 'custom-style)

;;; custom-style.el ends here
