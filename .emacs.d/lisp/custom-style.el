;;; custom-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the interface customizations.

;;; Code:

;;; Fonts
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

;; Remove *Messages* buffer
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

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
  :init
  (progn
    (setq solarized-use-variable-pitch nil ; Avoid all font-size changes
          ;; Don't add too much colours to the fringe
          solarized-emphasize-indicators nil
          ;; Don't change size of org-mode headlines (but keep other size-changes)
          solarized-scale-org-headlines nil
          ;; Underline below the font bottomline instead of the baseline
          x-underline-at-descent-line t)
    (if (daemonp)
        (add-hook 'after-make-frame-functions
                  '(lambda (f)
                     (with-selected-frame f
                       (when (window-system f) (load-theme 'solarized-light t)))))
      (load-theme 'solarized-light t))

    ;; Functions to remove background when on terminals
    (defun on-frame-open (frame)
      "Remove background for FRAME on terminals."
      (if (not (display-graphic-p frame))
          (set-face-background 'default "unspecified-bg" frame)))
    (on-frame-open (selected-frame))

    (add-hook 'after-make-frame-functions 'on-frame-open)

    (defun on-after-init ()
      "Remove background after init on terminals."
      (unless (display-graphic-p (selected-frame))
        (set-face-background 'default "unspecified-bg" (selected-frame))))

    (add-hook 'window-setup-hook 'on-after-init)))

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

(use-package electric
  :init (electric-layout-mode))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package calendar
  :defer t
  :config (setq calendar-week-start-day 1)) ; In Europe we start on Monday

(use-package time
  :bind (("C-c u i" . emacs-init-time)
         ("C-c u u" . emacs-uptime)
	 ("C-c u t" . display-time-world))
  :config
  (setq display-time-world-time-format "%H:%M %Z, %d. %b"
	display-time-world-list '(("Europe/Rome" "Rome")
				  ("Europe/London" "London")
				  ("Asia/Hong_Kong" "Hong Kong")
				  ("Asia/Tokyo" "Tokyo"))))

(use-package info
  :defer t
  :config
  ;; Fix `Info-quoted' face by going back to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
		      :inherit font-lock-constant-face))

;; Let apropos commands perform more extensive searches than default
(setq apropos-do-all t)

(use-package page-break-lines ; Better looking break lines
  :ensure t
  :defer t
  :init (global-page-break-lines-mode))

(use-package transpose-frame ; Easily swap frames
  :ensure t
  :bind (("C-c t t" . transpose-frame)
         ("C-c t h" . flop-frame)
         ("C-c t v" . flip-frame)))

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
  :init
  (progn
    ;; Navigate occurrences of the symbol under point with M-n and M-p
    (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
    ;; Highlight symbol occurrences
    (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 0.4 ; Almost immediately
        highlight-symbol-on-navigation-p t)) ; Immediately after navigation

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
                                   '("FlyC.*"
                                     "Projectile.*"
                                     "PgLn"
                                     "company"
                                     "Undo-Tree"
                                     "Wrap"
                                     "hhat"
                                     "SliNav"
                                     "hl-s"
                                     "WSC.*")
                                   "\\|")))
          (sml/setup)))

;; Minor mode to hide the mode line
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

;;;###autoload
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message "Hidden Mode Line Mode enabled.")))

;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

(provide 'custom-style)

;;; custom-style.el ends here
