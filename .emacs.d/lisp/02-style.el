;;; 02-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the interface customizations.

;;; Code:

;;; Mode line
(use-package smart-mode-line
  :ensure t
  :init
  (progn
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
                               "hl-s")
                             "\\|"))
          sml/theme 'automatic)
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

(use-package unicode-fonts
  :ensure t
  :init (unicode-fonts-setup))

(use-package list-unicode-display
  :ensure t
  :defer t)

;;; Interface
;; Toggle all frames maximized and fullscreen
(modify-all-frames-parameters '((fullscreen . maximized)))

;; Turn off blinking cursor
(blink-cursor-mode -1)

;; Don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties '(read-only
				     t
				     point-entered
				     minibuffer-avoid-prompt
				     face
				     minibuffer-prompt))

;; Faster echo keystrokes
(setq echo-keystrokes 0.1)

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

;; Disable splash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Disable scratch buffer message
(setq initial-scratch-message nil)

;; Disable tooltips
(tooltip-mode -1)

;; Remove *Messages* buffer
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Automatically close some buffers on exit
(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	     (let ((completions "*Completions*"))
	       (and (get-buffer completions)
		    (kill-buffer completions)))))

;; Turn on column-number-mode
(column-number-mode)

;; Parenthesis and syntax highlighting
(use-package paren ; Highlight paired delimiters
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren t
		show-paren-when-point-in-periphery t))

;; Linum+ for better line numbers
(use-package linum+
  :disabled t
  :load-path "various"
  :config
  (progn
    (setq linum+-dynamic-format " %%%dd")

    ;; Linum+ resets linum-format to "smart" when it's loaded, hence we have to
    ;; use a eval-after-nload hook to set it to "dynamic".
    (eval-after-load "linum+" '(progn (setq linum-format 'dynamic)))))

;;; Buffer navigation
(use-package ido
  :init (progn
	  (ido-mode)
	  (ido-everywhere))
  :config
  (progn
    (setq ido-enable-flex-matching t ; Match characters if string doesn't match
	  ido-create-new-buffer 'always ; Create a new buffer if nothing matches
	  ido-use-filename-at-point 'guess
	  ;; Visit buffers and files in the selected window
	  ido-default-file-method 'selected-window
	  ido-default-buffer-method 'selected-window
	  ido-context-switch-command nil
	  ido-cur-item nil
	  ido-default-item nil
	  ido-cur-list nil)

    ;; Ido bury buffer
    (add-hook
     'ido-setup-hook
     (defun custom/define-ido-bury-key ()
       (define-key ido-completion-map
	 (kbd "C-b") 'custom/ido-bury-buffer-at-head)))

    (defun custom/ido-bury-buffer-at-head ()
      "Bury the buffer at the head of 'ido-matches'."
      (interactive)
      (let ((enable-recursive-minibuffers t)
	    (buf (ido-name (car ido-matches)))
	    (nextbuf (cadr ido-matches)))
	(when (get-buffer buf)
	  ;; If next match names a buffer use the buffer object;
	  ;; buffer name may be changed by packages such as
	  ;; uniquify.
	  (when (and nextbuf (get-buffer nextbuf))
	    (setq nextbuf (get-buffer nextbuf)))
	  (bury-buffer buf)
	  (if (bufferp nextbuf)
	      (setq nextbuf (buffer-name nextbuf)))
	  (setq ido-default-item nextbuf
		ido-text-init ido-text
		ido-exit 'refresh)
	  (exit-minibuffer)))

      ;; Find files with sudo
      (defadvice ido-find-file (after find-file-sudo activate)
        "Find file as root if necessary."
        (unless (and buffer-file-name
                     (file-writable-p buffer-file-name))
          (find-alternate-file
           (concat "/sudo:root@localhost:" buffer-file-name)))))))



(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode)
  :config
  (setq flx-ido-use-faces nil))

(use-package ido-load-library
  :ensure t
  :bind ("C-c f l" . ido-load-library-find))

(use-package recentf
  :init (recentf-mode)
  :bind (("C-x C-r" . ido-recentf-open))
  :config
  (progn
    ;; Open recent files with ido
    (defun ido-recentf-open ()
      "Use 'ido-completing-read' to \\[find-file] a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))

    (setq recentf-max-saved-items 200
          recentf-max-menu-items 15
          recentf-auto-cleanup 300
          recentf-exclude (list "/\\.git/.*\\'"
                                "/elpa/.*\\'"))))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward uniquify-separator ":"))

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
;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
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

(use-package smex
  :ensure t
  :bind (([remap execute-extended-command] . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package imenu
  :defer t
  :bind (("M-i" . imenu)))

(use-package imenu-anywhere
  :ensure t
  :bind (("M-I" . imenu-anywhere)))

(use-package calendar
  :defer t
  :config
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))

(use-package time
  :bind (("C-c u i" . emacs-init-time)
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
  ;; Fix the stupid `Info-quoted' face. Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
		      :inherit font-lock-constant-face))

;; Let apropos commands perform more extensive searches than default
(setq apropos-do-all t)

(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode)
  :defer page-break-lines-modes)

(use-package transpose-frame
  :ensure t
  :bind (("C-c t t" . transpose-frame)
         ("C-c t h" . flop-frame)
         ("C-c t v" . flip-frame)))

;;; Highlightings
(use-package diff-hl
  :ensure t
  :defer t
  :init
  (progn
    ;; Highlight changes to the current file in the fringe
    (global-diff-hl-mode)
    ;; Highlight changed files in the fringe of Dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    ;; Fall back to the display margin, if the fringe is unavailable
    (unless (display-graphic-p)
      (diff-hl-margin-mode))))

(use-package highlight-symbol
  :ensure t
  :defer t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
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
  (setq highlight-symbol-idle-delay 0.4 ; Highlight almost immediately
        highlight-symbol-on-navigation-p t)) ; Highlight immediately after navigation

(use-package rainbow-mode
  :ensure t
  :config (add-hook 'css-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

(use-package hl-line
  :init (global-hl-line-mode 1))

(provide '02-style)

;;; 02-style.el ends here
