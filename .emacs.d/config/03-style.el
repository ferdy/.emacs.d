;;; 03-style.el --- Part of my Emacs configuration

;;; Commentary:
;; This file stores the interface customizations.

;;; Code:
;; Set default font
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 110
		    :weight 'normal
		    :width 'normal)

;; Set font fallback
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
		    'unicode
		    (font-spec :family "DejaVu Sans Mono"
			       :width 'normal
			       :size 11.4
			       :weight 'normal)))

;; Toggle all frames maximized and fullscreen
(modify-all-frames-parameters '((fullscreen . maximized)))

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

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

;; Turn on visual-line-mode
(global-visual-line-mode)

;; Parenthesis and syntax highlighting
(setq show-paren-delay 0
      show-paren-style 'parenthesis)

(show-paren-mode 1)

;; Linum+ for better line numbers
(use-package linum+
  :disabled t
  :load-path "various"
  :config
  (progn
    (setq linum+-dynamic-format " %%%dd")

    ;; Linum+ resets linum-format to "smart" when it's loaded, hence we have to
    ;; use a eval-after-load hook to set it to "dynamic".
    (eval-after-load "linum+" '(progn (setq linum-format 'dynamic)))))

;; IDO
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
    ;; See http://endlessparentheses.com/Ido-Bury-Buffer.html
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
	  (exit-minibuffer))))))

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

;; SMEX
(use-package smex
  :ensure t
  :bind (([remap execute-extended-command] . smex)
	 ("M-X" . smex-major-mode-commands)))

;; IMENU
(use-package imenu
  :defer t
  :bind (("M-i" . imenu)))

;; IMENU-ANYWHERE
(use-package imenu-anywhere
  :ensure t
  :bind (("M-I" . imenu-anywhere)))

;; Tune `eval-expression'
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; Set unique buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward uniquify-separator ":"))

;; Faster echo keystrokes
;; See http://endlessparentheses.com/faster-keystroke-echo.html
(setq echo-keystrokes 0.1)

;; Set the directory where all backup and autosave files will be saved
(defvar backup-dir "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))

;; Set theme
(use-package solarized
  :ensure solarized-theme
  :defer t
  :init
  (progn
    (if (daemonp)
	(add-hook 'after-make-frame-functions
		  '(lambda (f)
		     (with-selected-frame f
		       (when (window-system f) (load-theme 'solarized-dark t)))))
      (load-theme 'solarized-dark t))

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

    (add-hook 'window-setup-hook 'on-after-init))
  :config t
  ;; Make the modeline high contrast
  (setq solarized-high-contrast-mode-line t
	;; Don't change size of org-mode headlines (but keep other size-changes)
	solarized-scale-org-headlines nil
	;; Avoid all font-size changes
	solarized-use-variable-pitch nil
	;; Underline below the font bottomline instead of the baseline
	x-underline-at-descent-line t))

;; C-specific Indentation
(setq c-default-style "linux"
      c-basic-offset 4)

;; Electric pairing and code layout
(use-package electric-layout
  :init (electric-layout-mode))

(use-package electric-pair
  :init (electric-pair-mode))

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Exclude some directories in grep
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;;; Calendar
(use-package calendar
  :defer t
  :config
  ;; In Europe we start on Monday
  (setq calendar-week-start-day 1))

;;; Documentation
(use-package info
  :defer t
  :config
  ;; Fix the stupid `Info-quoted' face. Courier is an abysmal face, so go back
  ;; to the default face.
  (set-face-attribute 'Info-quoted nil :family 'unspecified
		      :inherit font-lock-constant-face))

;; Let apropos commands perform more extensive searches than default
(setq apropos-do-all t)

;; Better ediff behavior
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Browse URLs with eww
(use-package browse-url
  :defer t
  :config (setq browse-url-browser-function #'eww-browse-url))

;; BROWSE-KILL-RING
(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . browse-kill-ring)))

;; AG
;; Requires: silversearcher-ag
(use-package ag
  :ensure t
  :config
  (setq ag-reuse-buffers t ; Don't spam buffer list with ag buffers
	ag-highlight-search t ; A little fanciness
	;; Use Projectile to find the project root
	ag-project-root-function (lambda (d) (let ((default-directory d))
					       (projectile-project-root)))))
(use-package wgrep
  :ensure t
  :defer t)

(use-package wgrep-ag
  :ensure t
  :defer t)

;; PROJECTILE
(use-package projectile
  :ensure t
  :defer t
  :init (projectile-global-mode)
  :config
  (progn
    (setq projectile-completion-system 'ido
	  projectile-find-dir-includes-top-level t)

    ;; Replace Ack with Ag in Projectile commander
    (def-projectile-commander-method ?a
      "Find ag on project."
      (call-interactively 'projectile-ag))))

;; Group buffers by Projectile project
(use-package ibuffer-projectile
  :ensure t
  :defer t
  :init
  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-projectile-set-filter-groups)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic)))))

;; PAGE BREAK LINES
(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode)
  :defer page-break-lines-modes)

;; FILL COLUMN INDICATOR
(use-package fill-column-indicator
  :ensure t
  :defer t)

;; SMARTSCAN MODE
(use-package smartscan
  :ensure t
  :defer t
  :init (global-smartscan-mode 1))

;; ACE-JUMP-MODE
(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-c j" . ace-jump-mode-pop-mark))
  :config
  ;; Sync marks with Emacs built-in commands
  (ace-jump-mode-enable-mark-sync))

;; EASY-KILL
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
	 ([remap mark-sexp] . easy-mark)))

;; IEDIT
(use-package iedit
  :ensure t)

;; EXPAND-REGION
(use-package expand-region
  :ensure t
  :bind (("M-2" . er/expand-region)))

;; FLX-ISEARCH
(use-package flx-isearch
  :ensure t
  :bind (("C-M-s" . flx-isearch-forward)
	 ("C-M-r" . flx-isearch-backward)))

;; ADAPTIVE-WRAP
(use-package adaptive-wrap
  :ensure t)

;; Mode line
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
			       "my-keys-mode"
			       "PgLn"
			       "company"
			       "Undo-Tree")
			     "\\|")))
    (sml/setup)
    (sml/apply-theme 'automatic)))

;;; 03-style.el ends here
