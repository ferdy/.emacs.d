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

;; Turn off blinking cursor
(blink-cursor-mode 0)

;; Toggle all frames maximized and fullscreen
;; (modify-all-frames-parameters '((fullscreen . maximized)))

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
      (error "Cannot unscroll yet."))
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
(global-visual-line-mode +1)

;; Linum+ for better line numbers
(add-to-list 'load-path "~/.emacs.d/various")
(require 'linum+)
(setq linum+-dynamic-format " %%%dd")

;; Linum+ resets linum-format to "smart" when it's loaded, hence we have to
;; use a eval-after-load hook to set it to "dynamic".
(eval-after-load "linum+" '(progn (setq linum-format 'dynamic)))

;; Turn on ido-mode for better buffers switching
(defvar ido-context-switch-command nil)
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

(ido-ubiquitous-mode +1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode 1)

;; Turn on ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; Turn on flx-ido for better search results
(require 'flx-ido)
(flx-ido-mode 1)
(setq flx-ido-use-faces nil)

;; Ido-charged version of imenu
;; See http://www.emacswiki.org/emacs/ImenuMode#toc13
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer not in SYMBOL-LIST using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  name-and-pos symbol-names position)
      (unless ido-mode
	(ido-mode 1)
	(setq ido-enable-flex-matching t))
      (while (progn
	       (imenu--cleanup)
	       (setq imenu--index-alist nil)
	       (ido-goto-symbol (imenu--make-index-alist))
	       (setq selected-symbol
		     (ido-completing-read "Symbol? " symbol-names))
	       (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	(push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
	(cond
	 ((and (listp symbol) (imenu--subalist-p symbol))
	  (ido-goto-symbol symbol))
	 ((listp symbol)
	  (setq name (car symbol))
	  (setq position (cdr symbol)))
	 ((stringp symbol)
	  (setq name symbol)
	  (setq position
		(get-text-property 1 'org-imenu-marker symbol))))
	(unless (or (null position) (null name)
		    (string= (car imenu--rescan-item) name))
	  (add-to-list 'symbol-names name)
	  (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "M-i") 'ido-goto-symbol)

;; Enable smex
;; See https://github.com/nonsequitur/smex
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Set unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;; Faster echo keystrokes
;; See http://endlessparentheses.com/faster-keystroke-echo.html
(setq echo-keystrokes 0.1)

;; Set the directory where all backup and autosave files will be saved
(defvar backup-dir "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))

;; Set solarized theme
;; Make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; Avoid all font-size changes
(setq solarized-use-variable-pitch nil)

;; Underline below the font bottomline instead of the baseline
(setq x-underline-at-descent-line t)

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

(add-hook 'window-setup-hook 'on-after-init)

;; C-specific Indentation
(setq c-default-style "linux"
      c-basic-offset 4)

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Exclude some directories in grep
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;; Set default browser
(setq gnus-button-url 'browse-url-generic
      browse-url-generic-program "firefox"
      browse-url-browser-function gnus-button-url)

;; Mode line
(sml/setup)
(sml/apply-theme 'automatic)

;; Let apropos commands perform more extensive searches than default
(setq apropos-do-all t)

;; Better ediff behavior
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; BROWSE-KILL-RING SETUP
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

;; AG SETUP
;; Require: silversearcher-ag
(setq ag-reuse-buffers t
      ag-highlight-search t)

;; PAGE BREAK LINES
(turn-on-page-break-lines-mode)

;; SMARTSCAN MODE
(global-smartscan-mode 1)

;;; 03-style.el ends here
