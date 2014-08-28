;; style.el
;;
;; This file stores the interface customizations.

;; set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; set font fallback
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 11.4
                               :weight 'normal)))

;; turn off blinking cursor
(blink-cursor-mode 0)

;; turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; undo scrolling
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

;; disable annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; disable splash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; disable scratch buffer message
(setq initial-scratch-message nil)

;; tooltips in the echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; linum+ for better line numbers
(add-to-list 'load-path "~/.emacs.d/various")
(require 'linum+)
(setq linum+-dynamic-format " %%%dd")

;; linum+ resets linum-format to "smart" when it's loaded, hence we have to
;; use a eval-after-load hook to set it to "dynamic".
(eval-after-load "linum+" '(progn (setq linum-format 'dynamic)))

;; turn on ido-mode for better buffers switching
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode 1)

;; set unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;; faster echo keystrokes
;; see http://endlessparentheses.com/faster-keystroke-echo.html
(setq echo-keystrokes 0.1)

;; set the directory where all backup and autosave files will be saved
(defvar backup-dir "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))

;; set solarized theme
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
(require 'solarized-dark-theme)

(if (daemonp)
(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f) (load-theme 'solarized-dark t)))))
(load-theme 'solarized-dark t))

;; functions to remove background when on terminals
(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
    (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))

(add-hook 'after-make-frame-functions 'on-frame-open)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

;; C-specific Indentation
(setq c-default-style "linux"
      c-basic-offset 4)

;; delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
