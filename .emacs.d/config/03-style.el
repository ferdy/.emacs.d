;;;; 03-style.el
;;; This file stores the interface customizations.

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
(modify-all-frames-parameters '((fullscreen . maximized)))

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

;; Tooltips in the echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

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

;; Helm setup
(require 'helm-config)
(helm-mode 1)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; C-h is delete-backward-char, don't touch it!
(eval-after-load "helm-files"
  '(let ((helm-find-files-C-h-map (lookup-key helm-find-files-map (kbd "C-h"))))
     ;; make sure C-h is no longer a prefix key
     (define-key helm-find-files-map (kbd "C-h") nil)
     ;; rebind "C-h ..." to "M-m ..." to preserve functionality
     (define-key helm-find-files-map (kbd "M-m") helm-find-files-C-h-map)))

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(global-set-key (kbd "C-c h o") 'helm-occur)

(setq helm-apropos-function-list t)

(require 'helm-eshell)
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))

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

;; Mode line setup
;; See http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(setq-default
 mode-line-format
 '(;; position
   "%4l:"
   "%3c"
   "  "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           " RO ")
          ((buffer-modified-p)
           " ** ")
          (t "    ")))
   "  "
   ;; directory and buffer/file name
   (:eval
    (when (= (length (window-list)) 1)
      (shorten-directory default-directory 30)))
   "%b"
   "    "
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   mode-name
   "%] "
   ;; (:eval
   ;;  (when (= (length (window-list)) 1)
   ;;    (format-mode-line minor-mode-alist)))
   mode-line-process
   (global-mode-string global-mode-string)
   "    "
   "%p/%I"
   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "../" output)))
    output))
