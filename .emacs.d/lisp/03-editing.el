;;; 03-editing.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:
;; This file stores editing and searching customizations.

;;; Code:
;; Scrolling
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

;; View read-only
(setq view-read-only t)

;; Delete the selection instead of insert
(use-package delsel
  :defer t
  :init (delete-selection-mode))

;; Subword/superword editing
(use-package subword
  :defer t)

;; Set the directory where all backup and autosave files will be saved
(defvar backup-dir "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Exclude some directories in grep
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "elpa")))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;; AG
;; Requires: silversearcher-ag
(use-package ag
  :ensure t
  :config
  (setq ag-reuse-buffers t ; Don't spam buffer list with ag buffers
	ag-highlight-search t))

(use-package wgrep
  :ensure t
  :defer t)

(use-package wgrep-ag
  :ensure t
  :defer t)

;; EASY-KILL
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
	 ([remap mark-sexp] . easy-mark)))

;; IEDIT
(use-package iedit
  :ensure t
  :config
  (progn
    (defun iedit-dwim (arg)
      "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
      (interactive "P")
      (if arg
	  (iedit-mode)
	(save-excursion
	  (save-restriction
	    (widen)
	    ;; this function determines the scope of `iedit-start'.
	    (if iedit-mode
		(iedit-done)
	      ;; `current-word' can of course be replaced by other
	      ;; functions.
	      (narrow-to-defun)
	      (iedit-start (current-word) (point-min) (point-max)))))))

    (global-set-key (kbd "C-,") 'iedit-dwim)))

;; EXPAND-REGION
(use-package expand-region
  :ensure t
  :bind (("M-2" . er/expand-region)))

;; FLX-ISEARCH
(use-package flx-isearch
  :ensure t
  :bind (("C-M-s" . flx-isearch-forward)
	 ("C-M-r" . flx-isearch-backward)))

;; VISUAL-REGEXP
(use-package visual-regexp
  :ensure t
  :bind (("C-c r" . vr/query-replace)
	 ("C-c R" . vr/replace)))

;; ADAPTIVE-WRAP
(use-package adaptive-wrap
  :ensure t)

;; ANZU
(use-package anzu
  :ensure t
  :init (global-anzu-mode)
  :config
  (progn
    (setq anzu-cons-mode-line-p nil
	  anzu-mode-lighter "")
    (setcar (cdr (assq 'isearch-mode minor-mode-alist))
	    '(:eval (anzu--update-mode-line)))))

;; RAINBOW DELIMITERS
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
	  (add-hook hook #'rainbow-delimiters-mode)))

;; AGGRESSIVE INDENT
(use-package aggressive-indent
  :ensure t
  :init (global-aggressive-indent-mode 1)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

;; HUNGRY DELETE
(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode))

;; BROWSE-KILL-RING
(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . browse-kill-ring)))

;; Better ediff behavior
(use-package ediff-wind
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
	ediff-split-window-function #'split-window-horizontally))

;; C-specific Indentation
(setq c-default-style "linux"
      c-basic-offset 4)

;; ELECTRIC LAYOUT
(use-package electric
  :init (electric-layout-mode))

;; ELECTRIC PAIR
(use-package elec-pair
  :init (electric-pair-mode))

;; MULTIPLE CURSORS
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m e" . mc/mark-more-like-this-extended)
	 ("C-c m h" . mc/mark-all-like-this-dwim)
	 ("C-c m l" . mc/edit-lines)
	 ("C-c m n" . mc/mark-next-like-this)
	 ("C-c m p" . mc/mark-previous-like-this)
	 ("C-c m r" . vr/mc-mark)
	 ("C-c m C-a" . mc/edit-beginnings-of-lines)
	 ("C-c m C-e" . mc/edit-ends-of-lines)
	 ("C-c m C-s" . mc/mark-all-in-region))
  :config
  (setq mc/mode-line
	;; Simplify the MC mode line indicator
	'(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
		      face font-lock-warning-face)))

;; MULTIFILES
(use-package multifiles
  :ensure t
  :defer t
  :bind (("C-!" . mf/mirror-region-in-multifile)))

;; MACROSTEP
(use-package macrostep
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'lisp-mode
    (bind-key "C-c e" #'macrostep-expand emacs-lisp-mode-map)
    (bind-key "C-c e" #'macrostep-expand lisp-interaction-mode-map)))

;; Save point position in files
(use-package saveplace
  :config (setq-default save-place t))

;; Auto-revert buffers of changed files
(use-package autorevert
  :init (global-auto-revert-mode))

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

;; ELISP-SLIME-NAV
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode))

;; ZOP-TO-CHAR
(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(provide '03-editing)

;;; 03-editing.el ends here
