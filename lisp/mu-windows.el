;;; mu-windows.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my windows customizations and utilities.

;;; Code:

(setq window-combination-resize t) ; Size new windows proportionally

(use-package fullframe                 ; Generalized execution in a single frame
  :ensure t
  :defer t)

(use-package eyebrowse                  ; Easy workspaces creation and switching
  :ensure t
  :init (eyebrowse-mode t)
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-mode-line-style 'always
        eyebrowse-new-workspace t
        eyebrowse-wrap-around t))

(use-package windmove                   ; Quickly move between windows
  :bind (("C-c <up>"    . windmove-up)
         ("C-c <down>"  . windmove-down)
         ("C-c <left>"  . windmove-left)
         ("C-c <right>" . windmove-right)))

(use-package ace-window                 ; Better movements between windows
  :ensure t
  :bind (("C-x o"   . ace-window)
         ("C-c w w" . ace-window)
         ("C-c w s" . ace-swap-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                ;; Ignore case
                aw-translate-char-function #'downcase))

(use-package ediff-wind                 ; Ediff window management
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

;;; Utilities and key bindings
(defun mu-find-side-windows (&optional side)
  "Get all side window if any.
If SIDE is non-nil only get windows on that side."
  (let (windows)
    (walk-window-tree
     (lambda (window)
       (let ((window-side (window-parameter window 'window-side)))
         (when (and window-side (or (not side) (eq window-side side)))
           (push window windows)))))
    windows))

;;;###autoload
(defun mu-quit-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (mu-find-side-windows))
    (when (window-live-p window)
      (quit-window nil window)
      ;; When the window is still live, delete it
      (when (window-live-p window)
        (delete-window window)))))

;;;###autoload
(defun mu-switch-to-minibuffer-window ()
  "Switch to current minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;;;###autoload
(defun mu-toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(bind-key "C-c w q" #'mu-quit-side-windows)
(bind-key "C-c w d" #'mu-toggle-current-window-dedication)
(bind-key "C-c w b" #'mu-switch-to-minibuffer-window)

;; Better shrink/enlarge windows
(bind-keys*
 ("C-S-<up>"    . enlarge-window)
 ("C-S-<down>"  . shrink-window)
 ("C-S-<left>"  . shrink-window-horizontally)
 ("C-S-<right>" . enlarge-window-horizontally))

;;;###autoload
(defun mu-window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(bind-key "C-c w t" #'mu-window-split-toggle)

(defvar mu-saved-window-configuration nil)

(defun mu-save-wins-then-call (func &optional args)
  "Save current window configuration, then call FUNC optionally with ARGS."
  (interactive)
  (push (current-window-configuration) mu-saved-window-configuration)
  (cond
   ;; We have arguments for the function
   ((bound-and-true-p args) (funcall func args))
   ;; The function expects exactly one argument, and we want it to be nil
   ((equal args "nil") (funcall func nil))
   ;; The function does not expect arguments
   (t (funcall func))))

(defun mu-restore-window-configuration (config)
  "Kill current buffer and restore the window configuration in CONFIG."
  (interactive)
  (kill-this-buffer)
  (set-window-configuration config))

(defun mu-pop-window-configuration ()
  "Restore the previous window configuration and clear current window."
  (interactive)
  (let ((config (pop mu-saved-window-configuration)))
    (if config
        (mu-restore-window-configuration config)
      (if (> (length (window-list)) 1)
          (delete-window)
        (bury-buffer)))))

(provide 'mu-windows)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-windows.el ends here
