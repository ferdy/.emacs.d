;;; mu-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my windows customizations and utilities.

;;; Code:

(validate-setq window-combination-resize t) ; Size new windows proportionally

(use-package eyebrowse                  ; Easy workspaces creation and switching
  :ensure t
  :bind (("C-c C-è" . eyebrowse-prev-window-config)
         ("C-c C-+" . eyebrowse-next-window-config))
  :init (eyebrowse-mode t)
  :config
  (validate-setq eyebrowse-mode-line-separator " "
                 eyebrowse-mode-line-style 'always
                 eyebrowse-new-workspace t
                 eyebrowse-wrap-around t))

(use-package golden-ratio               ; Automatically resize windows
  :ensure t
  :init
  (defun mu-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind ("C-c t g" . mu-toggle-golden-ratio)
  :config
  (validate-setq
   golden-ratio-extra-commands '(windmove-up
                                 windmove-down
                                 windmove-left
                                 windmove-right
                                 ace-window
                                 ace-delete-window
                                 ace-select-window
                                 ace-swap-window
                                 ace-maximize-window)
   ;; Exclude some modes from golden ratio
   golden-ratio-exclude-modes '(flycheck-error-list-mode
                                calc-mode
                                dired-mode
                                ediff-mode)
   ;; Exclude special buffers from golden ratio
   golden-ratio-exclude-buffer-regexp `(,(rx bos "*which-key*" eos)))
  :diminish golden-ratio-mode)

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
  :config
  (validate-setq aw-keys                 ; Use home row
                 '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                 aw-dispatch-always t))

(use-package ediff-wind                 ; Ediff window management
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (validate-setq
   ediff-window-setup-function #'ediff-setup-windows-plain
   ediff-split-window-function #'split-window-horizontally))

;;; Utilities and keybindings
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

(provide 'mu-windows)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-windows.el ends here
