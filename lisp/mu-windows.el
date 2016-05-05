;;; mu-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my windows customizations and utilities.

;;; Code:

(setq window-combination-resize t)      ; Size new windows proportionally

(use-package winner                     ; Undo and redo window configurations
  :init (winner-mode))

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
  :bind (("C-c t g" . mu-toggle-golden-ratio))
  :config
  (setq golden-ratio-extra-commands '(windmove-up
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

(use-package ace-window                 ; Better movements between windows
  :ensure t
  :bind (("C-x o"   . ace-window)
         ("C-c w w" . ace-window)
         ("C-c w s" . ace-swap-window))
  :config
  (setq aw-keys                 ; Use home row
        '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always t)

  ;; Make leading char more visible
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground "red" :weight normal :height 1.5))))))

(use-package ediff-wind                 ; Ediff window management
  :defer t
  :config
  ;; Prevent Ediff from spamming the frame
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

;; Standard window commands
(bind-keys
 ("C-c w =" . balance-windows)
 ("C-c w k" . delete-window)
 ("C-c w /" . split-window-right)
 ("C-c w -" . split-window-below)
 ("C-c w m" . delete-other-windows))

(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("C-c w <left>"  . windmove-left)
         ("C-c w <right>" . windmove-right)
         ("C-c w <up>"    . windmove-up)
         ("C-c w <down>"  . windmove-down)))

;;; Utilities and keybindings
(defun mu--quit-side-windows (pos)
  "Quit windows on the POS side of the current frame."
  (dolist (window (window-at-side-list nil pos))
    (when (window-live-p window)
      (quit-window nil window)
      ;; When the window is still live, delete it
      (when (window-live-p window)
        (delete-window window)))))

;;;###autoload
(defun mu-quit-bottom-side-windows ()
  "Quit bottom side windows."
  (interactive)
  (mu--quit-side-windows 'bottom))

;;;###autoload
(defun mu-quit-right-side-windows ()
  "Quit right side windows."
  (interactive)
  (mu--quit-side-windows 'right))

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

(bind-key "C-c w b" #'mu-quit-bottom-side-windows)
(bind-key "C-c w r" #'mu-quit-right-side-windows)
(bind-key "C-c w d" #'mu-toggle-current-window-dedication)

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

;;; mu-windows.el ends here
