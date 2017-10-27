;;; mu-style.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my windows customizations and utilities.

;;; Code:

(validate-setq window-combination-resize t) ; Size new windows proportionally

(use-package eyebrowse                  ; Easy workspaces creation and switching
  :ensure t
  :init (eyebrowse-mode t)
  :config
  (validate-setq
   eyebrowse-mode-line-separator " "
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
  :config (validate-setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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

;;;###autoload
(defun mu-ediff-dwim ()
  "Do ediff as I mean.

If a region is active, call `ediff-regions-wordwise'.
Else if the frame has 2 windows with identical major modes,
  - Do `ediff-files' if the buffers are associated to files and the buffers
    have not been modified.
  - Do `ediff-buffers' otherwise.
Else if the current is a file buffer with a VC backend, call `vc-ediff'
Else call `ediff-buffers'."
  (interactive)
  (let* ((num-win (safe-length (window-list)))
         (bufa (get-buffer (buffer-name)))
         (filea (buffer-file-name bufa))
         (modea (with-current-buffer bufa major-mode))
         bufb fileb modeb)
    (save-excursion
      (other-window 1)
      (setq bufb (get-buffer (buffer-name)))
      (setq fileb (buffer-file-name bufb))
      (setq modeb (with-current-buffer bufb major-mode)))
    (cond
     ;; If a region is selected
     ((region-active-p)
      (call-interactively #'ediff-regions-wordwise))
     ;; Else if 2 windows with same major modes
     ((and (= 2 num-win)
           (eq modea modeb))
      (if ;; If either of the buffers is not associated to a file,
          ;; or if either of the buffers is modified
          (or (null filea)
              (null fileb)
              (buffer-modified-p bufa)
              (buffer-modified-p bufb))
          (progn
            (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
            (ediff-buffers bufa bufb))
        (progn
          (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
          (ediff-files filea fileb))))
     ;; Else if file in current buffer has a vc backend
     ((and filea
           (vc-registered filea))
      (call-interactively #'vc-ediff))
     ;; Else call `ediff-buffers'
     (t
      (call-interactively #'ediff-buffers)))))

(bind-key "C-c w e" #'mu-ediff-dwim)

(provide 'mu-windows)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-windows.el ends here
