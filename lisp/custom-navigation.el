;;; custom-navigation.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for in-buffer, frames and windows
;; navigation.

;;; Code:

;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 1000
      ;; Ensure M-v always undoes C-v
      scroll-preserve-screen-position 'always)

(use-package winner ; Undo and redo window configurations
  :init (winner-mode))

(use-package avy-jump ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j w" . avy-goto-word-1)
         ("C-c j j" . avy-goto-char-2))
  :config (setq avy-keys ; Use home row
                '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?v ?r ?u)))

(use-package ace-link ; Jump to links
  :ensure t
  :defer t
  :init (progn (with-eval-after-load 'info
                 (bind-key "C-c j l" #'ace-link-info Info-mode-map))
               (with-eval-after-load 'help-mode
                 (defvar help-mode-map) ; Silence the byte compiler
                 (bind-key "C-c j l" #'ace-link-help help-mode-map))))

(use-package ace-window ; Better movements between windows
  :ensure t
  :bind (("C-x o"   . ace-window)
         ("C-c n s" . ace-swap-window))
  :config (setq aw-keys ; Use home row
                '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-dispatch-always t))

(use-package elisp-slime-nav ; Navigate through elisp code with M-. & M-,
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

;;; Keybindings
;; Better forward and backward paragraph
(bind-key "M-a" 'custom/backward-paragraph)
(bind-key "M-e" 'custom/forward-paragraph)

;; Better window movings
(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(bind-key "C-x C-n" 'other-window)
(bind-key "C-x C-p" 'other-window-backward)

(bind-key "M-g" 'goto-line) ; Goto line is M-g

(defun custom/quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

(bind-key "C-c q" #'custom/quit-bottom-side-windows) ; Close side frames

;; Better mark commands
(defun push-mark-no-activate ()
  "Pushes 'point' to 'mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(bind-key "C-+" 'push-mark-no-activate)
(bind-key "M-+" 'jump-to-mark)
(bind-key [remap exchange-point-and-mark]
          'exchange-point-and-mark-no-activate global-map)

(provide 'custom-navigation)

;;; custom-navigation.el ends here
