;;; custom-navigation.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for in-buffer, frames and windows
;; navigation.

;;; Code:
;;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 1000
      ;; Ensure M-v always undoes C-v
      scroll-preserve-screen-position 'always)

(use-package winner ; Undo and redo window configurations
  :init (winner-mode))

(use-package avy-jump ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j s" . avy-isearch)
         ("C-c j w" . avy-goto-word-1)
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
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys ; Use home row
                '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-dispatch-always t))

(provide 'custom-navigation)

;;; custom-navigation.el ends here
