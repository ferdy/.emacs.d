;;; custom-keybindings.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores all the keybindings I use.

;;; Code:

;; Better forward and backward paragraph
(global-set-key "\M-a" 'custom/backward-paragraph)
(global-set-key "\M-e" 'custom/forward-paragraph)

;; Better window movings
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backward)

(global-set-key (kbd "C-x C-d") 'duplicate-line) ; Duplicate line at point

(global-set-key (kbd "C-c q")
                #'custom/quit-bottom-side-windows) ; Close side frames

;; Custom keybindings activated with C^x t
(define-prefix-command 'toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(bind-keys :map ctl-x-map ("t" . toggle-map))
(bind-keys :map toggle-map
           ("v" . visual-line-mode)
           ("l" . linum-mode)
           ("s" . create-scratch-buffer)
           ("r" . revert-this-buffer)
           ("w" . writeroom-mode))

(global-set-key "\M-g" 'goto-line) ; Goto line is M-g

(global-set-key "\C-x\C-k" 'kill-this-buffer) ; Kill only the current buffer

(setq next-line-add-newlines t) ; C^n adds new line when at the end of a line

(global-set-key (kbd "C-;") #'comment-line) ; Comment-line

;; Better mark commands
(global-set-key (kbd "C-+") 'push-mark-no-activate)
(global-set-key (kbd "M-+") 'jump-to-mark)
(define-key global-map [remap exchange-point-and-mark]
  'exchange-point-and-mark-no-activate)

(global-set-key (kbd "C-z") 'repeat) ; C-z for repeat (usually C-x z)

(global-set-key "\C-x\M-o" 'occur-dwim) ; Global key for 'occur-dwim'

;; Kill entire line with prefix argument
(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key "\C-k" (bol-with-prefix kill-visual-line))

(define-key comint-mode-map "\C-c\M-o"
  #'comint-clear-buffer) ; Clear comint buffers

;; Better shrink/enlarge windows
(global-set-key (kbd "C-S-<up>") 'enlarge-window)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)

(define-key isearch-mode-map [remap isearch-delete-char]
  #'custom/isearch-delete) ; Better backspace in isearch

;; Minor mode for 'override' keybindings
(use-package my-keys-mode
  :load-path "various"
  :config (progn
            (bind-keys :map my-keys-mode-map
                       ("M-a" . custom/backward-paragraph)
                       ("M-e" . custom/forward-paragraph)
                       ("C-," . iedit-dwim)
                       ("C-c o" . (lambda ()
                                    (interactive)
                                    (find-file "~/org/organizer.org"))))
            (global-my-keys-mode)))

(provide 'custom-keybindings)

;;; custom-keybindings ends here
