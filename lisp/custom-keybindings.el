;;; custom-keybindings.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the keybindings which are not strictly related to:
;; - a specific mode already configured with use-package;
;; - another custom-* file.

;;; Code:

(use-package which-key ; Show help popups for prefix keys
  :ensure t
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.5
                which-key-key-replacement-alist
                '(("<\\([[:alnum:]-]+\\)>" . "\\1")
                  ("up"                  . "↑")
                  ("right"               . "→")
                  ("down"                . "↓")
                  ("left"                . "←")
                  ("DEL"                 . "⌫")
                  ("deletechar"          . "⌦")
                  ("RET"                 . "⏎")))
  :diminish which-key-mode)

(defun revert-this-buffer ()
  "Revert current buffer without asking for confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

;; Custom keybindings activated with C^x t
(define-prefix-command 'toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(bind-keys :map ctl-x-map ("t" . toggle-map))
(bind-keys :map toggle-map
           ("v" . visual-line-mode)
           ("l" . linum-mode)
           ("s" . scratch)
           ("r" . revert-this-buffer)
           ("w" . writeroom-mode))

(bind-key "M-=" 'count-words) ; Use count-words instead of count-words-region
(bind-key "C-z" 'repeat) ; C-z for repeat (usually C-x z)

;; Better shrink/enlarge windows
(bind-key "C-S-<up>" 'enlarge-window)
(bind-key "C-S-<down>" 'shrink-window)
(bind-key "C-S-<left>" 'shrink-window-horizontally)
(bind-key "C-S-<right>" 'enlarge-window-horizontally)

;; Keybindings I do not want to be overridden by a majore mode
(bind-keys* ("M-a"     . custom/backward-paragraph)
            ("M-e"     . custom/forward-paragraph)
            ("C-c o"   . (lambda ()
                           (interactive)
                           (find-file "~/org/organizer.org")))
            ("C-c M-s" . ag))

(provide 'custom-keybindings)

;;; custom-keybindings ends here
