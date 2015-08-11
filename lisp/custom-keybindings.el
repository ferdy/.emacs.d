;;; custom-keybindings.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores all the keybindings I use which are not strictly related to
;; a specific mode already configured with use-package.

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

;; Better forward and backward paragraph
(bind-key "M-a" 'custom/backward-paragraph)
(bind-key "M-e" 'custom/forward-paragraph)

;; Better window movings
(bind-key "C-x C-n" 'other-window)
(bind-key "C-x C-p" 'other-window-backward)

(bind-key "C-x C-e" 'pp-eval-last-sexp) ; Pretty-print evaluated expression
(bind-key "C-x C-d" 'duplicate-line) ; Duplicate line at point
(bind-key "C-c q" #'custom/quit-bottom-side-windows) ; Close side frames
(bind-key "M-Q" #'unfill-paragraph) ; The opposite of fill-paragraph
(bind-key "C-c C" #'custom/copy-filename-as-kill) ; Copy current file name

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

(setq next-line-add-newlines t) ; C^n adds new line when at the end of a line

(bind-key "M-g" 'goto-line) ; Goto line is M-g
(bind-key "M-=" 'count-words) ; Use count-words instead of count-words-region
(bind-key "C-x C-k" 'kill-this-buffer) ; Kill only the current buffer
(bind-key "C-;" #'comment-line) ; Comment-line
(bind-key "C-z" 'repeat) ; C-z for repeat (usually C-x z)
(bind-key "C-c M-o" #'comint-clear-buffer comint-mode-map) ; Clear comint buffer
(bind-key [remap isearch-delete-char]
          #'custom/isearch-delete isearch-mode-map) ; Better backspace in isearch

;; Better mark commands
(bind-key "C-+" 'push-mark-no-activate)
(bind-key "M-+" 'jump-to-mark)
(bind-key [remap exchange-point-and-mark]
          'exchange-point-and-mark-no-activate global-map)

;; Kill entire line with prefix argument
(bind-key [remap sp-kill-hybrid-sexp] (bol-with-prefix sp-kill-hybrid-sexp))
(bind-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(bind-key [remap kill-line] (bol-with-prefix kill-line))
(bind-key "C-k" (bol-with-prefix kill-visual-line))

;; Better shrink/enlarge windows
(bind-key "C-S-<up>" 'enlarge-window)
(bind-key "C-S-<down>" 'shrink-window)
(bind-key "C-S-<left>" 'shrink-window-horizontally)
(bind-key "C-S-<right>" 'enlarge-window-horizontally)

(bind-keys* ; Keybindings I do not want to be overridden by a majore mode
 ("M-a"     . custom/backward-paragraph)
 ("M-e"     . custom/forward-paragraph)
 ("C-c o"   . (lambda ()
                (interactive)
                (find-file "~/org/organizer.org")))
 ("C-c M-s" . ag))

(provide 'custom-keybindings)

;;; custom-keybindings ends here
