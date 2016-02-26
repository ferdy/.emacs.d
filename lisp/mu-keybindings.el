;;; mu-keybindings.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores keybindings that are not strictly related to:
;; - a specific mode already configured with use-package;
;; - another mu-* file.

;;; Code:

;; Define prefix commands for my personal key binding groups.  Not specifically
;; important, but plays better with which-key, as it shows the names of prefix
;; commands in its popup
(defmacro mu-define-group (prefix name &optional map)
  "Define a group at PREFIX with NAME in MAP."
  (let ((command (intern (format "group:%s" name))))
    `(progn
       (define-prefix-command ',command)
       (bind-key ,prefix #',command ,map))))

(mu-define-group "C-c a" applications)
(mu-define-group "C-c a a" admin)
(mu-define-group "C-c a o" org)
(mu-define-group "C-c a L" language)
(mu-define-group "C-c a L l" langtool)
(mu-define-group "C-c a L t" translations)
(mu-define-group "C-c a m" math)
(mu-define-group "C-c a r" remote)
(mu-define-group "C-c a s" shells)
(mu-define-group "C-c a t" time-and-date)
(mu-define-group "C-c a w" eww)
(mu-define-group "C-c b" buffers)
(mu-define-group "C-c c" compile-and-comments)
(mu-define-group "C-c d" develop)
(mu-define-group "C-c e" errors)
(mu-define-group "C-c e v" evalator)
(mu-define-group "C-c f" files)
(mu-define-group "C-c i" insertion)
(mu-define-group "C-c m" major-mode)
(mu-define-group "C-c n" navigation)
(mu-define-group "C-c n l" links)
(mu-define-group "C-c n m" marks)
(mu-define-group "C-c o" multiple-cursors)
(mu-define-group "C-c p" projects)
(mu-define-group "C-c s" search-and-symbols)
(mu-define-group "C-c t" toggles)
(mu-define-group "C-c v" version-control)
(mu-define-group "C-c w" windows-and-frames)
(mu-define-group "C-c x" text)
(mu-define-group "C-c x a" align)

(use-package which-key                  ; Show help popups for prefix keys
  :ensure t
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.5
                which-key-key-replacement-alist
                '(("<\\([[:alnum:]-]+\\)>" . "\\1")
                  ("up"                    . "↑")
                  ("right"                 . "→")
                  ("down"                  . "↓")
                  ("left"                  . "←")
                  ("DEL"                   . "⌫")
                  ("deletechar"            . "⌦")
                  ("RET"                   . "⏎")))
  :diminish which-key-mode)

(bind-key "M-=" 'count-words)    ; Use count-words instead of count-words-region
(bind-key "C-z" 'repeat)         ; C-z for repeat (usually C-x z)

;; These keybindings make it easier to type curly braces and square brackets
;; with an Italian keyboard layout
(defun mu-insert-pair (pair)
  "Insert PAIR.
If PAIR is an opening pair, the closing pair will be inserted as well."
  (insert pair)
  (sp-insert-pair))

(bind-key "C-è" (lambda () (interactive) (mu-insert-pair "[")))
(bind-key "C-é" (lambda () (interactive) (mu-insert-pair "]")))
(bind-key "C-à" (lambda () (interactive) (mu-insert-pair "{")))
(bind-key "C-°" (lambda () (interactive) (mu-insert-pair "}")))

(provide 'mu-keybindings)

;;; mu-keybindings ends here
