;;; custom-keybindings.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the keybindings which are not strictly related to:
;; - a specific mode already configured with use-package;
;; - another custom-* file.

;;; Code:

;; Define prefix commands for my personal key binding groups.  Not specifically
;; important, but plays better with which-key, as it shows the names of prefix
;; commands in its popup
(defmacro custom/define-group (prefix name &optional map)
  "Define a group at PREFIX with NAME in MAP."
  (let ((command (intern (format "group:%s" name))))
    `(progn
       (define-prefix-command ',command)
       (bind-key ,prefix #',command ,map))))

(custom/define-group "C-c a" applications)
(custom/define-group "C-c a o" org)
(custom/define-group "C-c a S" stackexchange)
(custom/define-group "C-c a L" language)
(custom/define-group "C-c a w" eww)
(custom/define-group "C-c c" compile-and-comments)
(custom/define-group "C-c e" errors)
(custom/define-group "C-c f" files)
(custom/define-group "C-c h" helm)
(custom/define-group "C-c i" insertion)
(custom/define-group "C-c m" major-mode)
(custom/define-group "C-c n" navigation)
(custom/define-group "C-c o" multiple-cursors)
(custom/define-group "C-c p" projects)
(custom/define-group "C-c s" search-and-symbols)
(custom/define-group "C-c t" toggles)
(custom/define-group "C-c v" version-control)
(custom/define-group "C-c w" windows-and-frames)
(custom/define-group "C-c x" text)
(custom/define-group "C-c x a" align)

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

(bind-key "M-=" 'count-words) ; Use count-words instead of count-words-region
(bind-key "C-z" 'repeat) ; C-z for repeat (usually C-x z)

;; Keybindings I do not want to be overridden by a majore mode
(bind-keys* ("M-a"     . custom/backward-paragraph)
            ("M-e"     . custom/forward-paragraph)
            ("C-c o"   . (lambda ()
                           (interactive)
                           (find-file "~/org/organizer.org"))))

(provide 'custom-keybindings)

;;; custom-keybindings ends here
