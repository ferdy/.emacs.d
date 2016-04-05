;;; mu-keybindings.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores keybindings that are not strictly related to:
;; - a specific mode already configured with use-package;
;; - another mu-* file.

;;; Code:

(use-package which-key                  ; Show help popups for prefix keys
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎")))

  (which-key-declare-prefixes
    "C-c !"     "flycheck"
    "C-c @"     "outline"
    "C-c 8"     "typo"
    "C-c 8 -"   "typo/dashes"
    "C-c 8 <"   "typo/left-brackets"
    "C-c 8 >"   "typo/right-brackets"
    "C-c a"     "applications"
    "C-c a a"   "admin"
    "C-c a o"   "org"
    "C-c a L"   "languages"
    "C-c a L l" "langtool"
    "C-c a L t" "translations"
    "C-c a m"   "math"
    "C-c a r"   "remote"
    "C-c a s"   "shells"
    "C-c a t"   "time-and-date"
    "C-c a w"   "eww"
    "C-c b"     "buffers"
    "C-c c"     "compile-and-comments"
    "C-c d"     "develop"
    "C-c e"     "errors"
    "C-c e v"   "evalator"
    "C-c f"     "files"
    "C-c i"     "insertion"
    "C-c m"     "major mode"
    "C-c n"     "navigation"
    "C-c n l"   "links"
    "C-c o"     "cursors"
    "C-c p"     "projects"
    "C-c s"     "search-and-symbols"
    "C-c t"     "toggles"
    "C-c v"     "version-control"
    "C-c w"     "windows-and-frames"
    "C-c x"     "text"
    "C-c x a"   "align")

  (which-key-declare-prefixes-for-mode 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure")

  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode
    "C-c m"   "elisp/personal"
    "C-c m e" "eval")
  :diminish which-key-mode)

(bind-key "M-=" 'count-words)    ; Use count-words instead of count-words-region
(bind-key "C-z" 'repeat)         ; C-z for repeat (usually C-x z)
(bind-key "C-c l" 'find-library) ; Go to the source code of the given library

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
