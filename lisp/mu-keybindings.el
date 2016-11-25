;;; mu-keybindings.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
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
  (validate-setq
   which-key-idle-delay 0.4
   which-key-sort-order 'which-key-prefix-then-key-order)

  ;; Prettify some symbols
  (add-to-list 'which-key-replacement-alist '(("up" . nil) . ("↑" . nil)))
  (add-to-list 'which-key-replacement-alist '(("right" . nil) . ("→" . nil)))
  (add-to-list 'which-key-replacement-alist '(("down" . nil) . ("↓" . nil)))
  (add-to-list 'which-key-replacement-alist '(("left" . nil) . ("←" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⌫" . nil)))
  (add-to-list 'which-key-replacement-alist
               '(("deletechar" . nil) . ("⌦" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))

  (which-key-add-key-based-replacements
    "C-c !"     "flycheck"
    "C-c @"     "outline"
    "C-c 8"     "typo"
    "C-c 8 -"   "typo/dashes"
    "C-c 8 <"   "typo/left-brackets"
    "C-c 8 >"   "typo/right-brackets"
    "C-c a"     "applications"
    "C-c a a"   "admin"
    "C-c a L"   "languages"
    "C-c a L l" "langtool"
    "C-c a L t" "translations"
    "C-c a m"   "math"
    "C-c a r"   "remote"
    "C-c a s"   "shells"
    "C-c a S"   "sx"
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
    "C-c o"     "org"
    "C-c n l"   "links"
    "C-c p"     "projects"
    "C-c p s"   "projects/search"
    "C-c p x"   "projects/execute"
    "C-c p 4"   "projects/other-window"
    "C-c s"     "search-and-symbols"
    "C-c t"     "toggles"
    "C-c v"     "version-control"
    "C-c w"     "windows-and-frames"
    "C-c x"     "text"
    "C-c x a"   "align")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c TAB" "md/images"
    "C-c C-a" "md/links"
    "C-c C-c" "md/process"
    "C-c C-s" "md/style"
    "C-c C-t" "md/header"
    "C-c C-x" "md/structure")

  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c m"   "elisp/personal"
    "C-c m m" "macroexpand"
    "C-c m e" "eval"
    "C-c m r" "refs"
    "C-c m s" "el-search")

  (which-key-add-major-mode-key-based-replacements 'scheme-mode
    "C-c m"  "scheme/personal")

  (which-key-add-major-mode-key-based-replacements 'sly-mode
    "C-c m"  "clisp/personal")

  (which-key-add-major-mode-key-based-replacements 'clojure-mode
    "C-c m"   "clj/personal"
    "C-c m r" "clj-refactor")

  (which-key-add-major-mode-key-based-replacements 'haskell-mode
    "C-c m" "haskell/personal"
    "C-c m i" "haskell/imports")

  (which-key-add-major-mode-key-based-replacements 'rust-mode
    "C-c C-c" "rust/cargo")

  (which-key-add-major-mode-key-based-replacements 'scala-mode
    "C-c C-b" "ensime/build"
    "C-c C-d" "ensime/debug"
    "C-c C-r" "ensime/refactor"
    "C-c C-v" "ensime/misc"
    "C-c m"   "scala/personal"
    "C-c m b" "scala/build")

  (which-key-add-major-mode-key-based-replacements 'js2-mode
    "C-c m"   "js/personal"
    "C-c m r" "refactor")

  (which-key-add-major-mode-key-based-replacements 'web-mode
    "C-c C-a" "web/attributes"
    "C-c C-b" "web/blocks"
    "C-c C-d" "web/dom"
    "C-c C-e" "web/element"
    "C-c C-t" "web/tags")

  (which-key-add-major-mode-key-based-replacements 'ledger-mode
    "C-c m"   "ledger/personal"
    "C-c m b" "clean-buffer")
  :diminish which-key-mode)

(use-package hydra                      ; Make bindings that stick
  :ensure t)

(bind-key "M-=" 'count-words)    ; Use count-words instead of count-words-region
(bind-key "C-z" 'undo)           ; C-z for undo
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

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-keybindings ends here
