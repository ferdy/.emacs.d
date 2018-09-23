;;; mu-cursors.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for multiple-cursors.

;;; Code:

;; Free C-m and make it different from RET
(define-key input-decode-map [?\C-m] [C-m])

(use-package multiple-cursors        ; Easily place multiple cursors in a buffer
  :ensure t
  :bind (("C-'"         . set-rectangular-region-anchor)
         ("<C-m> ^"     . mc/edit-beginnings-of-lines)
         ("<C-m> $"     . mc/edit-ends-of-lines)
         ("<C-m> '"     . mc/edit-ends-of-lines)
         ("<C-m> R"     . mc/reverse-regions)
         ("<C-m> S"     . mc/sort-regions)
         ("<C-m> W"     . mc/mark-all-words-like-this)
         ("<C-m> Y"     . mc/mark-all-symbols-like-this)
         ("<C-m> a"     . mc/mark-all-like-this-dwim)
         ("<C-m> c"     . mc/mark-all-dwim)
         ("<C-m> l"     . mc/insert-letters)
         ("<C-m> n"     . mc/insert-numbers)
         ("<C-m> r"     . mc/mark-all-in-region-regexp)
         ("<C-m> t"     . mc/mark-sgml-tag-pair)
         ("<C-m> w"     . mc/mark-next-like-this-word)
         ("<C-m> x"     . mc/mark-more-like-this-extended)
         ("<C-m> y"     . mc/mark-next-like-this-symbol)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> ("     . mc/mark-all-symbols-like-this-in-defun)
         ("<C-m> C-("   . mc/mark-all-words-like-this-in-defun)
         ("<C-m> M-("   . mc/mark-all-like-this-in-defun)
         ("<C-m> ["     . mc/vertical-align-with-space)
         ("<C-m> {"     . mc/vertical-align))
  :bind (:map selected-keymap
              ("C-'" . mc/edit-lines)
              ("c"   . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this))
  :init (setq mc/mode-line
              ;; Simplify the MC mode line indicator
              '(:propertize (:eval (concat " " (number-to-string
                                                (mc/num-cursors))))
                            face font-lock-warning-face))
  :config (validate-setq mc/always-run-for-all t))

(use-package mc-extras                  ; Extra functions for multiple-cursors
  :ensure t
  :bind (("C-. M-C-f" . mc/mark-next-sexps)
         ("C-. M-C-b" . mc/mark-previous-sexps)
         ("C-. <"     . mc/mark-all-above)
         ("C-. >"     . mc/mark-all-below)
         ("C-. C-d"   . mc/remove-current-cursor)
         ("C-. C-k"   . mc/remove-cursors-at-eol)
         ("C-. M-d"   . mc/remove-duplicated-cursors)
         ("C-. |"     . mc/move-to-column)
         ("C-. ~"     . mc/compare-chars)))

(use-package mc-freeze
  :ensure mc-extras
  :bind ("C-. C-f" . mc/freeze-fake-cursors-dwim))

(use-package mc-rect
  :ensure mc-extras
  :bind ("C-\"" . mc/rect-rectangle-to-multiple-cursors))

(provide 'mu-cursors)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-cursors.el ends here
