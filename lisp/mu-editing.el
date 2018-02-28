;;; mu-editing.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for text editing tools.

;;; Code:

(prefer-coding-system 'utf-8)

(use-package electric                   ; Electric modes package
  :config (add-hook 'after-init-hook #'electric-indent-mode))

(use-package aggressive-indent          ; Automatically indent code
  :ensure t
  :bind ("C-c t i" . aggressive-indent-mode)
  :init (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook clojure-mode-hook))
          (add-hook hook #'aggressive-indent-mode))
  :config
  ;; Free C-c C-q, used in Org and in CIDER
  (unbind-key "C-c C-q" aggressive-indent-mode-map)

  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t w" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode)))

(use-package shrink-whitespace          ; Better whitespace removal
  :ensure t
  :bind ("M-SPC" . shrink-whitespace))

(use-package hungry-delete              ; Delete all whitespaces
  :ensure t
  :bind (("C-c <backspace>" . hungry-delete-backward)
         ("C-c <deletechar>" . hungry-delete-forward)))

(use-package undo-tree                  ; Show buffer changes as a tree
  :ensure t
  :init (global-undo-tree-mode)
  :config (validate-setq undo-tree-visualizer-timestamps t))

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package subword                    ; Handle capitalized subwords
  :defer t
  ;; Do not override `transpose-words', it should not transpose subwords
  :bind (:map subword-mode-map
              ([remap transpose-words] . nil))
  :init (global-subword-mode 1))

(use-package expand-region      ; Increase the selected region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package change-inner              ; Change contents based on semantic units
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package easy-kill                  ; Better kill text
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package adaptive-wrap              ; Better line wrap
  :ensure t
  :defer t
  :config
  (setq-default adaptive-wrap-extra-indent 2)

  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package aggressive-fill-paragraph  ; Automatically fill paragrah
  :ensure t
  :defer t
  :config
  (add-hook 'org-mode-hook #'aggressive-fill-paragraph-mode)
  (add-hook 'TeX-mode-hook #'aggressive-fill-paragraph-mode))

(use-package visual-fill-column         ; Fill column wrapping
  :ensure t
  :bind ("C-c t v" . visual-fill-column-mode)
  :init
  ;; Turn on whenever visual line mode is on, and in all text or prog mode
  ;; buffers to get centered text
  (dolist (hook '(visual-line-mode-hook
                  prog-mode-hook
                  text-mode-hook))
    (add-hook hook #'visual-fill-column-mode))
  :config
  ;; Split windows vertically despite large margins, because Emacs otherwise
  ;; refuses to vertically split windows with large margins
  (validate-setq split-window-preferred-function
                 #'visual-fill-column-split-window-sensibly))

(use-package align                      ; Align text in buffers
  :bind (("C-c x a a" . align)
         ("C-c x a c" . align-current)))

(use-package ialign                     ; Visual align-regexp
  :ensure t
  :bind ("C-c x a i" . ialign))

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
  :init
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string
                                          (mc/num-cursors))))
                      face font-lock-warning-face)))

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

(use-package saveplace                  ; Save point position in files
  :init (save-place-mode 1))

(use-package super-save                 ; Autosave buffers when they lose focus
  :ensure t
  :init (super-save-mode)
  :config (validate-setq super-save-auto-save-when-idle t))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :bind ("C-c t t" . auto-revert-tail-mode)
  :config
  (validate-setq
   auto-revert-verbose nil
   ;; Revert Dired buffers, too
   global-auto-revert-non-file-buffers t
   ;; Auto-revert files opened via TRAMP
   auto-revert-remote-files t))

(use-package copyright                  ; Deal with copyright notices
  :defer t
  :config
  (validate-setq
   ;; Use ranges to denote consecutive years
   copyright-year-ranges t
   ;; Limit copyright changes to my own copyright
   copyright-names-regexp (regexp-quote user-full-name)))

(use-package typo                       ; Automatically use typographic quotes
  :ensure t
  :init
  (setq-default typo-language "English")

  (typo-global-mode)

  (dolist (hook '(org-mode-hook
                  markdown-mode-hook
                  rst-mode-hook))
    (add-hook hook 'typo-mode)))

(use-package olivetti                   ; Distraction-free interface
  :ensure t
  :bind ("C-c t r" . olivetti-mode)
  :config (validate-setq olivetti-hide-mode-line t))

(use-package tildify                    ; Insert non-breaking spaces on the fly
  :bind ("C-c x t" . tildify-region)
  :init (add-hook 'text-mode-hook #'tildify-mode)
  :config
  ;; Use the right space for LaTeX
  (add-hook 'latex-mode-hook
            (lambda () (setq-local tildify-space-string "~"))))

(use-package unfill                     ; Smart fill/unfill paragraph
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package string-edit                ; Edit strings in a separate buffer
  :ensure t
  :bind ("C-c x s" . string-edit-at-point))

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Make Tab complete if the line is indented
(validate-setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(validate-setq
 indicate-empty-lines t
 require-final-newline t)

(validate-setq
 kill-ring-max 200                      ; More killed items
 kill-do-not-save-duplicates t          ; No duplicates in kill ring
 ;; Save the contents of the clipboard to kill ring before killing
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t)

;;; Utilities and keybindings
(bind-key "C-c x i" #'indent-region)
(bind-key "C-c t v" #'visual-line-mode)

;; Kill entire line with prefix argument
(defmacro bol-with-prefix (function)
  "Define a new function which will call FUNCTION.

Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument.  The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "mu-%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

(bind-key [remap sp-kill-hybrid-sexp] (bol-with-prefix sp-kill-hybrid-sexp))
(bind-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(bind-key [remap kill-line] (bol-with-prefix kill-line))
(bind-key "C-k" (bol-with-prefix kill-visual-line))

;;;###autoload
(defun just-one-space-in-region (beg end)
  "Replace all whitespace in the region from BEG to END with single spaces."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

;;;###autoload
(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(bind-key "C-x C-d" 'duplicate-line)    ; Duplicate line at point

;; Join line with the next one
(bind-key "C-j" (lambda ()
                  (interactive)
                  (join-line -1)))

;;;###autoload
(defun flush-kill-lines (regex)
  "Flush lines matching REGEX and append to kill ring.  Restrict to region\
if active."
  (interactive "sFlush kill regex: ")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (point) (mark))
        (goto-char 0))
      (while (search-forward-regexp regex nil t)
        (move-beginning-of-line nil)
        (kill-whole-line)))))

(bind-key [remap just-one-space] #'cycle-spacing)

;;;###autoload
(defun untabify-buffer ()
  "Apply `untabify' to the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun indent-buffer ()
  "Apply `indent-region' to the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including `indent-buffer', which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(bind-key "C-c t c" #'cleanup-buffer)

;;;###autoload
(defun mu-smart-kill-whole-line (&optional arg)
  "Kill whole line and move back to indentation.
Kill the whole line with function `kill-whole-line' and then move
`back-to-indentation'.
With prefix ARG, kill that many lines."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

;;;###autoload
(defun mu-smart-backward-kill-line ()
  "Kill line backwards and re-indent."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

;;;###autoload
(defun mu-smart-open-line ()
  "Insert empty line after the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;;;###autoload
(defun mu-back-to-indentation-or-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(define-minor-mode mu-auto-fill-comments-mode
  "Minor mode to auto-fill comments only."
  :lighter nil
  :keymap nil
  (cond
   (mu-auto-fill-comments-mode
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))
   (:else
    (kill-local-variable 'comment-auto-fill-only-comments)
    (auto-fill-mode -1))))

(add-hook 'prog-mode-hook #'mu-auto-fill-comments-mode)

;;;###autoload
(defun mu-insert-current-date (iso)
  "Insert the current date at point.
When ISO is non-nil, insert the date in ISO 8601 format.
Otherwise insert the date as Mar 04, 2014."
  (interactive "P")
  (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

;;;###autoload
(defun mu-open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(bind-keys
 ([remap kill-whole-line]        . mu-smart-kill-whole-line)
 ([remap move-beginning-of-line] . mu-back-to-indentation-or-beginning-of-line)
 ("RET"                          . newline-and-indent)
 ("S-RET"                        . mu-smart-open-line)
 ("C-o"                          . mu-open-line-with-reindent)
 ("C-<backspace>"                . mu-smart-backward-kill-line))

(defhydra hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("_" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))

(bind-keys ("C-x C-0" . hydra-zoom/body)
           ("C-x C-=" . hydra-zoom/body)
           ("C-x C--" . hydra-zoom/body)
           ("C-x C-+" . hydra-zoom/body))

(bind-keys :map prog-mode-map
           ("C-c d c" . comment-region)
           ("C-c d u" . uncomment-region))

;; Enable upcase-/downcase-region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'mu-editing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-editing.el ends here
