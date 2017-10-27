;;; mu-editing.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for text editing tools.

;;; Code:

(prefer-coding-system 'utf-8)

(electric-indent-mode 1)

(use-package electric                   ; Electric modes package
  :config (add-hook 'after-init-hook #'electric-indent-mode))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t w" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

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
  :config (validate-setq undo-tree-visualizer-timestamps t)
  :diminish undo-tree-mode)

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package subword                    ; Handle capitalized subwords
  :defer t
  ;; Do not override `transpose-words', it should not transpose subwords
  :bind (:map subword-mode-map
              ([remap transpose-words] . nil))
  :init (global-subword-mode 1)
  :diminish subword-mode)

(use-package expand-region      ; Increase the selected region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package easy-kill                  ; Better kill text
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package adaptive-wrap              ; Better line wrap
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

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

(use-package multiple-cursors        ; Easily place multiple cursors in a buffer
  :ensure t
  :bind ("C-c x m" . mu-multiple-cursors/body)
  :init
  (defhydra mu-multiple-cursors (:hint nil)
    "
Multiple cursors (quit with _q_)
^Mark^                ^Edit^                 ^Other^
^----^----------------^----^-----------------^-----^----------------------
_e_:   mark more      _l_:   edit lines      _a_: vertical align
_h_:   mark all       _C-a_: edit begs       _s_: set rect action
_n_:   mark next      _C-e_: edit ends
_p_:   mark previous  _i_:   insert numbers
_r_:   mark regexp
_C-s_: mark region
"
    ("q" nil)
    ("a" mc/vertical-align :exit t)
    ("e" mc/mark-more-like-this-extended)
    ("h" mc/mark-all-like-this-dwim)
    ("i" mc/insert-numbers)
    ("l" mc/edit-lines :exit t)
    ("n" mc/mark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("r" vr/mc-mark)
    ("s" set-rectangular-region-anchor)
    ("C-a" mc/edit-beginnings-of-lines :exit t)
    ("C-e" mc/edit-ends-of-lines :exit t)
    ("C-s" mc/mark-all-in-region))
  :config
  (setq mc/mode-line
        ;; Simplify the MC mode line indicator
        '(:propertize (:eval (concat " " (number-to-string
                                          (mc/num-cursors))))
                      face font-lock-warning-face))
  :diminish multiple-cursors-mode)

(use-package saveplace                  ; Save point position in files
  :init (save-place-mode 1))

(use-package super-save                 ; Autosave buffers when they lose focus
  :ensure t
  :init (super-save-mode)
  :config (validate-setq super-save-auto-save-when-idle t)
  :diminish super-save-mode)

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config
  (validate-setq auto-revert-verbose nil
                 ;; Revert Dired buffers, too
                 global-auto-revert-non-file-buffers t)
  :diminish auto-revert-mode)

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
    (add-hook hook 'typo-mode))
  :diminish typo-mode)

(use-package writeroom-mode             ; Distraction-free interface
  :ensure t
  :bind ("C-c t r" . writeroom-mode))

(use-package tildify                    ; Insert non-breaking spaces on the fly
  :bind ("C-c x t" . tildify-region)
  :init (add-hook 'text-mode-hook #'tildify-mode)
  :config
  ;; Use the right space for LaTeX
  (add-hook 'latex-mode-hook
            (lambda () (setq-local tildify-space-string "~")))
  :diminish tildify-mode)

(use-package copy-as-format            ; Copy using format for GitHub/Slack/etc.
  :ensure t
  :bind (("C-c c g" . copy-as-format-github)
         ("C-c c r" . copy-as-format-rst)
         ("C-c c s" . copy-as-format-slack)))

(use-package unfill                     ; Smart fill/unfill paragraph
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package string-edit                ; Edit strings in a separate buffer
  :ensure t
  :bind ("C-c x s" . string-edit-at-point))

(use-package fancy-narrow               ; narrow-to-region with more eye candy
  :ensure t
  :init (fancy-narrow-mode)
  :diminish fancy-narrow-mode)

;; C-n adds new line when at the end of a line
(validate-setq next-line-add-newlines t)

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
  "Flush lines matching REGEX and append to kill ring.  Restrict to \
region if active."
  (interactive "sFlush kill regex: ")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (point) (mark))
        (goto-char 0))
      (while (search-forward-regexp regex nil t)
        (move-beginning-of-line nil)
        (kill-whole-line)))))

;;;###autoload
(defun mu-align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment from START to END respecting the given REGEXP.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left.  If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                             (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
        (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

;;;###autoload
(defun mu-align-repeat-decimal (start end)
  "Align from START to END a table of numbers.
Use decimal points and dollar signs (both optional)."
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro mu-create-align-repeat-x
    (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "mu-align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (mu-align-repeat start end ,regexp ,justify-right after)))))

(mu-create-align-repeat-x "comma" "," nil t)
(mu-create-align-repeat-x "semicolon" ";" nil t)
(mu-create-align-repeat-x "colon" ":" nil t)
(mu-create-align-repeat-x "equal" "=")
(mu-create-align-repeat-x "math-oper" "[+\\-*/]")
(mu-create-align-repeat-x "ampersand" "&")
(mu-create-align-repeat-x "bar" "|")
(mu-create-align-repeat-x "left-paren" "(")
(mu-create-align-repeat-x "right-paren" ")" t)

(bind-key "C-c x a r" #'mu-align-repeat)
(bind-key "C-c x a m" #'mu-align-repeat-math-oper)
(bind-key "C-c x a ." #'mu-align-repeat-decimal)
(bind-key "C-c x a ," #'mu-align-repeat-comma)
(bind-key "C-c x a ;" #'mu-align-repeat-semicolon)
(bind-key "C-c x a :" #'mu-align-repeat-colon)
(bind-key "C-c x a =" #'mu-align-repeat-equal)
(bind-key "C-c x a &" #'mu-align-repeat-ampersand)
(bind-key "C-c x a |" #'mu-align-repeat-bar)
(bind-key "C-c x a (" #'mu-align-repeat-left-paren)
(bind-key "C-c x a )" #'mu-align-repeat-right-paren)

(defun mu-align-whitespace (start end)
  "Align columns from START to END by whitespace."
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(bind-key "C-c x a SPC" #'mu-align-whitespace)

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

(provide 'mu-editing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-editing.el ends here
