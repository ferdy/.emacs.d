;;; mu-editing.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for text editing tools.

;;; Code:

(prefer-coding-system 'utf-8)

(use-package electric                   ; Electric modes package
  :config (add-hook 'after-init-hook #'electric-indent-mode))

(use-package aggressive-indent          ; Automatically indent code
  :ensure t
  :bind ("C-c t i" . aggressive-indent-mode)
  :hook ((lisp-mode       . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode    . aggressive-indent-mode))
  :config
  ;; Free C-c C-q, used in Org and in CIDER
  (unbind-key "C-c C-q" aggressive-indent-mode-map)

  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

(use-package undo-tree                  ; Show buffer changes as a tree
  :ensure t
  :init (global-undo-tree-mode)
  :config (setq undo-tree-visualizer-timestamps t))

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
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config (setq-default adaptive-wrap-extra-indent 2))

(use-package aggressive-fill-paragraph  ; Automatically fill paragrah
  :ensure t
  :hook ((org-mode . aggressive-fill-paragraph-mode)
         (TeX-mode . aggressive-fill-paragraph-mode)))

(use-package unfill                     ; Smart fill/unfill paragraph
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package visual-fill-column         ; Fill column wrapping
  :ensure t
  :bind ("C-c t v" . visual-fill-column-mode)
  :hook ((visual-line-mode . visual-fill-column-mode)
         (prog-mode        . visual-fill-column-mode)
         (text-mode        . visual-fill-column-mode))
  :config
  ;; Split windows vertically despite large margins, because Emacs otherwise
  ;; refuses to vertically split windows with large margins
  (setq split-window-preferred-function
        #'visual-fill-column-split-window-sensibly)

  (defun mu-maybe-adjust-visual-fill-column ()
    "Readjust visual fill column when the global font size is modified."
    (if visual-fill-column-mode
        (add-hook
         'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
      (remove-hook
       'after-setting-font-hook 'visual-fill-column--adjust-window t)))

  (add-hook 'visual-fill-column-mode-hook #'mu-maybe-adjust-visual-fill-column))

(use-package align                      ; Align text in buffers
  :bind (("C-c x a a" . align)
         ("C-c x a c" . align-current)))

(use-package ialign                     ; Visual align-regexp
  :ensure t
  :bind ("C-c x a i" . ialign))

(use-package saveplace                  ; Save point position in files
  :init (save-place-mode 1))

(use-package super-save                 ; Auto-save buffers
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :bind ("C-c t t" . auto-revert-tail-mode)
  :config
  (setq auto-revert-verbose nil
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t
        ;; Auto-revert files opened via TRAMP
        auto-revert-remote-files t))

(use-package copyright                  ; Deal with copyright notices
  :defer t
  :config
  (setq copyright-year-ranges t   ; Use ranges to denote consecutive years
        ;; Limit copyright changes to my own copyright
        copyright-names-regexp (regexp-quote user-full-name)))

(use-package typo                       ; Automatically use typographic quotes
  :ensure t
  :hook ((org-mode      . typo-mode)
         (markdown-mode . typo-mode)
         (rst-mode      . typo-mode))
  :init (setq-default typo-language "English"))

(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind ("C-c t r" . writeroom-mode)
  :config
  (setq writeroom-fullscreen-effect 'maximized
        writeroom-bottom-divider-width 0))

(use-package tildify                    ; Insert non-breaking spaces on the fly
  :hook ((text-mode  . tildify-mode)
         (latex-mode . (lambda ()
                         ;; Use the right space for LaTeX
                         (setq-local tildify-space-string "~")))))

(use-package string-edit                ; Edit strings in a separate buffer
  :ensure t
  :bind (("C-c x s" . string-edit-at-point)
         :map string-edit-mode-map
         ("C-c C-q" . string-edit-abort)))

(use-package edit-indirect              ; Edit regions in separate buffers
  :ensure t
  :defer t)

(use-package mmm-mode                   ; Allow multiple major modes in a buffer
  :ensure t
  :config
  (require 'mmm-auto)
  (setq mmm-global-mode 'buffers-with-submode-classes
        mmm-submode-decoration-level 2))

(use-package slow-keys                  ; Slow keys mode to avoid RSI
  :ensure t
  :bind ("C-c t s" . slow-keys-mode)
  :config (setq slow-keys-min-delay 0.03))

(use-package move-text                  ; Move line or region with M-up/M-down
  :ensure t
  :bind* (([M-down] . move-text-down)
          ([M-up]   . move-text-up)))

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

(setq kill-ring-max 200                 ; More killed items
      kill-do-not-save-duplicates t     ; No duplicates in kill ring
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;;; Utilities and key bindings
(bind-key "C-c x i" #'indent-region)
(bind-key "C-c t v" #'visual-line-mode)

;; Kill entire line with prefix argument
(defmacro bol-with-prefix (function)
  "Define a new function which will call FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument. The FUNCTION still receives the
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

(defun mu-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

;;;###autoload
(defun mu-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers will
be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (mu-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(bind-key "C-c x d" #'mu-duplicate-current-line-or-region)

;;;###autoload
(defun mu-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers will
be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (mu-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (dotimes (_ arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(bind-key "C-c x D" #'mu-duplicate-and-comment-current-line-or-region)

;; Join line with the next one
(bind-key "C-j" (lambda ()
                  (interactive)
                  (join-line -1)))

;;;###autoload
(defun flush-kill-lines (regex)
  "Flush lines matching REGEX and append to kill ring.
Restrict to region if active."
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
  (whitespace-cleanup)
  (indent-buffer))

(bind-key "C-c b" #'cleanup-buffer)

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
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;;;###autoload
(defun mu-back-to-indentation-or-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line. If
point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line. If ARG is not nil or 1, move forward
ARG - 1 lines first. If point reaches the beginning or end of the
buffer, stop there."
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
 ("S-<return>"                   . mu-smart-open-line)
 ("C-o"                          . mu-open-line-with-reindent)
 ("C-<backspace>"                . mu-smart-backward-kill-line))

(bind-keys :map prog-mode-map
           ("C-c d c" . comment-region)
           ("C-c d u" . uncomment-region))

;;;###autoload
(defun mu-upcase-region (beg end)
  "`upcase-region' from BEG to END when region is active."
  (interactive "*r")
  (when (use-region-p)
    (upcase-region beg end)))

;;;###autoload
(defun mu-downcase-region (beg end)
  "`downcase-region' from BEG to END when region is active."
  (interactive "*r")
  (when (use-region-p)
    (downcase-region beg end)))

;;;###autoload
(defun mu-capitalize-region (beg end)
  "`capitalize-region'from BEG to END when and region is active."
  (interactive "*r")
  (when (use-region-p)
    (capitalize-region beg end)))

(bind-key "C-x C-u" #'mu-upcase-region)
(bind-key "C-x C-l" #'mu-downcase-region)
(bind-key "C-x M-c" #'mu-capitalize-region)

(provide 'mu-editing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-editing.el ends here
