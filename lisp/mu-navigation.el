;;; mu-navigation.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for general in-buffer navigation.

;;; Code:

;; Scrolling
(validate-setq
 scroll-conservatively 1000
 ;; Move to beg/end of buffer before signalling an error
 scroll-error-top-bottom t
 ;; Ensure M-v always undoes C-v
 scroll-preserve-screen-position 'always
 ;; Start recentre from top
 recenter-positions '(top middle bottom)
 ;; Disable mouse scrolling acceleration
 mouse-wheel-progressive-speed nil)

(use-package bookmark                   ; Bookmarks to files and directories
  :bind
  ;; Bind "C-x 4 r" to something more useful
  ;; than `find-file-read-only-other-window'
  ("C-x 4 r" . bookmark-jump-other-window)
  :config
  (validate-setq bookmark-completion-ignore-case nil)
  (bookmark-maybe-load-default-file))

(use-package avy-jump                   ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j"   . avy-goto-word-1)
         ("C-c n b" . avy-pop-mark)
         ("C-c n j" . avy-goto-char-2)
         ("C-c n t" . avy-goto-char-timer)
         ("C-c n w" . avy-goto-word-1)))

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'outline-minor-mode)))

(use-package beginend                   ; Redefine M-< and M-> for some modes
  :ensure t
  :config (beginend-global-mode))

(use-package dumb-jump                  ; Jump to definitions
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (validate-setq dumb-jump-selector 'ivy))

(use-package elisp-def             ; Macro-aware go-to-definition for Emacs Lisp
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("M-." . elisp-def)
              ("M-," . xref-pop-marker-stack)))

(use-package macrostep                  ; Navigate through macros
  :ensure t
  :after lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c m m e" . macrostep-expand))
  :bind (:map lisp-interaction-mode-map
              ("C-c m m e" . macrostep-expand)))

;; Quickly pop the mark several times with C-u C-SPC C-SPC
(validate-setq set-mark-command-repeat-pop t)

;; Focus new help windows when opened
(setq-default help-window-select t)

;; Keep popping on C-SPC
(validate-setq set-mark-command-repeat-pop t)

;;; Utilities and keybindings
;; Better forward and backward paragraph
;;;###autoload
(defun mu-forward-paragraph (&optional n)
  "Advance N times just past next blank line."
  (interactive "p")
  (let ((m (use-region-p))
        (para-commands
         '(mu-forward-paragraph mu-backward-paragraph)))
    ;; Only push mark if it's not active and we're not repeating.
    (or m
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; The actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min)))))
    ;; If mark wasn't active, I like to indent the line too.
    (unless m
      (indent-according-to-mode)
      ;; This looks redundant, but it's surprisingly necessary.
      (back-to-indentation))))

;;;###autoload
(defun mu-backward-paragraph (&optional n)
  "Go back up N times to previous blank line."
  (interactive "p")
  (mu-forward-paragraph (- n)))

;; Better paragraph movements
(bind-keys*
 ("M-a" . mu-backward-paragraph)
 ("M-e" . mu-forward-paragraph))

;;;###autoload
(defun super-next-line ()
  "Move 5 lines down."
  (interactive)
  (ignore-errors (forward-line 5)))

;;;###autoload
(defun super-previous-line ()
  "Move 5 lines up."
  (interactive)
  (ignore-errors (forward-line -5)))

;;;###autoload
(defun super-backward-char ()
  "Move point 5 characters back."
  (interactive)
  (ignore-errors (backward-char 5)))

;;;###autoload
(defun super-forward-char ()
  "Move point 5 characters forward."
  (interactive)
  (ignore-errors (forward-char 5)))

(bind-keys
 ("C-S-n" . super-next-line)
 ("C-S-p" . super-previous-line)
 ("C-S-b" . super-backward-char)
 ("C-S-f" . super-forward-char))

;;;###autoload
(defun goto-prev-line-with-same-indentation ()
  "Move to previous line with the same indentation as the current."
  (interactive)
  (back-to-indentation)
  (re-search-backward
   (s-concat "^" (s-repeat (current-column) " ") "[^ \t\r\n\v\f]"))
  (back-to-indentation))

;;;###autoload
(defun goto-next-line-with-same-indentation ()
  "Move to next line with the same indentation as the current."
  (interactive)
  (back-to-indentation)
  (re-search-forward
   (s-concat "^" (s-repeat (current-column) " ") "[^ \t\r\n\v\f]")
   nil nil (if (= 0 (current-column)) 2 1))
  (back-to-indentation))

(bind-keys
 ("C-þ" . goto-prev-line-with-same-indentation)
 ("C-ñ" . goto-next-line-with-same-indentation))

(provide 'mu-navigation)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-navigation.el ends here
