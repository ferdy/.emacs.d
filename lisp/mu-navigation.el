;;; mu-navigation.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

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

(use-package ace-link                   ; Quickly follow links with Avy
  :ensure t
  :init (ace-link-setup-default))

(use-package outline                    ; Navigate outlines in buffers
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'outline-minor-mode))
  :diminish outline-minor-mode)

(use-package beginend                   ; Redefine M-< and M-> for some modes
  :ensure t
  :init (beginend-setup-all))

;; Quickly pop the mark several times with C-u C-SPC C-SPC
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

;; Better mark commands
;;;###autoload
(defun push-mark-no-activate ()
  "Pushes 'point' to 'mark-ring' and does not activate the region.

Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;;;###autoload
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the 'mark-ring' order.

This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;;;###autoload
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(bind-key "C-+" 'push-mark-no-activate)
(bind-key "M-+" 'jump-to-mark)
(bind-key [remap exchange-point-and-mark]
          'exchange-point-and-mark-no-activate global-map)

;; When popping the mark, continue popping until the cursor
;; actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(provide 'mu-navigation)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-navigation.el ends here
