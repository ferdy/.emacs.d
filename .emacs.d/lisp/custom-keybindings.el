;;; custom-keybindings.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores all the keybindings I use.

;;; Code:

;; Better forward and backward paragraph
(global-set-key "\M-a" 'custom/backward-paragraph)
(global-set-key "\M-e" 'custom/forward-paragraph)

(defun custom/forward-paragraph (&optional n)
  "Advance N times just past next blank line."
  (interactive "p")
  (let ((m (use-region-p))
        (para-commands
         '(custom/forward-paragraph custom/backward-paragraph)))
    ;; only push mark if it's not active and we're not repeating.
    (or m
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; the actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min)))))
    ;; if mark wasn't active, I like to indent the line too.
    (unless m
      (indent-according-to-mode)
      ;; this looks redundant, but it's surprisingly necessary.
      (back-to-indentation))))

(defun custom/backward-paragraph (&optional n)
  "Go back up N times to previous blank line."
  (interactive "p")
  (custom/forward-paragraph (- n)))

;; Better window movings
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backward)

(global-set-key (kbd "C-x C-d") 'duplicate-line) ; Duplicate line at point

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;; Custom keybindings activated with C^x t
(define-prefix-command 'toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'toggle-map)
(define-key toggle-map "v" 'visual-line-mode)
(define-key toggle-map "l" 'linum-mode)
(define-key toggle-map "h" 'hidden-mode-line-mode)
(define-key toggle-map "u" 'unscroll)
(define-key toggle-map "s" 'create-scratch-buffer)
(define-key toggle-map "r" 'revert-this-buffer)
(define-key toggle-map "w" 'writeroom-mode)

(global-set-key "\M-g" 'goto-line) ; Goto line is M-g

(global-set-key "\C-x\C-k" 'kill-this-buffer) ; Kill only the current buffer

(setq next-line-add-newlines t) ; C^n adds new line when at the end of a line

(global-set-key (kbd "C-;") #'comment-line) ; Comment-line

;; Better mark commands
(defun push-mark-no-activate ()
  "Pushes 'point' to 'mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-+") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-+") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark]
  'exchange-point-and-mark-no-activate)

(global-set-key (kbd "C-z") 'repeat) ; C-z for repeat (usually C-x z)

(global-set-key "\C-x\C-o"
                'multi-occur-in-this-mode) ; Global key for 'multi-occur-in-this-mode'

(global-set-key "\C-x\M-o" 'occur-dwim) ; Global key for 'occur-dwim'

;; Kill entire line with prefix argument
(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument.  The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "custom/%s-BOL" function))))
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

(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key "\C-k" (bol-with-prefix kill-visual-line))

(define-key comint-mode-map "\C-c\M-o"
  #'comint-clear-buffer) ; Clear comint buffers

;; Better shrink/enlarge windows
(global-set-key (kbd "C-S-<up>") 'enlarge-window)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)

;; Minor mode for 'override' keybindings
(use-package my-keys-mode
  :load-path "various"
  :config
  (progn
    (define-key my-keys-mode-map (kbd "M-a") 'custom/backward-paragraph)
    (define-key my-keys-mode-map (kbd "M-e") 'custom/forward-paragraph)
    (define-key my-keys-mode-map (kbd "C-,") 'iedit-dwim)
    (define-key my-keys-mode-map (kbd "C-c o") (lambda ()
                                                 (interactive)
                                                 (find-file "~/org/organizer.org")))
    (global-my-keys-mode)))

(provide 'custom-keybindings)

;;; custom-keybindings ends here
