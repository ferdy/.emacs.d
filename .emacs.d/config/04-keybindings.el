;;;; keybindings.el

;;; This file stores all the keybindings I use.

;; Better backspacing
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Better forward and backward paragraph
;; See http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
(global-set-key "\M-a" 'custom/backward-paragraph)
(global-set-key "\M-e" 'custom/forward-paragraph)

(defun custom/forward-paragraph (&optional n)
  "Advance just past next blank line."
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
  "Go back up to previous blank line."
  (interactive "p")
  (custom/forward-paragraph (- n)))

;; Better window movings
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backward)

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;; Custom keybindings activated with C^x t
;; See http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(define-prefix-command 'toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'toggle-map)
(define-key toggle-map "v" 'visual-line-mode)
(define-key toggle-map "c" 'column-number-mode)
(define-key toggle-map "l" 'linum-mode)
(define-key toggle-map "h" 'hidden-mode-line-mode)
(define-key toggle-map "u" 'unscroll)

;; Goto line is M-g
(global-set-key "\M-g" 'goto-line)

;; See http://endlessparentheses.com/Meta-Binds-Part-1%253A-Drunk-in-the-Dark.html
(global-set-key "\M-9" 'backward-sexp)
(global-set-key "\M-0" 'forward-sexp)

;; Kill only the current buffer
(global-set-key "\C-x\C-k" 'kill-this-buffer)

;; C^n adds new line when at the end of a line
(setq next-line-add-newlines t)

;; Better mark commands
;; See http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun push-mark-no-activate ()
  "Pushes 'point' to 'mark-ring' and does not activate the region
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
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Ido bury buffer
;; See http://endlessparentheses.com/Ido-Bury-Buffer.html
(add-hook
 'ido-setup-hook
 (defun custom/define-ido-bury-key ()
   (define-key ido-completion-map
     (kbd "C-b") 'custom/ido-bury-buffer-at-head)))

(defun custom/ido-bury-buffer-at-head ()
  "Bury the buffer at the head of 'ido-matches'."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (buf (ido-name (car ido-matches)))
        (nextbuf (cadr ido-matches)))
    (when (get-buffer buf)
      ;; If next match names a buffer use the buffer object;
      ;; buffer name may be changed by packages such as
      ;; uniquify.
      (when (and nextbuf (get-buffer nextbuf))
        (setq nextbuf (get-buffer nextbuf)))
      (bury-buffer buf)
      (if (bufferp nextbuf)
          (setq nextbuf (buffer-name nextbuf)))
      (setq ido-default-item nextbuf
            ido-text-init ido-text
            ido-exit 'refresh)
      (exit-minibuffer))))

;; Use a ido-charged recentf
(require 'recentf)

;; get rid of 'find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use 'ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; C-z for repeat (usually C-x z)
(global-set-key (kbd "C-z") 'repeat)

;; C-x C-b for ibuffer
(global-set-key "\C-x\C-b" 'ibuffer)

;; Searching buffers with occur mode
(eval-when-compile
  (require 'cl))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; Global key for 'multi-occur-in-this-mode'
(global-set-key "\C-x\C-o" 'multi-occur-in-this-mode)

;; Global key for eshell
(global-set-key (kbd "<f1>") 'eshell)

;; Global key for shell
(global-set-key (kbd "<f2>") 'shell)

;; Global key for magit-status
(global-set-key (kbd "<f3>") 'magit-status)

;; Global key fo mf/mirror-region-in-multifile
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)
