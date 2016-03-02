;;; mu-buffers.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for buffers.

;;; Code:

;; Don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties '(read-only
                                     t
                                     point-entered
                                     minibuffer-avoid-prompt
                                     face
                                     minibuffer-prompt))

;; Automatically close somebuffers on exit
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((completions "*Completions*"))
               (and (get-buffer completions)
                    (kill-buffer completions)))))

(setq use-dialog-box nil)               ; Never use dialogs for minibuffer input

(setq kill-buffer-query-functions       ; Don't ask for confirmation
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

;; Configure `display-buffer' behaviour for some special buffers
(setq display-buffer-alist
      `( ;; Put REPLs and error lists into the bottom side window
        (,(rx bos (or "*Flycheck errors*" ; Flycheck error list
                      "*compilation"      ; Compilation buffers
                      "*Warnings*"        ; Emacs warnings
                      "*Help*"            ; Help window
                      "*shell"            ; Shell window
                      "*cider-repl"       ; CIDER buffers
                      "*sly-mrepl"        ; Sly REPL
                      "*SQL"              ; SQL REPL
                      ))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side            . bottom)
         (reusable-frames . visible)
         (window-height   . 0.33))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(use-package uniquify                   ; Unique buffer names
  :config (setq uniquify-buffer-name-style
                'post-forward uniquify-separator ":"))

(use-package ibuffer                    ; Buffer management
  :bind (([remap list-buffers] . ibuffer)
         ("C-c b i"            . ibuffer))
  :config (setq ibuffer-show-empty-filter-groups nil ; Hide empty groups
                ibuffer-formats
                '((mark modified read-only " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " "
                        filename-and-process)
                  (mark modified read-only " "
                        (name 18 18 :left :elide)
                        " "
                        (size 9 -1 :right)
                        " "
                        (mode 16 16 :left :elide)
                        " " filename-and-process)
                  (mark " "
                        (name 16 -1)
                        " " filename))))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(use-package scratch                    ; Mode-specific scratch buffers
  :ensure t
  :bind ("C-c b s" . scratch))

;;; Utilities and keybindings
;;;###autoload
(defun mu-kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (cl-letf (((symbol-function 'kill-buffer-ask)
             (lambda (buffer) (kill-buffer buffer))))
    (kill-matching-buffers regexp)))

;; Don't kill the important buffers
(defconst mu-do-not-kill-buffer-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

;;;###autoload
(defun mu-do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.
Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) mu-do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

;; Don't kill important buffers
(add-hook 'kill-buffer-query-functions
          #'mu-do-not-kill-important-buffers)

(bind-key "C-x C-k" 'kill-this-buffer)  ; Kill only the current buffer

;;;###autoload
(defun scratch-clear ()
  "Clear *scratch* buffer."
  (interactive)
  (with-current-buffer "*scratch*"
    (if (string= "*scratch*" (buffer-name))
        (erase-buffer))))

(bind-key "C-c C-l" #'scratch-clear lisp-interaction-mode-map)

(provide 'mu-buffers)

;;; mu-buffers.el ends here
