;;; mu-buffers.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for buffers.

;;; Code:

;; Don't let the cursor go into minibuffer prompt
(let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
      (dont-touch-prompt-prop '(cursor-intangible t)))
  (setq minibuffer-prompt-properties (append default dont-touch-prompt-prop))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(setq use-dialog-box nil               ; Never use dialogs for minibuffer input
      history-length 1000              ; Store more history
      )

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180))

(setq kill-buffer-query-functions       ; Don't ask for confirmation
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(defun mu-display-buffer-fullframe (buffer alist)
  "Display BUFFER in fullscreen.

ALIST is a `display-buffer' ALIST. Return the new window for BUFFER."
  (let ((window (display-buffer-pop-up-window buffer alist)))
    (when window
      (delete-other-windows window))
    window))

;; Configure `display-buffer' behaviour for some special buffers
(setq display-buffer-alist
      `(
        ;; Magit status window in fullscreen
        (,(rx "*magit: ")
         (mu-display-buffer-fullframe)
         (reusable-frames . nil))
        ;; Messages, errors, Calendar and REPLs in the bottom side window
        (,(rx bos (or "*Help"             ; Help buffers
                      "*Warnings*"        ; Emacs warnings
                      "*compilation"      ; Compilation buffers
                      "*Flycheck errors*" ; Flycheck error list
                      "*shell"            ; Shell window
                      "*Calendar"         ; Calendar window
                      "*cider-repl"       ; CIDER REPL
                      "*sly-mrepl"        ; Sly REPL
                      "*ielm"             ; IELM REPL
                      "*SQL"              ; SQL REPL
                      "*Cargo"            ; Cargo process buffers
                      (and (1+ nonl) " output*") ; AUCTeX command output
                      ))
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.4))
        ;; Let `display-buffer' reuse visible frames for all buffers.  This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; later entry with more specific actions.
        ("." nil (reusable-frames . visible))))

(use-package uniquify                   ; Unique buffer names
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"
        ;; Ignore special buffers
        uniquify-ignore-buffers-re "^\\*"))

(use-package ibuffer                    ; Buffer management
  :bind (([remap list-buffers] . ibuffer)
         ("C-c b i"            . ibuffer))
  :config
  (setq ibuffer-show-empty-filter-groups nil ; Hide empty groups
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

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package scratch                    ; Mode-specific scratch buffers
  :ensure t
  :bind ("C-c b s" . scratch))

;; Use `emacs-lisp-mode' instead of `lisp-interaction-mode' for scratch buffer
(setq initial-major-mode 'emacs-lisp-mode)

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

(provide 'mu-buffers)

;;; mu-buffers.el ends here
