;;; mu-buffers.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for buffers.

;;; Code:

;; Don't let the cursor go into minibuffer prompt
(let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
      (dont-touch-prompt-prop '(cursor-intangible t)))
  (setq minibuffer-prompt-properties
        (append default dont-touch-prompt-prop))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Allow to read from minibuffer while in minibuffer.
(validate-setq enable-recursive-minibuffers t)

;; Show the minibuffer depth (when larger than 1)
(minibuffer-depth-indicate-mode 1)

(validate-setq
 ;; Never use dialogs for minibuffer input
 use-dialog-box nil
 ;; Store more history
 history-length 1000)

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config
  (validate-setq
   savehist-save-minibuffer-history t
   savehist-autosave-interval 180))

;; Don't ask for confirmation
(validate-setq kill-buffer-query-functions
               (delq 'process-kill-buffer-query-function
                     kill-buffer-query-functions))

(validate-setq
 frame-resize-pixelwise t               ; Resize by pixels
 frame-title-format '(:eval (if (buffer-file-name)
                                (abbreviate-file-name (buffer-file-name))
                              "%b")))

;; Configure `display-buffer' behaviour for some special buffers
(validate-setq
 display-buffer-alist
 `(
   ;; Messages, errors, processes, Calendar and REPLs in the bottom side window
   (,(rx bos (or "*Apropos"             ; Apropos buffers
                 "*Man"                 ; Man buffers
                 "*Help"                ; Help buffers
                 "*Warnings*"           ; Emacs warnings
                 "*Process List*"       ; Processes
                 "*Proced"              ; Proced processes list
                 "*Compile-Log*"        ; Emacs byte compiler log
                 "*compilation"         ; Compilation buffers
                 "*Flycheck errors*"    ; Flycheck error list
                 "*Calendar"            ; Calendar window
                 "*cider-repl"          ; CIDER REPL
                 "*intero"              ; Intero REPL
                 "*idris-repl"          ; Idris REPL
                 "*ielm"                ; IELM REPL
                 "*SQL"                 ; SQL REPL
                 "*Cargo"               ; Cargo process buffers
                 ;; AUCTeX command output
                 (and (1+ nonl) " output*")))
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . bottom)
    (reusable-frames . visible)
    (window-height . 0.45))
   (,(rx bos "*shell")
    (display-buffer-same-window)
    (reusable-frames . nil))
   (,(rx bos "*pdf")
    (display-buffer-reuse-window display-buffer-in-side-window)
    (side . right)
    (reusable-frames . visible)
    (window-width . 0.5))
   ;; Let `display-buffer' reuse visible frames for all buffers.  This must
   ;; be the last entry in `display-buffer-alist', because it overrides any
   ;; later entry with more specific actions.
   ("." nil (reusable-frames . visible))))

(use-package uniquify                   ; Unique buffer names
  :config
  (validate-setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"
   ;; Ignore special buffers
   uniquify-ignore-buffers-re "^\\*"))

(use-package ibuf-ext                   ; Extensions for Ibuffer
  :config
  ;; Do not show empty groups
  (validate-setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer                    ; Buffer management
  :bind (("C-x C-b" . mu-ibuffer-open)
         ([remap list-buffers] . ibuffer)
         :map ibuffer-mode-map
         ("q" . mu-pop-window-configuration))
  :config
  (validate-setq
   ibuffer-expert t              ; Do not prompt when on kill buffers operations
   ibuffer-filter-group-name-face 'font-lock-doc-face)

  (defun mu-ibuffer-open ()
    "Save window configuration and call `ibuffer'."
    (interactive)
    (mu-save-wins-then-call 'ibuffer))

  ;; Use a single full frame for ibuffer
  (with-eval-after-load 'ibuffer
    (fullframe ibuffer mu-pop-window-configuration))

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (validate-setq ibuffer-formats
                 '((mark modified read-only " "
                         (name 18 18 :left :elide)
                         " "
                         (size-h 9 -1 :right)
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
                         " "
                         (vc-status 16 16 :left)
                         " "
                         filename-and-process))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'filename/process)
                      (ibuffer-do-sort-by-filename/process)))))

;; Use `emacs-lisp-mode' instead of `lisp-interaction-mode' for scratch buffer
(validate-setq initial-major-mode 'emacs-lisp-mode)

;;; Utilities and keybindings
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
(add-hook 'kill-buffer-query-functions #'mu-do-not-kill-important-buffers)

(bind-key "C-x C-k" #'kill-this-buffer)  ; Kill only the current buffer

(provide 'mu-buffers)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-buffers.el ends here
