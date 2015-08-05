;;; custom-ibuffer.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores configuration for buffers.

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

(setq use-dialog-box nil) ; Never use dialogs for minibuffer input

(setq frame-resize-pixelwise t ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

;; Configure `display-buffer' behaviour for some special buffers.
(setq display-buffer-alist
      `(;; Put REPLs and error lists into the bottom side window
        (,(rx bos (or "*Flycheck errors*" ; Flycheck error list
                      "*compilation"      ; Compilation buffers
                      "*Warnings*"        ; Emacs warnings
                      "*cider"            ; CIDER buffers
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

(use-package uniquify ; Unique buffer names
  :config (setq uniquify-buffer-name-style
                'post-forward uniquify-separator ":"))

(use-package ibuffer ; Buffer management
  :bind ([remap list-buffers] . ibuffer)
  :config (progn
            (setq ibuffer-formats
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
                          " " filename)))

            (setq ibuffer-show-empty-filter-groups nil ; Hide empty groups
                  ibuffer-never-show-predicates ; Hide Helm buffers
                  '("*helm"))))

(use-package ibuffer-projectile ; Group buffers by Projectile project
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(provide 'custom-buffers)

;;; custom-ibuffer.el ends here