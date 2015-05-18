;;; custom-shells.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for eshell, shell and ansi-term.

;;; Code:

(use-package eshell
  :bind ("<f1>" . eshell-here)
  :config (progn
            (defun eshell-here ()
              "Open a new shell in the directory of the buffer's file.
The eshell is renamed to match that directory to make multiple eshell
windows easier."
              (interactive)
              (let* ((parent (if (buffer-file-name)
                                 (file-name-directory (buffer-file-name))
                               default-directory))
                     (name   (car (last (split-string parent "/" t)))))
                (other-window 1)
                (eshell "new")
                (rename-buffer (concat "*eshell: " name "*"))))

            (defun eshell/clear ()
              "Clear the eshell buffer."
              (interactive)
              (let ((inhibit-read-only t))
                (erase-buffer)))

            (setq eshell-cmpl-cycle-completions nil
                  eshell-save-history-on-exit t)

            (defadvice eshell-gather-process-output
                (before absolute-cmd (command args) act)
              "Run scrips from current working on remote system."
              (setq command (file-truename command)))

            ;; Disable hl-line-mode in eshell
            (add-hook 'eshell-mode-hook (lambda ()
                                          (setq-local global-hl-line-mode
                                                      nil)))

            ;; Use system su/sudo
            (eval-after-load "em-unix"
              '(progn
                 (unintern 'eshell/su nil)
                 (unintern 'eshell/sudo nil)))))

(use-package ansi-term
  :bind ("<f2>" . custom/term)
  :init (progn
          (defun custom/term ()
            "Wrapper for `ansi-term'."
            (interactive)
            (ansi-term "/bin/zsh"))

          ;; Disable hl-line-mode in ansi-term
          (add-hook 'term-mode-hook (lambda ()
                                      (setq-local global-hl-line-mode
                                                  nil)))

          ;; Close buffer with 'exit'
          (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
            (if (memq (process-status proc) '(signal exit))
                (let ((buffer (process-buffer proc)))
                  ad-do-it
                  (kill-buffer buffer))
              ad-do-it))
          (ad-activate 'term-sentinel)

          ;; Always use UTF-8
          (defun my-term-use-utf8 ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
          (add-hook 'term-exec-hook 'my-term-use-utf8)

          ;; Paste with C-y
          (defun my-term-paste (&optional string)
            (interactive)
            (process-send-string
             (get-buffer-process (current-buffer))
             (if string string (current-kill 0))))

          (defun my-term-hook ()
            (goto-address-mode) ; Clickable URLs
            (define-key term-raw-map "\C-y" 'my-term-paste))
          (add-hook 'term-mode-hook 'my-term-hook)))

(use-package shell
  :bind ("S-<f2>" . shell)
  :config (progn
            (defun clear-shell ()
              (interactive)
              (let ((comint-buffer-maximum-size 0))
                (comint-truncate-buffer)))

            (define-key shell-mode-map (kbd "C-l") 'clear-shell)

            ;; Shell buffer maximized
            (add-hook 'shell-mode-hook
                      (lambda ()
                        (delete-other-windows)))

            ;; Disable hl-line-mode in shell
            (add-hook 'shell-mode-hook (lambda ()
                                         (setq-local global-hl-line-mode
                                                     nil)))))

(provide 'custom-shells)

;;; custom-shells.el ends here
