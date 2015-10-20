;;; mu-shells.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for eshell, shell and ansi-term.

;;; Code:

(use-package eshell ; Emacs command shell
  :bind ("C-c a s e" . eshell-here)
  :config
  (progn
    ;; Handy aliases
    (defalias 'ff 'find-file)
    (defun eshell/l (&rest args) "Same as `ls -lah'"
           (apply #'eshell/ls "-lah" args))

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
    (with-eval-after-load "em-unix"
      '(progn
         (unintern 'eshell/su nil)
         (unintern 'eshell/sudo nil)))))

(use-package shell ; Specialized comint.el for running the shell
  :bind ("C-c a s t" . shell)
  :config
  (progn
    (defun clear-shell ()
      (interactive)
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))

    (bind-key "C-l" #'clear-shell shell-mode-map)

    ;; Disable hl-line-mode in shell
    (add-hook 'shell-mode-hook (lambda ()
                                 (setq-local global-hl-line-mode
                                             nil)))

    ;; Do not echo input back at me
    (defun mu-shell-turn-echo-off ()
      (setq comint-process-echoes t))

    (add-hook 'shell-mode-hook 'mu-shell-turn-echo-off)))

(use-package ansi-term ; Powerful terminal emulator
  :bind ("C-c a s T" . ansi-term)
  :init
  (progn
    ;; Always use Zsh
    (defvar my-term-shell "/usr/bin/zsh")
    (defadvice ansi-term (before force-bash)
      (interactive (list my-term-shell)))
    (ad-activate 'ansi-term)

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
      (bind-key "C-y" #'my-term-paste term-raw-map))
    (add-hook 'term-mode-hook 'my-term-hook)))

;;; Utilities and keybindings
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)    ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t)   ; scroll to show max possible output
 '(comint-input-ignoredups t)             ; no duplicates in command history
 '(comint-completion-addsuffix t)         ; insert space/slash after completion
 )

(defun comint-clear-buffer ()
  "Easily clear comint buffers."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(bind-key "C-c M-o" #'comint-clear-buffer comint-mode-map) ; Clear comint buffer

;; Truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(provide 'mu-shells)

;;; mu-shells.el ends here
