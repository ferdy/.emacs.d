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
  :defer t
  :init (progn
          (defun custom/term ()
            "Wrapper for `ansi-term'."
            (interactive)
            (ansi-term "/bin/zsh"))

          ;; Disable hl-line-mode in ansi-term
          (add-hook 'term-mode-hook (lambda ()
                                      (setq-local global-hl-line-mode
                                                  nil)))))

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

(use-package multi-term ; Manage multiple terminal buffers
  :ensure t
  :bind (("<f2>" . multi-term))
  :init (progn
          (setq multi-term-program "/bin/zsh")
          ;; Wrap long lines
          (add-hook 'term-mode-hook #'toggle-word-wrap)))

(provide 'custom-shells)

;;; custom-shells.el ends here
