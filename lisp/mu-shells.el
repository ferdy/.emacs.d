;;; mu-shells.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for eshell, shell and ansi-term.

;;; Code:

(use-package eshell                     ; Emacs command shell
  :bind ("C-c a s e" . eshell)
  :config
  ;; Handy aliases
  (defalias 'ff 'find-file)

  (defun eshell/l (&rest args) "Same as `ls -lah'"
         (apply #'eshell/ls "-lah" args))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (add-hook
   'eshell-mode-hook
   (lambda ()
     (bind-keys :map eshell-mode-map
                ("C-c C-l"                . counsel-esh-history)
                ([remap eshell-pcomplete] . completion-at-point))))

  ;; Use system su/sudo
  (with-eval-after-load "em-unix"
    '(progn
       (unintern 'eshell/su nil)
       (unintern 'eshell/sudo nil))))

(use-package em-cmpl
  :ensure eshell
  :config (validate-setq eshell-cmpl-cycle-completions nil))

(use-package em-hist
  :ensure eshell
  :config (validate-setq eshell-save-history-on-exit t))

(use-package shell                 ; Specialized comint.el for running the shell
  :bind (("C-c a s t" . shell)
         (:map shell-mode-map
               ("C-c C-l" . counsel-shell-history)))
  :config
  ;; Do not echo input back at me
  (defun mu-shell-turn-echo-off ()
    (validate-setq comint-process-echoes t))

  (add-hook 'shell-mode-hook 'mu-shell-turn-echo-off))

(use-package ansi-term                  ; Powerful terminal emulator
  :bind ("C-c a s T" . ansi-term)
  :init
  ;; Always use Zsh
  (defvar mu-term-shell "/usr/bin/zsh")

  (defadvice ansi-term (before force-bash)
    (interactive (list mu-term-shell)))
  (ad-activate 'ansi-term)

  ;; Close buffer with 'exit'
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
    (if (memq (process-status proc) '(signal exit))
        (let ((buffer (process-buffer proc)))
          ad-do-it
          (kill-buffer buffer))
      ad-do-it))
  (ad-activate 'term-sentinel)

  ;; Always use UTF-8
  (defun mu-term-use-utf8 ()
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'mu-term-use-utf8)

  ;; Paste with C-y
  (defun mu-term-paste (&optional string)
    (interactive)
    (process-send-string
     (get-buffer-process (current-buffer))
     (if string string (current-kill 0))))

  (defun mu-term-hook ()
    (goto-address-mode)               ; Clickable URLs
    (bind-key "C-y" #'mu-term-paste term-raw-map))
  (add-hook 'term-mode-hook 'mu-term-hook))

;;; Utilities and keybindings
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)    ; Always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; Always add output at the bottom
 '(comint-scroll-show-maximum-output t)   ; Scroll to show max possible output
 '(comint-input-ignoredups t)             ; No duplicates in command history
 '(comint-completion-addsuffix t)         ; Insert space/slash after completion
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

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-shells.el ends here
