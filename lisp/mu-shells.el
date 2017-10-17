;;; mu-shells.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for shell, EShell and ansi-term.

;;; Code:

(use-package shell                 ; Specialized comint.el for running the shell
  :bind (("C-c a s t" . shell)
         ("<f1>"      . shell)
         (:map shell-mode-map
               ("<tab>" . completion-at-point)))
  :config
  (unbind-key "C-c C-l" shell-mode-map)
  (bind-key "C-c C-l" #'counsel-shell-history shell-mode-map)

  ;; Prefer Bash to Fish for compatibility reasons
  (validate-setq explicit-shell-file-name "/bin/bash")

  ;; Do not echo input back at me
  (defun mu-shell-turn-echo-off ()
    (validate-setq comint-process-echoes t))

  (add-hook 'shell-mode-hook #'mu-shell-turn-echo-off)

  ;; C-d to kill buffer if process is dead
  (defun mu-comint-delchar-or-eof-or-kill-buffer (arg)
    "Kill the buffer is process is dead, otherwise delete ARG."
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

  (add-hook 'shell-mode-hook
            (lambda ()
              (bind-key "C-d" #'mu-comint-delchar-or-eof-or-kill-buffer
                        shell-mode-map))))

(use-package shx                        ; Enhance comint-mode
  :ensure t
  :init (shx-global-mode 1))

(use-package bash-completion            ; Bash completion for shell-mode
  :ensure t
  :config (bash-completion-setup))

(use-package eshell                     ; Emacs command shell
  :bind ("C-c a s e" . eshell-here)
  :config
  (defun eshell-here ()
    "Open EShell in the directory associated with the current buffer's file.
The EShell is renamed to match that directory to make multiple windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (name   (car (last (split-string parent "/" t)))))
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))))

  ;; Handy aliases
  (defalias 'ff 'find-file)

  (defun eshell/l (&rest args) "Same as `ls -lah'"
         (apply #'eshell/ls "-lah" args))

  (defun eshell/d ()
    "Open a dired instance of the current working directory."
    (dired "."))

  (defun eshell/gs (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))                      ; The echo command suppresses output

  (defun eshell/clear ()
    "Clear `eshell' buffer, comint-style."
    (interactive)
    (let ((input (eshell-get-old-input)))
      (eshell/clear-scrollback)
      (eshell-emit-prompt)
      (insert input)))

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
       (unintern 'eshell/sudo nil)))

  (defun mu-eshell-quit-or-delete-char (arg)
    "Use C-d to either delete forward char or exit EShell."
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp nil nil))
        (progn
          (eshell-life-is-too-much)
          (ignore-errors
            (delete-window)))
      (delete-char arg)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-key "C-d"
                        #'mu-eshell-quit-or-delete-char eshell-mode-map))))

(use-package em-banner
  :ensure eshell
  :config
  (validate-setq
   eshell-banner-message (concat "Welcome to EShell, "
                                 (capitalize user-login-name)
                                 "!\n\n")))

(use-package esh-mode
  :ensure eshell
  :config (validate-setq eshell-scroll-to-bottom-on-input 'all))

(use-package em-cmpl
  :ensure eshell
  :config (validate-setq eshell-cmpl-cycle-completions nil))

(use-package em-hist
  :ensure eshell
  :config (validate-setq eshell-hist-ignoredups t))

(use-package em-term
  :ensure eshell
  :config
  (add-to-list 'eshell-visual-commands "ssh")
  (add-to-list 'eshell-visual-commands "htop")
  (add-to-list 'eshell-visual-commands "top")
  (add-to-list 'eshell-visual-commands "tail")
  (add-to-list 'eshell-visual-commands "npm"))

(use-package em-cmpl
  :ensure eshell
  :config
  (add-to-list 'eshell-command-completions-alist
               '("gunzip" "gz\\'"))
  (add-to-list 'eshell-command-completions-alist
               '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'")))

(use-package shell-switcher             ; Fast switching between shell buffers
  :ensure t
  :bind (("C-'"   . shell-switcher-switch-buffer)
         ("C-M-'" . shell-switcher-new-shell)))

(use-package eshell-bookmark            ; Bookmarks for EShell buffers
  :ensure t
  :config (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))

(use-package ansi-term                  ; Powerful terminal emulator
  :bind ("C-c a s T" . ansi-term)
  :init
  ;; Always use Bash
  (defvar mu-term-shell "/bin/bash")

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

(use-package fish-mode                  ; Handle Fish shell scripts
  :ensure t
  :mode ("\\.fish\\'" . fish-mode)
  :config
  ;; Run fish_indent before save
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'fish_indent-before-save))))

;;; Utilities and key bindings
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
