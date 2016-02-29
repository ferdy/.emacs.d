;;; mu-ivy.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Ivy and related packages.

;;; Code:

(use-package swiper                     ; Isearch with an overview
  :ensure t
  :bind (("C-c s s" . swiper)
         ("C-c s S" . swiper-all))
  :bind (:map isearch-mode-map
              ("M-i" . swiper-from-isearch)))

(use-package ivy                        ; Incremental Vertical completYon
  :ensure swiper
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-r" . ivy-switch-buffer))
  :init (ivy-mode 1)
  :config
  (progn
    ;; Show recently killed buffers when calling `ivy-switch-buffer'
    (setq ivy-use-virtual-buffers t
          ivy-count-format ""           ; Suppress counter
          ;; Show full file path
          ivy-virtual-abbreviate 'full))
  :diminish ivy-mode)

(use-package smex                       ; Better M-x interface
  :ensure t)

(use-package counsel                    ; Completion functions with Ivy
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x"     . counsel-M-x)
         ("C-h v"   . counsel-describe-variable)
         ("C-h f"   . counsel-describe-function)
         ("C-h S"   . counsel-info-lookup-symbol)
         ("C-c u"   . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c G"   . counsel-git-grep)
         ("C-c k"   . counsel-ag)
         ("C-x l"   . counsel-locate)
         ("C-x i"   . counsel-imenu)
         ("M-y"     . counsel-yank-pop))
  :config
  (progn
    (setq counsel-find-file-at-point t
          counsel-find-file-ignore-regexp
          (concat
           ;; file names beginning with # or .
           "\\(?:\\`[#.]\\)"
           ;; file names ending with # or ~
           "\\|\\(?:\\`.+?[#~]\\'\\)"))

    (ivy-set-actions
     'counsel-find-file
     `(("x"
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))
        ,(propertize "delete" 'face 'font-lock-warning-face))))))

(provide 'mu-ivy)

;;; mu-ivy.el ends here
