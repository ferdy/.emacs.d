;;; mu-swiper.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Swiper, Ivy and Counsel.

;;; Code:

(use-package swiper                     ; Isearch with an overview
  :ensure t
  :bind ("C-s" . swiper))

(use-package ivy                        ; Incremental Vertical completYon
  :ensure swiper
  :bind ("C-c C-r" . ivy-resume)
  :init (ivy-mode 1)
  :config
  (progn
    ;; Show recently killed buffers when calling `ivy-switch-buffer'
    (setq ivy-use-virtual-buffers t
          ivy-virtual-abbreviate 'full ; Show full file path
          ivy-re-builders-alist '((t . ivy--regex-plus))))
  :diminish ivy-mode)

(use-package counsel                    ; Completion functions with Ivy
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("M-x"     . counsel-M-x)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-load-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
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

(provide 'mu-swiper)

;;; mu-swiper.el ends here
