;;; mu-swiper.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Swiper, Ivy and Counsel.

;;; Code:

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package ivy
  :ensure swiper
  :bind ("C-c C-r" . ivy-resume)
  :init (ivy-mode 1)
  :config (setq ivy-use-virtual-buffers t)
  :diminish ivy-mode)

(use-package counsel
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
         ("C-S-o"   . counsel-rhythmbox)))

(provide 'mu-swiper)

;;; mu-swiper.el ends here
