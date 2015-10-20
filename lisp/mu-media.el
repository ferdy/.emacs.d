;;; mu-media.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configurations for media utilities.

;;; Code:

(use-package camcorder ; Record movements from within Emacs
  :ensure t
  :bind ("C-c t c" . camcorder-mode)
  :config (setq camcorder-output-directory "~/videos")
  :diminish (camcorder-mode ". ⓒ"))

(use-package bongo ; Play music with Emacs
  :ensure t
  :bind ("C-c a b" . bongo))

(provide 'mu-media)

;;; mu-media.el ends here
