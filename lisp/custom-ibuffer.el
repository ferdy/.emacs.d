;;; custom-ibuffer.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my Ibuffer configuration.

;;; Code:

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config (progn
            (setq ibuffer-formats
                  '((mark modified read-only vc-status-mini " "
                          (name 18 18 :left :elide)
                          " "
                          (size 9 -1 :right)
                          " "
                          (mode 16 16 :left :elide)
                          " "
                          (vc-status 16 16 :left)
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
                          " " filename))
                  ibuffer-show-empty-filter-groups nil
                  ibuffer-saved-filter-groups
                  '(("default"
                     ("helm" (predicate string-match "Helm" mode-name))
                     ("starbuffers" (name . "^\\*.*?\\*$")))))))

(use-package ibuffer-vc ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile ; Group buffers by Projectile project
  :ensure t
  :defer t)

(provide 'custom-ibuffer)

;;; custom-ibuffer.el ends here
