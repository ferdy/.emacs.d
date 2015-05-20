;;; custom-ibuffer.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my Ibuffer configuration.

;;; Code:

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
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
                          " " filename)))

            ;; Hide empty groups
            (setq ibuffer-show-empty-filter-groups nil)))

(use-package ibuffer-vc ; Group buffers by VC project and status
  :ensure t
  :defer t
  :disabled t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile ; Group buffers by Projectile project
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-projectile-set-filter-groups)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(provide 'custom-ibuffer)

;;; custom-ibuffer.el ends here
