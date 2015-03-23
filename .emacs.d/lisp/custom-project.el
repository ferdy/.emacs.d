;;; custom-project.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for project management utilities.

;;; Code:

;;; Project Management
(use-package projectile
  :ensure t
  :config
  (progn
    (setq projectile-completion-system 'ido
          projectile-find-dir-includes-top-level t)

    ;; Replace Ack with Ag in Projectile commander
    (def-projectile-commander-method ?a
      "Find ag on project."
      (call-interactively 'projectile-ag))

    (projectile-cleanup-known-projects)
    (projectile-global-mode)))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :defer t
  :config
  (progn
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
          ibuffer-show-empty-filter-groups nil)))

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

(provide 'custom-project)

;;; custom-project.el ends here
