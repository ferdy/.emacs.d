;;; custom-project.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

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
    (setq projectile-completion-system 'helm
          projectile-find-dir-includes-top-level t)

    ;; Replace Ack with Ag in Projectile commander
    (def-projectile-commander-method ?a
      "Find ag on project."
      (call-interactively 'projectile-ag))

    (projectile-cleanup-known-projects)
    (projectile-global-mode)))

(provide 'custom-project)

;;; custom-project.el ends here
