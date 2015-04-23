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
  :disabled t
  :config
  (progn (setq projectile-completion-system 'helm
               projectile-find-dir-includes-top-level t)

         ;; Replace Ack with helm-ag in Projectile commander
         (def-projectile-commander-method ?a
           "Find ag on project."
           (call-interactively 'helm-projectile-ag))

         (projectile-cleanup-known-projects)
         (projectile-global-mode)))

(use-package springboard ; Temporarily change default-directory for one command
  :load-path "various"
  :bind ("C-c s s" . springboard)
  :init (setq springboard-directories
              '("/home/manuel/emacs/emacs/"
                "/home/manuel/githubs/manuel-uberti/emacs/")))

(provide 'custom-project)

;;; custom-project.el ends here
