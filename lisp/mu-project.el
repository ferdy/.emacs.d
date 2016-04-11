;;; mu-project.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for project management utilities.

;;; Code:

(use-package projectile                 ; Project management
  :ensure t
  :bind (:map projectile-command-map
              ("p" . counsel-projectile-switch-project))
  :init
  (projectile-global-mode)

  (defun counsel-projectile-switch-project ()
    (interactive)
    (ivy-read "Switch to project: "
              projectile-known-projects
              :require-match t
              :action '(1
                        ("o" projectile-switch-project-by-name
                         "Select file in project")
                        ("g" projectile-vc
                         "Open `magit-status' for this project"))))
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'ivy
        projectile-find-dir-includes-top-level t)

  (projectile-register-project-type 'lein-cljs '("project.clj")
                                    "lein cljsbuild once"
                                    "lein cljsbuild test")
  :diminish projectile-mode)

(provide 'mu-project)

;;; mu-project.el ends here
