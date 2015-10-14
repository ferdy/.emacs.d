;;; mu-project.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for project management utilities.

;;; Code:

(use-package projectile ; Project management
  :ensure t
  :init (projectile-global-mode)
  :config
  (progn
    ;; Remove dead projects when Emacs is idle
    (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

    (setq projectile-completion-system 'helm
          projectile-find-dir-includes-top-level t)

    (projectile-register-project-type 'lein-cljs '("project.clj")
                                      "lein cljsbuild once"
                                      "lein cljsbuild test"))
  :diminish projectile-mode)

(provide 'mu-project)

;;; mu-project.el ends here
