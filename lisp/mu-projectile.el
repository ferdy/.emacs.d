;;; mu-projectile.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Projectile and related extensions.

;;; Code:

(use-package projectile                 ; Project management
  :ensure t
  :init (projectile-mode)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (validate-setq
   projectile-completion-system 'ivy
   projectile-find-dir-includes-top-level t
   projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

  (projectile-register-project-type 'lein-cljs '("project.clj")
                                    :compile "lein cljsbuild once"
                                    :test "lein cljsbuild test")
  :diminish projectile-mode)

(use-package counsel-projectile         ; Ivy integration for Projectile
  :ensure t
  :bind (:map projectile-command-map
              ("p" . counsel-projectile-switch-project)
              ("r" . counsel-projectile-rg))
  :init (counsel-projectile-mode))

(use-package projectile-ripgrep         ; Search projects with ripgrep
  :ensure t
  :bind (:map projectile-command-map
              ("s r" . projectile-ripgrep)))

(provide 'mu-projectile)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-projectile.el ends here
