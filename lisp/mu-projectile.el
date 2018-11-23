;;; mu-projectile.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for Projectile and related extensions.

;;; Code:

(use-package projectile                 ; Project management
  :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :init (projectile-mode)
  :config (setq projectile-completion-system 'ivy
                projectile-find-dir-includes-top-level t
                projectile-require-project-root 'prompt))

(use-package counsel-projectile         ; Ivy integration for Projectile
  :ensure t
  :after projectile
  :bind (:map projectile-command-map
              ("p"   . counsel-projectile-switch-project)
              ("r"   . counsel-projectile-rg)
              ("s s" . counsel-projectile-rg))
  :init (counsel-projectile-mode))

(provide 'mu-projectile)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-projectile.el ends here
