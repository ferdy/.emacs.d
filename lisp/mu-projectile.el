;;; mu-projectile.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for Projectile and related extensions.

;;; Code:

(use-package projectile                 ; Project management
  :ensure t
  :init (projectile-mode)
  :config
  ;; Prefer old prefix
  (validate-setq projectile-keymap-prefix (kbd "C-c p"))
  (unbind-key "C-c C-p" projectile-mode-map)
  
  (validate-setq
   projectile-completion-system 'ivy
   projectile-find-dir-includes-top-level t)

  (projectile-register-project-type 'lein-cljs '("project.clj")
                                    :compile "lein cljsbuild once"
                                    :test "lein cljsbuild test"))

(use-package counsel-projectile         ; Ivy integration for Projectile
  :ensure t
  :bind (:map projectile-command-map
              ("p" . counsel-projectile-switch-project)
              ("r" . counsel-projectile-rg))
  :init (counsel-projectile-mode))

(provide 'mu-projectile)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-projectile.el ends here
