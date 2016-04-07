;;; mu-project.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for project management utilities.

;;; Code:

(use-package projectile                 ; Project management
  :ensure t
  :bind ("C-c p" . mu-projectile/body)
  :init (projectile-global-mode)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'ivy
        projectile-find-dir-includes-top-level t)

  (projectile-register-project-type 'lein-cljs '("project.clj")
                                    "lein cljsbuild once"
                                    "lein cljsbuild test")
  (defhydra mu-projectile-other-window (:hint nil)
    "projectile-other-window"
    ("f"  projectile-find-file-other-window        "file")
    ("g"  projectile-find-file-dwim-other-window   "file dwim")
    ("d"  projectile-find-dir-other-window         "dir")
    ("b"  projectile-switch-to-buffer-other-window "buffer")
    ("q"  nil                                      "cancel" :color blue))

  (defhydra mu-projectile (:hint nil)
    "
     Projectile: %(projectile-project-root) (quit with _q_)

^Find File^         ^Search/Tags^
^---------^---------^-----------^---------
_F_ file            _a_ ag
_f_ file dwim       _o_ multi-occur
_d_ file curr dir
_r_ recent file
_D_ dir
^Buffers^             ^Cache^
^-------^-------------^-----^-------------
_i_ Ibuffer           _c_ cache clear
_b_ switch to buffer  _x_ remove known project
_K_ Kill all buffers  _X_ cleanup non-existing
                    ^^_z_ cache current
"
    ("a"   projectile-ag)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("F"   projectile-find-file)
    ("f"   projectile-find-file-dwim)
    ("D"   projectile-find-file-in-directory)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("P"   projectile-switch-project "switch project")
    ("p"   projectile-switch-project)
    ("s"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("`"   mu-projectile-other-window/body "other window")
    ("q"   nil))
  :diminish projectile-mode)

(provide 'mu-project)

;;; mu-project.el ends here
