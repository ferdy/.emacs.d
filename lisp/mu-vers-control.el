;;; mu-vers-control.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for version control tools.

;;; Code:

(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (validate-setq vc-follow-symlinks t))

(use-package magit                      ; The best Git client out there
  :ensure t
  :bind (("C-c v c" . magit-clone)
         ("C-c v C" . magit-checkout)
         ("C-c v v" . magit-status)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull))
  :config
  (validate-setq magit-save-repository-buffers 'dontask
                 magit-refs-show-commit-count 'all)

  ;; Use Ivy
  (validate-setq magit-completing-read-function 'ivy-completing-read)

  ;; Show status buffer in fullscreen
  (validate-setq magit-display-buffer-function
                 #'magit-display-buffer-fullframe-status-v1)

  ;; Kill Magit buffers when quitting `magit-status'
  (defun magit-quit-session (&optional kill-buffer)
    "Kill all Magit buffers on quit"
    (interactive)
    (magit-restore-window-configuration kill-buffer)
    (mu-kill-buffers "^\\*magit"))

  (bind-key "q" #'magit-quit-session magit-status-mode-map)

  ;; Set `magit-repository-directories' for `magit-status'
  (defun mu-magit-set-repo-dirs-from-projectile ()
    "Set `magit-repository-directories' with known Projectile projects."
    (validate-setq magit-repository-directories
                   (mapcar
                    (lambda (dir)
                      (substring dir 0 -1))
                    (cl-remove-if-not
                     (lambda (project)
                       (unless (file-remote-p project)
                         (file-directory-p (concat project "/.git/"))))
                     (projectile-relevant-known-projects)))))

  (with-eval-after-load 'projectile
    (mu-magit-set-repo-dirs-from-projectile))

  (add-hook 'projectile-switch-project-hook
            #'mu-magit-set-repo-dirs-from-projectile)

  ;; Refresh `diff-hl' accordingly
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)
  :diminish (magit-wip-after-save-local-mode
             magit-wip-before-change-mode))

(use-package git-commit                 ; Git commit message mode
  :ensure t
  :init (global-git-commit-mode)
  :config
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode         ; Git attributes mode
  :ensure t
  :defer t)

(provide 'mu-vers-control)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-vers-control.el ends here
