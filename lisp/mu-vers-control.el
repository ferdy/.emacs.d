;;; mu-vers-control.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for version control tools.

;;; Code:

(use-package vc-hooks ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package magit ; The best Git client out there
  :ensure t
  :bind (("C-c v c" . magit-clone)
         ("C-c v v" . magit-status)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull))
  ;; Aggressively commit to WIP refs on any change
  :init
  (progn (magit-wip-after-save-mode)
         (magit-wip-after-apply-mode)
         (magit-wip-before-change-mode))
  :config
  (progn
    ;; Be quiet
    (setq magit-revert-buffers 'silent
          magit-save-repository-buffers 'dontask
          magit-refs-show-commit-count 'all
          magit-push-always-verify nil
          magit-revision-show-gravatars nil)

    ;; Set `magit-status' fullscreen
    (setq magit-post-display-buffer-hook
          #'(lambda ()
              (when (derived-mode-p 'magit-status-mode)
                (delete-other-windows))))

    ;; Set `magit-repository-directories' for `magit-status'
    (defun mu-magit-set-repo-dirs-from-projectile ()
      "Set `magit-repository-directories' with known Projectile projects."
      (let ((project-dirs (bound-and-true-p projectile-known-projects)))
        ;; Remove trailing slashes from project directories
        (setq magit-repository-directories
              (mapcar #'directory-file-name project-dirs))))

    (with-eval-after-load 'projectile
      (mu-magit-set-repo-dirs-from-projectile))

    (add-hook 'projectile-switch-project-hook
              #'mu-magit-set-repo-dirs-from-projectile))
  :diminish (magit-wip-after-save-local-mode
             magit-wip-before-change-mode))

(use-package magit-rockstar ; Extra functions for Magit
  :ensure t
  :init
  (progn
    (magit-define-popup-action 'magit-rebase-popup
                               ?R "Rockstar" 'magit-rockstar)
    (magit-define-popup-action 'magit-commit-popup
                               ?n "Reshelve" 'magit-reshelve)
    (magit-define-popup-action 'magit-branch-popup
                               ?R "Toggle rebasing" 'magit-branch-toggle-rebase)
    (magit-define-popup-action 'magit-fetch-popup
                               ?p "Pull request" 'magit-branch-pull-request)))

(use-package git-commit ; Git commit message mode
  :ensure t
  :defer t
  :config (remove-hook 'git-commit-finish-query-functions
                       #'git-commit-check-style-conventions))

(use-package gitconfig-mode ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-timemachine ; Go back in Git time
  :ensure t
  :bind ("C-c v t" . git-timemachine))

(provide 'mu-vers-control)

;;; mu-vers-control.el ends here
