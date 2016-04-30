;;; mu-vers-control.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for version control tools.

;;; Code:

(use-package vc-hooks                   ; Simple version control
  :defer t
  :config
  ;; Always follow symlinks to files in VCS repos
  (setq vc-follow-symlinks t))

(use-package magit                      ; The best Git client out there
  :ensure t
  :bind (("C-c v c" . magit-clone)
         ("C-c v C" . magit-checkout)
         ("C-c v v" . magit-status)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull))
  :init
  ;; Aggressively commit to WIP refs on any change
  (magit-wip-after-save-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode)
  :config
  ;; Be quiet
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        magit-revision-show-gravatars nil)

  ;; Use Ivy
  (setq magit-completing-read-function 'ivy-completing-read)

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
    (setq magit-repository-directories
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
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  :diminish (magit-wip-after-save-local-mode
             magit-wip-before-change-mode))

(use-package magit-rockstar             ; Extra functions for Magit
  :ensure t
  :init
  (magit-define-popup-action 'magit-rebase-popup
    ?R "Rockstar" 'magit-rockstar)
  (magit-define-popup-action 'magit-commit-popup
    ?n "Reshelve" 'magit-reshelve)
  (magit-define-popup-action 'magit-branch-popup
    ?R "Toggle rebasing" 'magit-branch-toggle-rebase)
  (magit-define-popup-action 'magit-fetch-popup
    ?p "Pull request" 'magit-branch-pull-request))

(use-package gh                         ; Github API library
  :defer t
  :config (setq gh-profile-default-profile "manuel-uberti"))

(use-package magit-gh-pulls             ; Show Github PRs in Magit
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

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

;;; mu-vers-control.el ends here
