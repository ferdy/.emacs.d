;;; custom-vers-control.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my versioning control configuration.

;;; Code:

(use-package magit
  :load-path "site-lisp/magit/lisp"
  :bind ("<f3>" . magit-status)
  :config (progn
            (defadvice magit-status (around magit-fullscreen activate)
              "Turn fullscreen on for magit-status."
              (window-configuration-to-register :magit-fullscreen)
              ad-do-it
              (delete-other-windows))

            (defun magit-quit-session ()
              "Restore previous window configuration and cleanup buffers."
              (interactive)
              (custom/kill-buffers "^\\*magit")
              (jump-to-register :magit-fullscreen))

            (bind-key "q" #'magit-quit-session magit-status-mode-map)

            ;; Set Magit's repo dirs for `magit-status' from Projectile's known
            ;; projects.
            (defun custom/magit-set-repo-dirs-from-projectile ()
              "Set `magit-repo-dirs' from known Projectile projects."
              (let ((project-dirs (bound-and-true-p projectile-known-projects)))
                ;; Remove trailing slashes from project directories
                (setq magit-repository-directories
                      (mapcar #'directory-file-name project-dirs))))

            (with-eval-after-load 'projectile
              (custom/magit-set-repo-dirs-from-projectile))

            (add-hook 'projectile-switch-project-hook
                      #'custom/magit-set-repo-dirs-from-projectile)

            ;; Automatically access Github PRs
            (defun custom/add-PR-fetch ()
              "If refs/pull is not defined on a GH repo, define it."
              (let ((fetch-address
                     "+refs/pull/*/head:refs/pull/origin/*")
                    (magit-remotes
                     (magit-get-all "remote" "origin" "fetch")))
                (unless (or (not magit-remotes)
                            (member fetch-address magit-remotes))
                  (when (string-match
                         "github" (magit-get "remote" "origin" "url"))
                    (magit-git-string
                     "config" "--add" "remote.origin.fetch"
                     fetch-address)))))

            (add-hook 'magit-mode-hook #'custom/add-PR-fetch)

            ;; Create Github PRs from Magit
            (defun custom/visit-pull-request-url ()
              "Visit the current branch's PR on Github."
              (interactive)
              (browse-url
               (format "https://github.com/%s/pull/new/%s"
                       (replace-regexp-in-string
                        "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                        (magit-get "remote"
                                   (magit-get-remote)
                                   "url"))
                       (cdr (magit-get-remote-branch)))))

            (eval-after-load 'magit
              '(define-key magit-mode-map "v"
                 #'custom/visit-pull-request-url))))

(use-package gitconfig-mode ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode ; Git attributes mode
  :ensure t
  :defer t)

(provide 'custom-vers-control)

;;; custom-vers-control.el ends here
