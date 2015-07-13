;;; custom-vers-control.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my versioning control configuration.

;;; Code:

(use-package magit ; The best Git client out there
  :ensure t
  :bind ("<f3>" . magit-status)
  :config (progn
            (setq magit-revert-buffers 'silent
                  magit-save-repository-buffers 'dontask
                  magit-refs-show-commit-count 'all)

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

            ;; Set `magit-repository-directories' for `magit-status'
            (defun custom/magit-set-repo-dirs-from-projectile ()
              "Set `magit-repository-directories' with known Projectile projects."
              (let ((project-dirs (bound-and-true-p projectile-known-projects)))
                ;; Remove trailing slashes from project directories
                (setq magit-repository-directories
                      (mapcar #'directory-file-name project-dirs))))

            (with-eval-after-load 'projectile
              (custom/magit-set-repo-dirs-from-projectile))

            (add-hook 'projectile-switch-project-hook
                      #'custom/magit-set-repo-dirs-from-projectile)

            ;; Visit Github PRs from Magit
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

            (bind-key "v" #'custom/visit-pull-request-url
                      magit-mode-map)))

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

(provide 'custom-vers-control)

;;; custom-vers-control.el ends here
