;;; mu-vers-control.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (2014-C) 2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for version control tools.

;;; Code:

(use-package vc-hooks                   ; Simple version control
  :bind ("C-c v r" . vc-refresh-state)
  :config
  ;; Always follow symlinks to files in VCS repos
  (validate-setq vc-follow-symlinks t))

(use-package magit                      ; The best Git client out there
  :ensure t
  :bind (("C-c v c" . magit-clone)
         ("C-c v C" . magit-checkout)
         ("C-c v d" . magit-dispatch-popup)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v p" . magit-pull)
         ("C-c v v" . mu-magit-open))
  :config
  (validate-setq
   magit-save-repository-buffers 'dontask
   magit-refs-show-commit-count 'all
   magit-branch-prefer-remote-upstream '("master")
   magit-branch-adjust-remote-upstream-alist '(("origin/master" "master")))

  ;; Hide "Recent Commits"
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  ;; Show refined hunks during diffs
  (set-default 'magit-diff-refine-hunk t)

  ;; Use Ivy
  (validate-setq magit-completing-read-function 'ivy-completing-read)

  ;; Show status buffer in fullscreen
  (validate-setq
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  (defun mu-magit-open ()
    "Open Magit status after storing current window configuration."
    (interactive)
    (mu-push-window-configuration)
    (magit-status))

  (defun mu-quit-magit-session ()
    "Kill all Magit related buffers when closing Magit status."
    (interactive)
    (mu-pop-window-configuration)
    (mu-kill-buffers "^\\*magit"))

  ;; Show status buffer in fullscreen
  (with-eval-after-load 'magit
    (fullframe magit-status mu-quit-magit-session))

  (bind-key "q" #'mu-quit-magit-session magit-status-mode-map)

  (add-hook 'projectile-switch-project-hook
            #'mu-magit-set-repo-dirs-from-projectile)

  ;; Refresh `diff-hl' accordingly
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)

  ;; Free C-c C-w for Eyebrowse
  (unbind-key "C-c C-w" git-commit-mode-map)
  :diminish (magit-wip-after-save-local-mode
             magit-wip-before-change-mode))

(use-package magit-gitflow              ; gitflow extension for Magit
  :ensure t
  :after magit
  :config
  ;; Free C-f and use a more suitable key binding
  (unbind-key "C-f" magit-gitflow-mode-map)
  (bind-key "C-c v f" #'magit-gitflow-popup magit-gitflow-mode-map)

  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  :diminish magit-gitflow-mode)

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
