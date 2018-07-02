;;; mu-vers-control.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for version control tools.

;;; Code:

(use-package vc-hooks                   ; Simple version control
  :bind (("S-<f5>" . vc-revert)
         ("C-c v r" . vc-refresh-state))
  :config (validate-setq vc-follow-symlinks t))

(use-package magit                      ; The best Git client out there
  :ensure t
  :bind (("C-c v c" . magit-clone)
         ("C-c v d" . magit-dispatch-popup)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v v" . magit-status)
         (:map magit-mode-map
               ([remap previous-line] . magit-previous-line)
               ([remap next-line] . magit-next-line))

         (:map dired-mode-map
               ("l" . magit-dired-log)))
  :config
  (validate-setq
   magit-save-repository-buffers 'dontask
   magit-refs-show-commit-count 'all
   magit-branch-prefer-remote-upstream '("master")
   magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))
   magit-completing-read-function 'ivy-completing-read
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18))

  ;; Hide "Recent Commits"
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  ;; Show refined hunks during diffs
  (set-default 'magit-diff-refine-hunk t)

  (add-hook 'projectile-switch-project-hook
            #'mu-magit-set-repo-dirs-from-projectile)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)

  ;; Refresh VC state when Magit refreshes the buffer to keep ibuffer-vc in sync
  (add-hook 'magit-refresh-buffer-hook #'vc-refresh-state)

  ;; Free C-c C-w for Eyebrowse
  (unbind-key "C-c C-w" git-commit-mode-map)

  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map))

(use-package magit-files                ; Magit file operations
  :ensure magit
  :config
  ;; Add more operations to the file popup
  (magit-define-popup-action 'magit-file-popup
    ?R "Rename file" 'magit-file-rename)
  (magit-define-popup-action 'magit-file-popup
    ?K "Delete file" 'magit-file-delete)
  (magit-define-popup-action 'magit-file-popup
    ?U "Untrack file" 'magit-file-untrack)
  (magit-define-popup-action 'magit-file-popup
    ?C "Checkout file" 'magit-file-checkout))

(use-package magit-gitflow              ; gitflow extension for Magit
  :ensure t
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow)
  :config
  ;; Free C-f and use a more suitable key binding
  (unbind-key "C-f" magit-gitflow-mode-map)
  (bind-key "C-c v f" #'magit-gitflow-popup magit-gitflow-mode-map))

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

(use-package git-timemachine            ; Walk through revisions of a file
  :ensure t
  :bind ("C-c v t" . git-timemachine))

(use-package rigid-tabs                 ; Fix TAB alignment in diff buffers
  :ensure t
  :hook ((diff-mode-hook . rigid-tabs-diff-align)
         (magit-refresh-buffer-hook . rigid-tabs-diff-align)))

(provide 'mu-vers-control)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-vers-control.el ends here
