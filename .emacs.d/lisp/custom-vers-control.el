;;; custom-vers-control.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015 Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords:

;;; Commentary:

;; This file stores my versioning control configuration.

;;; Code:

(use-package magit
  :ensure t
  :defer t
  :bind (("<f3>" . magit-status))
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    ;; Shut up, Magit!
    (setq magit-save-some-buffers 'dontask
          magit-stage-all-confirm nil
          magit-unstage-all-confirm nil
          ;; Except when you ask something useful
          magit-set-upstream-on-push t
          magit-auto-revert-mode-lighter "")

    ;; Auto-revert files after Magit operations
    (magit-auto-revert-mode)

    ;; Fullscreen magit-status
    (defadvice magit-status (around magit-fullscreen activate)
      "Turn fullscreen on for magit-status."
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restore the previous window configuration and kill the magit buffer."
      (interactive)
      (custom/kill-buffers "^\\*magit")
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    ;; C-u M-x magit-status prompts a list of known repositories
    (setq magit-repo-dirs
          (mapcar
           (lambda (dir)
             (substring dir 0 -1))
           (cl-remove-if-not
            (lambda (project)
              (unless (file-remote-p project)
                (file-directory-p (concat project "/.git/"))))
            (projectile-relevant-known-projects))))))

(use-package git-commit-mode ; Git commit message mode
  :ensure t
  :defer t)

(use-package gitconfig-mode ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-rebase-mode ; Mode for git rebase -i
  :ensure t
  :defer t)

(provide 'custom-vers-control)

;;; custom-vers-control.el ends here
