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
  :bind (("<f3>" . magit-status)
         ("C-c v s" . magit-status)
         ("C-c v l" . magit-file-log))
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config (progn
            ;; Shut up, Magit!
            (setq magit-save-some-buffers 'dontask
                  magit-stage-all-confirm nil
                  magit-unstage-all-confirm nil
                  ;; Except when you ask something useful
                  magit-set-upstream-on-push t
                  magit-auto-revert-mode-lighter "")

            (defadvice magit-status (around magit-fullscreen activate)
              "Turn fullscreen on for magit-status."
              (window-configuration-to-register :magit-fullscreen)
              ad-do-it
              (delete-other-windows))

            (defun magit-quit-session ()
              "Restore previous window configuration and cleanup buffers."
              (interactive)
              (custom/kill-buffers "^\\*magit")
              (custom/kill-buffers "^\\*Springboard")
              (tramp-cleanup-all-buffers)
              (tramp-cleanup-all-connections)
              (jump-to-register :magit-fullscreen))

            (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
  :diminish magit-auto-revert-mode)

(use-package magit-gh-pulls ; Manage git pull requests from Magit
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

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
