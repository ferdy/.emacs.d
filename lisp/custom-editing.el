;;; custom-editing.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores editing customizations.

;;; Code:

(use-package zop-to-char ; Better zap-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package whitespace-cleanup-mode ; Cleanup whitespace in buffers
  :ensure t
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

(use-package undo-tree ; Show buffer changes as a tree
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package delsel ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package syntax-subword ; Make operations on words more fine-grained
  :ensure t
  :init (progn
          (setq syntax-subword-skip-spaces t)
          (global-syntax-subword-mode +1)))

(use-package easy-kill ; Better kill text
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ("C-c e m" . easy-mark)))

(use-package iedit ; Edit multiple occurrences
  :ensure t
  :config (progn
            (defun iedit-dwim (arg)
              "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
              (interactive "P")
              (if arg
                  (iedit-mode)
                (save-excursion
                  (save-restriction
                    (widen)
                    ;; this function determines the scope of `iedit-start'.
                    (if iedit-mode
                        (iedit-done)
                      ;; `current-word' can of course be replaced by other
                      ;; functions.
                      (narrow-to-defun)
                      (iedit-start (current-word) (point-min) (point-max)))))))

            (global-set-key (kbd "C-,") 'iedit-dwim)))

(use-package adaptive-wrap ; Better line wrap
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package aggressive-fill-paragraph ; Automatically fill paragrah
  :ensure t
  :defer t
  :config (progn
            (add-hook 'org-mode-hook #'aggressive-fill-paragraph-mode)
            (add-hook 'TeX-mode-hook #'aggressive-fill-paragraph-mode)))

(use-package visual-fill-column ; Wrap at fill column
  :ensure t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package aggressive-indent ; Automatically indent code
  :ensure t
  :init (global-aggressive-indent-mode 1)
  :config (add-to-list 'aggressive-indent-excluded-modes
                       'cider-repl-mode))

(use-package align ; Align text in buffers
  :bind (("C-c e a" . align)
         ("C-c e c" . align-current)
         ("C-c e r" . align-regexp)))

(use-package ediff-wind ; Better ediff behavior
  :defer 5
  :config (setq ediff-window-setup-function #'ediff-setup-windows-plain
                ediff-split-window-function #'split-window-horizontally))

(use-package multiple-cursors ; Easily place multiple cursor in a buffer
  :ensure t
  :bind (("C-c m e"   . mc/mark-more-like-this-extended)
	 ("C-c m h"   . mc/mark-all-like-this-dwim)
	 ("C-c m l"   . mc/edit-lines)
	 ("C-c m n"   . mc/mark-next-like-this)
	 ("C-c m p"   . mc/mark-previous-like-this)
	 ("C-c m r"   . vr/mc-mark)
	 ("C-c m C-a" . mc/edit-beginnings-of-lines)
	 ("C-c m C-e" . mc/edit-ends-of-lines)
	 ("C-c m C-s" . mc/mark-all-in-region))
  :config (setq mc/mode-line
                ;; Simplify the MC mode line indicator
                '(:propertize (:eval (concat " " (number-to-string
                                                  (mc/num-cursors))))
                              face font-lock-warning-face)))

(use-package multifiles ; Edit multiple files at once
  :ensure t
  :bind ("C-!" . mf/mirror-region-in-multifile))

(use-package macrostep ; Navigate through macros
  :ensure t
  :init (with-eval-after-load 'lisp-mode
          (bind-key "C-c e e" #'macrostep-expand emacs-lisp-mode-map)
          (bind-key "C-c e e" #'macrostep-expand lisp-interaction-mode-map)))

(use-package saveplace ; Save point position in files
  :init (progn
          (setq-default save-place t)
          (save-place-mode 1)))

(use-package autorevert ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode))

(use-package elisp-slime-nav ; Navigate through elisp code with M-. & M-,
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package transpose-mark ; Transpose data by leaving an Emacs mark
  :ensure t                 ; on the line you want to transpose.
  :bind ("C-c t m" . transpose-mark))

(use-package copyright ; Deal with copyright notices
  :defer t
  :bind (("C-c e C" . copyright-update))
  ;; Update copyright when visiting files
  :init (add-hook 'find-file-hook #'copyright-update)
  ;; Use ranges to denote consecutive years
  :config (setq copyright-year-ranges t
                copyright-names-regexp (regexp-quote user-full-name)))

(provide 'custom-editing)

;;; custom-editing.el ends here
