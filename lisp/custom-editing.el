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
         ([remap mark-sexp] . easy-mark)))

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

(use-package expand-region ; Expand selected region
  :ensure t
  :bind ("M-E" . er/expand-region))

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

(use-package paredit ; Balanced sexp editing
  :ensure t
  :defer t
  :init (dolist (hook '(eval-expression-minibuffer-setup-hook
                        emacs-lisp-mode-hook
                        inferior-emacs-lisp-mode-hook
                        clojure-mode-hook))
          (add-hook hook #'paredit-mode))
  :config (progn
            ;; Free M-s. There are some useful bindings in that prefix map.
            (define-key paredit-mode-map (kbd "M-s") nil)
            (define-key paredit-mode-map (kbd "M-S-<up>")
              #'paredit-splice-sexp)

            ;; Extreme barfage and slurpage
            (defun paredit-barf-all-the-way-backward ()
              (interactive)
              (paredit-split-sexp)
              (paredit-backward-down)
              (paredit-splice-sexp))

            (defun paredit-barf-all-the-way-forward ()
              (interactive)
              (paredit-split-sexp)
              (paredit-forward-down)
              (paredit-splice-sexp)
              (if (eolp) (delete-horizontal-space)))

            (defun paredit-slurp-all-the-way-backward ()
              (interactive)
              (catch 'done
                (while (not (bobp))
                  (save-excursion
                    (paredit-backward-up)
                    (if (eq (char-before) ?\()
                        (throw 'done t)))
                  (paredit-backward-slurp-sexp))))

            (defun paredit-slurp-all-the-way-forward ()
              (interactive)
              (catch 'done
                (while (not (eobp))
                  (save-excursion
                    (paredit-forward-up)
                    (if (eq (char-after) ?\))
                        (throw 'done t)))
                  (paredit-forward-slurp-sexp))))

            (nconc paredit-commands
                   '("Extreme Barfage & Slurpage"
                     (("C-M-)" "M-N")
                      paredit-slurp-all-the-way-forward
                      ("(foo (bar |baz) quux zot)"
                       "(foo (bar |baz quux zot))")
                      ("(a b ((c| d)) e f)"
                       "(a b ((c| d)) e f)"))
                     (("C-M-}" "M-F")
                      paredit-barf-all-the-way-forward
                      ("(foo (bar |baz quux) zot)"
                       "(foo (bar|) baz quux zot)"))
                     (("C-M-(" "M-P")
                      paredit-slurp-all-the-way-backward
                      ("(foo bar (baz| quux) zot)"
                       "((foo bar baz| quux) zot)")
                      ("(a b ((c| d)) e f)"
                       "(a b ((c| d)) e f)"))
                     (("C-M-{" "M-B")
                      paredit-barf-all-the-way-backward
                      ("(foo (bar baz |quux) zot)"
                       "(foo bar baz (|quux) zot)"))))

            (paredit-define-keys)
            (paredit-annotate-mode-with-examples)
            (paredit-annotate-functions-with-examples))
  :diminish paredit-mode)

(use-package redshank ; Lisp editing extension
  :ensure t
  :defer t
  :init (progn
          (setq redshank-prefix-key "C-c C-r") ; Change default prefix
          (dolist (hook '(eval-expression-minibuffer-setup-hook
                          emacs-lisp-mode-hook
                          inferior-emacs-lisp-mode-hook
                          clojure-mode-hook))
            (add-hook hook #'redshank-mode)))
  :diminish redshank-mode)

(add-hook 'after-save-hook ; Look for unbalanced parens when saving
          'check-parens nil t)

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
          (bind-key "C-c e m" #'macrostep-expand emacs-lisp-mode-map)
          (bind-key "C-c e m" #'macrostep-expand lisp-interaction-mode-map)))

(use-package saveplace ; Save point position in files
  :init (save-place-mode))

(use-package autorevert ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode))

(use-package elisp-slime-nav ; Navigate through elisp code with M-. & M-,
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package transpose-mark ; Transpose data by leaving an Emacs mark
  :ensure t                 ; on the line you want to transpose.
  :bind ("C-c t m" . transpose-mark))

;;; Windows and frames
(use-package winner ; Undo and redo window configurations
  :init (winner-mode))

(use-package avy-jump ; Jump to characters in buffers
  :ensure avy
  :bind (("C-c j s" . avy-isearch)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j j" . avy-goto-char-2))
  :config (setq avy-keys ; Use home row
                '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?v ?r ?u)))

(use-package ace-link ; Jump to links
  :ensure t
  :defer t
  :init (progn (with-eval-after-load 'info
                 (bind-key "C-c j l" #'ace-link-info Info-mode-map))
               (with-eval-after-load 'help-mode
                 (defvar help-mode-map) ; Silence the byte compiler
                 (bind-key "C-c j l" #'ace-link-help help-mode-map))))

(use-package ace-window ; Better movements between windows
  :ensure t
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys ; Use home row
                '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-dispatch-always t))

(provide 'custom-editing)

;;; custom-editing.el ends here



