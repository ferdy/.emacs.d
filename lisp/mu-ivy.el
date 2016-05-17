;;; mu-ivy.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Ivy and related packages.

;;; Code:

(use-package ivy                        ; Incremental Vertical completYon
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-r" . ivy-switch-buffer))
  :init (ivy-mode 1)
  :config
  (setq ivy-count-format ""             ; Suppress counter
        ivy-extra-directories nil       ; Do not show "./" and "../"
        ivy-virtual-abbreviate 'full    ; Show full file path
        ;; Show recently killed buffers when calling `ivy-switch-buffer'
        ivy-use-virtual-buffers t
        ;; Always ignore buffers set in `ivy-ignore-buffers'
        ivy-use-ignore-default 'always
        ;; Ignore some buffers in `ivy-switch-buffer'
        ivy-ignore-buffers '("company-statistics-cache.el"))

  ;; Speed up my workflow with prearranged windows
  (setq ivy-views '(("boccaperta + ba-server [–]"
                     (vert
                      (sexp (bookmark-jump "boccaperta"))
                      (sexp (bookmark-jump "ba-server"))))
                    ("desktop + ba-server [–]"
                     (vert
                      (sexp (bookmark-jump "desktop"))
                      (sexp (bookmark-jump "ba-server"))))))

  (defun ivy-insert-action (x)
    "Insert X at point."
    (with-ivy-window
      (insert x)))

  (ivy-set-actions
   t
   '(("I" ivy-insert-action "insert")))
  :diminish ivy-mode)

(use-package ivy-hydra                  ; Additional bindings for Ivy
  :ensure t
  :after ivy)

(use-package swiper                     ; Isearch with an overview
  :ensure t
  :bind (("C-c s s" . swiper-all)
         :map isearch-mode-map
         ("M-i" . swiper-from-isearch))
  :config (setq swiper-action-recenter t))

(use-package smex                       ; Better M-x interface
  :ensure t)

(use-package counsel                    ; Completion functions with Ivy
  :ensure t
  :init
  (setq counsel-mode-override-describe-bindings t)
  (counsel-mode 1)
  :bind (("C-s"   . counsel-grep-or-swiper)
         ("C-r"   . counsel-grep-or-swiper)
         ("C-c u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c G" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-x i" . counsel-imenu)
         ("C-c r" . counsel-linux-app))
  :bind (:map read-expression-map
              ("C-r" . counsel-expression-history))
  :config
  (setq counsel-find-file-at-point t
        counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))
  :diminish counsel-mode)

(provide 'mu-ivy)

;;; mu-ivy.el ends here
