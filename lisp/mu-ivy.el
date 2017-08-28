;;; mu-ivy.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Ivy and related packages.

;;; Code:

(use-package ivy                        ; Incremental Vertical completYon
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-r" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-o" . hydra-ivy/body))
  :init (ivy-mode 1)
  :config
  (validate-setq
   ivy-count-format "(%d/%d) "          ; Show current match and matches
   ivy-extra-directories nil            ; Do not show "./" and "../"
   ivy-virtual-abbreviate 'full         ; Show full file path
   ;; Jump back to first candidate when on the last one
   ivy-wrap t
   ;; Show recently killed buffers when calling `ivy-switch-buffer'
   ivy-use-virtual-buffers t
   ;; Always ignore buffers set in `ivy-ignore-buffers'
   ivy-use-ignore-default 'always
   ;; Ignore some buffers in `ivy-switch-buffer'
   ivy-ignore-buffers '("company-statistics-cache.el"
                        ".elfeed/index"))

  ;; Bind C-k to kill buffer from `ivy-switch-buffer'
  (defun mu-ivy-kill-buffer ()
    (interactive)
    (ivy-set-action 'kill-buffer)
    (ivy-done))

  (bind-key "C-k" #'mu-ivy-kill-buffer ivy-switch-buffer-map)
  :diminish ivy-mode)

(use-package ivy-hydra                  ; Additional bindings for Ivy
  :ensure t
  :defer t
  :after ivy)

(use-package ivy-pages                 ; Jump to pages with Ivy
  :ensure t
  :defer t
  :bind ("C-c n p" . ivy-pages))

(use-package ivy-historian              ; Store minibuffer candidates
  :ensure t
  :init (ivy-historian-mode +1))

(use-package swiper                     ; Isearch with an overview
  :ensure t
  :bind (("C-c s s" . swiper-all)
         :map isearch-mode-map
         ("M-i" . swiper-from-isearch))
  :config
  (validate-setq
   ;; Always recentre when leaving Swiper
   swiper-action-recenter t
   ;; Jump to the beginning of match when leaving Swiper
   swiper-goto-start-of-match t))

(use-package smex                       ; Better M-x interface
  :ensure t)

(use-package counsel                    ; Completion functions with Ivy
  :ensure t
  :init (counsel-mode)
  :bind (("C-s"     . counsel-grep-or-swiper)
         ("C-r"     . counsel-grep-or-swiper)
         ("C-c h h" . counsel-command-history)
         ("C-c g"   . counsel-git-grep)
         ("C-c G"   . counsel-git-log)
         ("C-c i"   . counsel-apropos)
         ("C-c k"   . counsel-rg)
         ("C-c l"   . counsel-find-library)
         ("C-c u"   . counsel-unicode-char)
         ("C-x j"   . counsel-bookmark)
         ("C-x l"   . counsel-locate)
         ("M-i"     . counsel-imenu)
         ([remap execute-extended-command] . counsel-M-x)
         ([remap find-file]                . counsel-find-file)
         ([remap bookmark-jump]            . counsel-bookmark)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol))
  :bind (:map read-expression-map
              ("C-r" . counsel-expression-history))
  :config
  ;; Use ripgrep instead of regular grep
  (validate-setq
   counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

  (validate-setq
   counsel-bookmark-avoid-dired t
   counsel-mode-override-describe-bindings t
   counsel-grep-post-action-hook '(recenter)
   counsel-find-file-at-point t
   counsel-find-file-ignore-regexp
   (concat
    ;; File names beginning with # or .
    "\\(?:\\`[#.]\\)"
    ;; File names ending with # or ~
    "\\|\\(?:\\`.+?[#~]\\'\\)"))
  :diminish counsel-mode)

(provide 'mu-ivy)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-ivy.el ends here
