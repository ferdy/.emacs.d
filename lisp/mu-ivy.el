;;; mu-ivy.el --- Part of my Emacs configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for Ivy and related packages.

;;; Code:

(use-package ivy                        ; Incremental Vertical completYon
  :ensure t
  :bind (("C-c r"   . ivy-resume)
         ("C-x C-r" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-j" . ivy-immediate-done)
         ("C-o" . hydra-ivy/body))
  :init (ivy-mode 1)
  :config
  (validate-setq
   ivy-count-format "(%d/%d) "          ; Show current match and matches
   ivy-extra-directories nil            ; Do not show "./" and "../"
   ivy-virtual-abbreviate 'full         ; Show full file path
   ivy-dynamic-exhibit-delay-ms 150
   ;; Jump back to first candidate when on the last one
   ivy-wrap t
   ;; Show recently killed buffers when calling `ivy-switch-buffer'
   ivy-use-virtual-buffers t
   ;; Always ignore buffers set in `ivy-ignore-buffers'
   ivy-use-ignore-default 'always
   ;; Ignore some buffers in `ivy-switch-buffer'
   ivy-ignore-buffers '("company-statistics-cache.el" ".elfeed/index"))
  :diminish ivy-mode)

(use-package ivy-hydra                  ; Additional bindings for Ivy
  :ensure t
  :after ivy)

(use-package ivy-historian              ; Store minibuffer candidates
  :ensure t
  :init (ivy-historian-mode +1))

(use-package ivy-xref                   ; Ivy interface for xref results
  :ensure t
  :config
  (validate-setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package swiper                     ; Isearch with an overview
  :ensure t
  :bind ("C-c s s" . swiper-all)
  :config
  (validate-setq
   ;; Always recentre when leaving Swiper
   swiper-action-recenter t
   ;; Jump to the beginning of match when leaving Swiper
   swiper-goto-start-of-match t
   ;; C-k C-g to go back to where the research started
   swiper-stay-on-quit t))

(use-package smex                       ; Better M-x interface
  :ensure t)

(use-package counsel                    ; Completion functions with Ivy
  :ensure t
  :init (counsel-mode)
  :bind (("C-s"     . counsel-grep-or-swiper)
         ("C-r"     . counsel-grep-or-swiper)
         ("C-c g"   . counsel-git-grep)
         ("C-c G"   . counsel-git-log)
         ("C-c i"   . counsel-apropos)
         ("C-c k"   . counsel-rg)
         ("C-c l"   . counsel-find-library)
         ("C-c u"   . counsel-unicode-char)
         ("C-x j"   . counsel-bookmark)
         :map read-expression-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (validate-setq
   ;; Use ripgrep instead of regular grep
   counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never %s %s"
   counsel-rg-base-command
   "rg -i -M 120 --no-heading --line-number --color never %s .")

  (validate-setq
   counsel-mode-override-describe-bindings t
   counsel-describe-function-function 'helpful-function
   counsel-describe-variable-function 'helpful-variable
   counsel-grep-post-action-hook '(recenter)
   counsel-yank-pop-preselect-last t
   counsel-find-file-at-point t)

  (validate-setq
   counsel-find-file-ignore-regexp (concat
                                    ;; File names beginning with # or .
                                    "\\(?:\\`[#.]\\)"
                                    ;; File names ending with # or ~
                                    "\\|\\(?:\\`.+?[#~]\\'\\)"))

  (validate-setq counsel-linux-app-format-function
                 #'counsel-linux-app-format-function-name-only)
  :diminish counsel-mode)

(use-package counsel-tramp          ; Ivy interface for SSH and Docker via TRAMP
  :ensure t
  :bind ("C-c a t t" . counsel-tramp))

(provide 'mu-ivy)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-ivy.el ends here
