;;; mu-ivy.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

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
   ivy-ignore-buffers '("company-statistics-cache.el" ".elfeed/index")))

(use-package ivy-hydra                  ; Additional bindings for Ivy
  :ensure t
  :after ivy)

(use-package ivy-historian              ; Store minibuffer candidates
  :ensure t
  :init (ivy-historian-mode +1))

(use-package ivy-xref                   ; Ivy interface for xref results
  :ensure t
  :config (validate-setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-yasnippet              ; Preview yasnippets with Ivy
  :ensure t
  :bind ("C-c y" . ivy-yasnippet))

(use-package ivy-bibtex                 ; Ivy interface for BibTeX entries
  :ensure t
  :defer t
  :config (validate-setq ivy-re-builders-alist
                         '((ivy-bibtex . ivy--regex-ignore-order)
                           (t . ivy--regex-plus))))

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

(use-package amx                        ; Better M-x interface
  ;; TODO: waiting for https://github.com/abo-abo/swiper/pull/1585
  :disabled t
  :ensure t)

(use-package counsel                    ; Completion functions with Ivy
  :ensure t
  :init (counsel-mode)
  :bind (("C-c b"   . counsel-ibuffer)
         ("C-c g"   . counsel-git-grep)
         ("C-c G"   . counsel-git-log)
         ("C-c i"   . counsel-apropos)
         ("C-c k"   . counsel-rg)
         ("C-c l"   . counsel-find-library)
         ("C-c n i" . counsel-imenu)
         ("C-c u"   . counsel-unicode-char)
         ("C-r"     . counsel-grep-or-swiper)
         ("C-s"     . counsel-grep-or-swiper)
         ("C-x j"   . counsel-bookmark)
         :map read-expression-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (validate-setq
   ;; Use ripgrep for counsel-git
   counsel-git-cmd "rg --files"
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
   counsel-yank-pop-preselect-last t)

  (validate-setq
   counsel-find-file-ignore-regexp (concat
                                    ;; File names beginning with # or .
                                    "\\(?:\\`[#.]\\)"
                                    ;; File names ending with # or ~
                                    "\\|\\(?:\\`.+?[#~]\\'\\)"))

  (validate-setq counsel-linux-app-format-function
                 #'counsel-linux-app-format-function-name-only))

;;; Utilities and key bindings
;;;###autoload
(defun mu-counsel-search-project (initial-input &optional use-current-dir)
  "Search using `counsel-rg' from the project root for INITIAL-INPUT.
If there is no project root, or if the prefix argument
USE-CURRENT-DIR is set, then search from the current directory
instead."
  (interactive (list (thing-at-point 'symbol)
                     current-prefix-arg))
  (let ((current-prefix-arg)
        (dir (if use-current-dir
                 default-directory
               (condition-case err
                   (projectile-project-root)
                 (error default-directory)))))
    (funcall 'counsel-rg initial-input dir)))

(bind-key* "M-?" #'mu-counsel-search-project)

(provide 'mu-ivy)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-ivy.el ends here
