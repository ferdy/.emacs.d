;;; mu-helm.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my Helm configuration.

;;; Code:

;;; Core setup
(use-package helm ; The ubiquitous Helm
  :ensure t
  :init (helm-mode 1)
  :config
  (progn
    (bind-keys :map helm-map
               ("C-i" . helm-execute-persistent-action)
               ("C-z" . helm-select-action))

    (setq helm-split-window-in-side-p t ; Open buffer in current window
          ;; Move to end/beginning when reaching top/bottom of source
          helm-move-to-line-cycle-in-source t
          ;; Scroll 8 lines using M-<next>/M-<prior>
          helm-scroll-amount 8
          ;; Fuzzy matching
          helm-mode-fuzzy-match t
          helm-completion-in-region-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-lisp-fuzzy-completion t
          ;; Cleaner Helm interface
          helm-display-header-line nil)

    (helm-adaptive-mode 1)   ; Adaptive sorting in all sources
    (helm-autoresize-mode 1) ; Autoresize Helm buffer

    ;; Man pages at point
    (add-to-list 'helm-sources-using-default-as-input
                 'helm-source-man-pages))
  :diminish helm-mode)

(use-package helm-config ; Applications library for `helm.el'
  :ensure helm
  :after helm
  :config
  (progn
    (bind-key "C-c h" helm-command-prefix)
    (unbind-key "C-x c")))

(use-package helm-command ; M-x in Helm
  :ensure helm
  :bind ([remap execute-extended-command] . helm-M-x))

(use-package helm-files ; Find files with Helm
  :ensure helm
  :bind (([remap find-file] . helm-find-files)
         ("C-x C-r"         . helm-recentf))
  :config
  (progn
    (bind-key "C-k" #'helm-ff-persistent-delete helm-find-files-map)

    ;; `helm-recentf-fuzzy-match' is set via Customize
    ;; Reason: https://emacs.stackexchange.com/a/106/5514
    (setq helm-ff-file-name-history-use-recentf t
          helm-ff-newfile-prompt-p nil ; Don't prompt for new buffer
          helm-idle-delay 0.1
          helm-input-idle-delay 0.1
          ;; Don't show boring files
          helm-ff-skip-boring-files t
          ;; Search for library in `require' and `declare-function' sexp
          helm-ff-search-library-in-sexp t
          ;; Auto-complete in find-files
          helm-ff-auto-update-initial-value t
          ;; Sort directories first
          helm-find-files-sort-directories t)))

(use-package helm-misc ; Misc Helm commands
  :ensure helm
  :bind ([remap switch-to-buffer] . helm-mini))

(use-package helm-buffers ; Manage buffers with Helm
  :ensure helm
  :bind ("C-c C-b" . helm-buffers-list)
  :config
  (progn
    (setq helm-buffers-fuzzy-matching t)
    (bind-key "C-k" #'helm-buffer-run-kill-persistent
              helm-buffer-map)))

(use-package helm-ring ; Helm commands for rings
  :ensure helm
  :bind (([remap yank-pop]        . helm-show-kill-ring)
         ("C-h SPC"               . helm-all-mark-rings)))

;;; Projects and navigation
(use-package helm-imenu ; Imenu through Helm
  :ensure helm
  :bind (("C-c n i" . helm-imenu-in-all-buffers)
         ("C-c n t" . helm-imenu))
  :config (setq helm-imenu-fuzzy-match t
                helm-imenu-execute-action-at-once-if-one nil))

(use-package helm-register ; Display registers with Helm
  :ensure helm
  :bind ([remap insert-register] . helm-register))

(use-package helm-bookmarks ; List bookmarks with Helm
  :ensure helm
  :bind ("C-x r l" . helm-bookmarks))

(use-package helm-pages ; Move between pages in current buffer with Helm
  :ensure t
  :bind ("C-c n P" . helm-pages))

(use-package helm-projectile ; Projectile through Helm
  :ensure t
  :after projectile
  :init (helm-projectile-on)
  :config (setq projectile-switch-project-action #'helm-projectile))

;;; Programming
(use-package helm-eval ; Evaluate expressions with Helm
  :ensure helm
  :bind (("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("C-c h *"   . helm-calcul-expression)))

(use-package helm-elisp ; Helm commands for Emacs Lisp
  :ensure helm
  :bind (("C-c h a" . helm-apropos)
         ("C-c h l" . helm-locate-library))
  :config (setq helm-apropos-fuzzy-match t))

(use-package cljr-helm ; Helm interface for clj-refactor
  :ensure t
  :bind ("C-c h h c" . cljr-helm))

(use-package helm-flycheck ; Show Flycheck errors with Helm
  :ensure t
  :bind ("C-c h h f" . helm-flycheck))

;;; Help
(use-package helm-info ; Helm tools for Info
  :ensure helm
  :bind (("C-c h h e" . helm-info-emacs)
         ("C-c h h i" . helm-info-at-point)))

(use-package helm-man ; Manual pages through Helm
  :ensure helm
  :bind ("C-c h m" . helm-man-woman))

(use-package helm-descbinds ; Describing keybinding through Helm
  :ensure t
  :bind ("C-c h d" . helm-descbinds))

;;; Search
(use-package helm-regex ; Helm regex tools
  :ensure helm
  :bind (("C-c M-o"     . helm-occur)
         ([remap occur] . helm-occur)
         ("C-c h o"     . helm-multi-occur)))

(use-package helm-swoop ; List matching lines in another buffer
  :ensure t
  :bind (("M-i"     . helm-swoop)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (progn
    ;; When doing isearch, hand the word over to helm-swoop
    (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map)
    ;; From helm-swoop to helm-multi-swoop-all
    (bind-key "M-i" #'helm-multi-swoop-all-from-helm-swoop
              helm-swoop-map)

    ;; Move up and down like isearch
    (bind-keys :map helm-swoop-map
               ("C-r" . helm-previous-line)
               ("C-s" . helm-next-line))

    (bind-keys :map helm-multi-swoop-map
               ("C-r" . helm-previous-line)
               ("C-s" . helm-next-line))

    (setq helm-multi-swoop-edit-save t ; Save when done with editing
          ;; If this value is t, split window inside the current window
          helm-swoop-split-with-multiple-windows nil
          ;; Split direction
          helm-swoop-split-direction 'split-window-vertically
          ;; If nil, boost invoke speed in exchange for text color
          helm-swoop-speed-or-color nil
          ;; Go to the opposite side from the end or beginning of line
          helm-swoop-move-to-line-cycle t
          ;; Optional face for line numbers
          helm-swoop-use-line-number-face t)

    ;; Match/Search methods (Fuzzy matching)
    (defvar helm-c-source-swoop-match-functions
      '(helm-mm-exact-match
        helm-mm-match
        helm-fuzzy-match))
    (setq helm-c-source-swoop-search-functions
          '(helm-mm-exact-search
            helm-mm-search
            helm-candidates-in-buffer-search-default-fn
            helm-fuzzy-search))))

(use-package helm-ag ; Helm frontend for Ag
  :ensure t
  :bind ("C-c h s" . helm-do-ag)
  :config (setq helm-ag-fuzzy-match t
                helm-ag-insert-at-point 'symbol
                helm-ag-source-type 'file-line))

;;; Completion
(use-package helm-company ; Show Company candidates through Helm
  :ensure t
  :after company
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'helm-company company-mode-map)
    (bind-key "C-:" #'helm-company company-mode-map)
    (bind-key "C-:" #'helm-company company-active-map)))

(use-package helm-c-yasnippet ; Helm source for Yasnippet
  :ensure t
  :after yasnippet
  :init (bind-key "C-c h y" #'helm-yas-complete)
  :config (setq helm-yas-space-match-any-greedy t))

;;; Languages
(use-package helm-flyspell ; Use Flyspell with Helm
  :ensure t
  :bind ("C-c h f" . helm-flyspell-correct))

(use-package helm-ispell ; Complete word using Ispell with Helm
  :ensure t
  :bind ("C-c h i" . helm-ispell))

(use-package helm-wordnet ; Helm interface for Wordnet dictionary
  :ensure t
  :bind ("C-c h w" . helm-wordnet)
  :config (setq helm-wordnet-wordnet-location "/usr/share/wordnet"))

;;; Input
(use-package helm-color ; Input colors with Helm
  :ensure helm
  :bind ("C-c h c" . helm-colors))

(use-package helm-unicode ; Browse Unicode characters by name
  :ensure t
  :bind ("C-c h 8" . helm-unicode))

;;; Shell integration
(use-package helm-shell ; Manage shells/terms with Helm
  :ensure helm
  :defer t
  :init
  (progn
    ;; Shell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (bind-key "C-c C-l"
                            #'helm-eshell-history
                            eshell-mode-map)))
    (bind-key "C-c C-l" #'helm-comint-input-ring shell-mode-map)

    ;; Completion with helm
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (bind-key [remap eshell-pcomplete]
                            'helm-esh-pcomplete
                            eshell-mode-map)))))

;;; Version control tools
(use-package helm-gitignore ; Generate .gitignore files with gitignore.io
  :ensure t
  :bind ("C-c h v g" . helm-gitignore))

;;; Extra utilities
(use-package helm-external ; Run external commands through Helm
  :ensure helm
  :bind ("C-c h x" . helm-run-external-command))

(use-package helm-bibtex ; Manage BibTeX bibliographies with Helm
  :ensure t
  :bind ("C-c h h b" . helm-bibtex))

(provide 'mu-helm)

;;; mu-helm.el ends here
