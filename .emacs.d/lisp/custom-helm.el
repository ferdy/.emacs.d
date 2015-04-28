;;; custom-helm.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my Helm configuration.

;;; Code:
 
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-c h i" . helm-semantic-or-imenu)
         ("C-c h o" . helm-occur)
         ("C-c h /" . helm-find-with-prefix-arg)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-c h x" . helm-register)
         ("C-c h M-:" . helm-eval-expression-with-eldoc)
         ("C-x r l" . helm-bookmarks)
         ("C-c h r" . helm-info-emacs)
         ("C-c h l" . helm-locate-library))
  :init (helm-mode 1)
  :config (progn
            (require 'helm-config)
            (global-set-key (kbd "C-c h") 'helm-command-prefix)
            (global-unset-key (kbd "C-x c"))

            (bind-keys :map helm-map
                       ("<tab>" . helm-execute-persistent-action)
                       ("C-i" . helm-execute-persistent-action)
                       ("C-z" . helm-select-action))

            (helm-adaptive-mode 1) ; Adaptive sorting in all sources

            ;; Call helm-find with C-u
            (defun helm-find-with-prefix-arg ()
              (interactive)
              (setq current-prefix-arg '(4)) ; C-u
              (call-interactively 'helm-find))

            (setq helm-split-window-in-side-p t ; Open buffer in current window
                  ;; Move to end/beginning when reaching top/bottom of source
                  helm-move-to-line-cycle-in-source t
                  ;; Scroll 8 lines using M-<next>/M-<prior>
                  helm-scroll-amount 8
                  ;; Fuzzy matching
                  helm-M-x-fuzzy-match t
                  helm-semantic-fuzzy-match t
                  helm-imenu-fuzzy-match t
                  helm-apropos-fuzzy-match t
                  helm-lisp-fuzzy-completion t
                  ;; Cleaner Helm interface
                  helm-display-header-line nil)

            (helm-autoresize-mode 1) ; Autoresize Helm buffer

            ;; Better Helm buffer management
            (add-to-list 'display-buffer-alist
                         `(,(rx bos "*helm" (* not-newline) "*" eos)
                           (display-buffer-in-side-window)
                           (inhibit-same-window . t)
                           (window-height . 0.4)))
            
            ;; Man pages at point
            (add-to-list 'helm-sources-using-default-as-input
                         'helm-source-man-pages))
  :diminish helm-mode)

(use-package helm-files ; Find files with Helm
  :ensure helm
  :defer t
  :bind (("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf))
  :config (progn
            (bind-keys :map helm-find-files-map
                       ("C-k" . helm-ff-persistent-delete))
            
            (setq helm-ff-file-name-history-use-recentf t
                  helm-ff-newfile-prompt-p nil ; Don't prompt for new buffer
                  helm-idle-delay 0.1
                  helm-input-idle-delay 0.1
                  ;; Don't show boring files
                  helm-ff-skip-boring-files t
                  ;; Search for library in `require' and `declare-function' sexp
                  helm-ff-search-library-in-sexp t
                  ;; Fuzzy matching
                  helm-recentf-fuzzy-match t
                  ;; Auto-complete in find-files
                  helm-ff-auto-update-initial-value t
                  ;; Sort directories first
                  helm-find-files-sort-directories t)))

(use-package helm-buffers ; Manage buffers with Helm
  :ensure helm
  :defer t
  :config (progn
            (bind-keys :map helm-buffer-map
                       ("C-k" . helm-buffer-run-kill-persistent))

            ;; Fuzzy matching
            (setq helm-buffers-fuzzy-matching t)))

(use-package helm-shell ; Manage shells/terms with Helm
  :ensure helm
  :defer t
  :init (progn
          (add-hook 'eshell-mode-hook
                    #'(lambda ()
                        (define-key eshell-mode-map (kbd "C-c C-l")
                          'helm-eshell-history)))

          (bind-keys :map shell-mode-map ; Shell history
                     ("C-c C-l" . helm-comint-input-ring))

          (bind-keys :map minibuffer-local-map ; Mini-buffer history
                     ("C-c C-l" . helm-minibuffer-history))))

(use-package helm-projectile ; Helm interface for Projectile
  :ensure t
  :defer 10
  :disabled t
  :init (helm-projectile-on))

(use-package helm-ag ; Helm interface for Ag
  :ensure t
  :commands helm-ag
  :bind (("C-c M-s" . helm-ag-with-prefix-arg))
  :config (progn
            ;; Call helm-ag with C-u
            (defun helm-ag-with-prefix-arg ()
              (interactive)
              (setq current-prefix-arg '(4)) ; C-u
              (call-interactively 'helm-ag))))

(use-package helm-swoop ; List matching lines in another buffer
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config (progn
            ;; When doing isearch, hand the word over to helm-swoop
            (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
            ;; From helm-swoop to helm-multi-swoop-all
            (define-key helm-swoop-map (kbd "M-i")
              'helm-multi-swoop-all-from-helm-swoop)

            ;; Move up and down like isearch
            (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
            (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
            (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
            (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

            ;; Save buffer when helm-multi-swoop-edit complete
            (setq helm-multi-swoop-edit-save t)

            ;; If this value is t, split window inside the current window
            (setq helm-swoop-split-with-multiple-windows nil)

            ;; Split direction.
            (setq helm-swoop-split-direction 'split-window-horizontally)

            ;; If nil, you can boost invoke speed in exchange for text color
            (setq helm-swoop-speed-or-color nil)

            ;; Go to the opposite side of line from the end or beginning of line
            (setq helm-swoop-move-to-line-cycle t)

            ;; Optional face for line numbers
            (setq helm-swoop-use-line-number-face t)))

(use-package helm-company ; Show Company candidates through Helm interface
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          ;; Use Company for completion
          (bind-key [remap completion-at-point] #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-mode-map)
          (bind-key "C-:" #'helm-company company-active-map)))

(use-package helm-descbinds ; Describing keybinding through Helm
  :ensure t
  :commands helm-descbinds
  :bind (("C-c h d" . helm-descbinds)))

(use-package helm-flyspell ; Use Flyspell with Helm
  :ensure t
  :commands helm-flyspell-correct
  :bind (("C-c h f" . helm-flyspell-correct)))

(use-package helm-flycheck ; Show Flycheck errors with Helm
  :ensure t
  :commands helm-flycheck
  :bind (("C-c h h f" . helm-flycheck)))

(use-package helm-bibtex ; Manage BibTeX bibliographies with Helm
  :ensure t
  :commands helm-bibtex
  :bind (("C-c h h b" . helm-bibtex)))

(use-package helm-mt ; Manage multi-term with Helm
  :ensure t
  :commands helm-mt
  :bind ("C-c h t" . helm-mt))

(provide 'custom-helm)

;;; custom-helm.el ends here
