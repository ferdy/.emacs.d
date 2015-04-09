;;; custom-programming.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for programming utilities.

;;; Code:

(use-package eldoc ; Documentation in the echo area
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package compile
  :config (progn
            (setq compilation-ask-about-save nil
                  compilation-always-kill t
                  compilation-scroll-output 'first-error)))

;;; Syntax checking
;; Requires: chktex
(use-package flycheck
  :ensure t
  :defer 5
  :init (global-flycheck-mode)
  :config
  (progn
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-display-errors-function
          #'flycheck-pos-tip-error-messages)

    ;; Use italic face for checker name
    (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic)))

(use-package flycheck-pos-tip ; Tooltip at point for flycheck messages
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck
          (setq flycheck-display-errors-function
                #'flycheck-pos-tip-error-messages)))

(use-package flycheck-package ; Flycheck for Emacs package development
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

;;; Clojure
;; Requires: openjdk-7-jre, openjdk-7-jre, lein
(use-package cider
  :ensure t
  :defer t
  :config (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(use-package clojure-mode
  :ensure t
  :defer t
  :init (progn (add-hook 'clojure-mode-hook #'cider-mode)
               (add-hook 'clojure-mode-hook #'subword-mode)))

(use-package clojure-mode-extra-font-locking
  :ensure clojure-mode
  :defer t
  :init (with-eval-after-load 'clojure-mode
          (require 'clojure-mode-extra-font-locking)))

(use-package nrepl-client
  :ensure cider
  :defer t
  :config (setq nrepl-hide-special-buffers t))

(use-package cider-repl
  :ensure cider
  :defer t
  :config
  (progn
    ;; Increase the history size and make it permanent
    (setq cider-repl-history-size 1000
          cider-repl-history-file (locate-user-emacs-file "cider-repl-history")
          cider-repl-pop-to-buffer-on-connect nil)))

;;; Haskell
;; Requires:
;; - cabal install pretty happy hasktags haskell-docs hoogle
;; - M-x package-install hindent
;;
;; Additionally, to be installed from source:
;; - https://github.com/chrisdone/ghci-ng
(use-package haskell-mode
  :ensure t
  :disabled t
  :config
  (progn
    (add-hook 'haskell-mode-hook #'subword-mode) ; Subword navigation
    (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; Scan and navigate
                                        ; declarations
    ;; Insert module templates into new buffers
    (add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)
    ;; Automatically run hasktags
    (setq haskell-tags-on-save t
          ;; Suggest adding/removing imports as by GHC warnings and Hoggle/GHCI
          ;; loaded modules respectively
          haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-use-presentation-mode t ; Don't clutter the echo area
          haskell-process-show-debug-tips nil ; Disable tips
          haskell-process-log t ; Log debugging information
          ;; Suggest imports automatically with Hayoo. Hayoo is slower because
          ;; it's networked, but it covers all of hackage, which is really an
          ;; advantage.
          haskell-process-suggest-hoogle-imports nil
          haskell-process-suggest-hayoo-imports t
          ;; Use GHCI NG from https://github.com/chrisdone/ghci-ng
          haskell-process-path-ghci "ghci-ng")
    (add-to-list 'haskell-process-args-cabal-repl "--with-ghc=ghci-ng")
    (bind-key "C-c h d" #'haskell-describe haskell-mode-map)
    (bind-key "C-c h h" #'haskell-hayoo haskell-mode-map)
    (bind-key "C-c h H" #'haskell-hoogle haskell-mode-map)
    (bind-key "C-c u i" #'haskell-navigate-imports haskell-mode-map)
    (bind-key "C-c f c" #'haskell-cabal-visit-file haskell-mode-map)))

(use-package haskell
  :ensure haskell-mode
  :disabled t
  :init (dolist (hook '(haskell-mode-hook haskell-cabal-mode-hook))
          (add-hook hook #'interactive-haskell-mode))
  :config
  (progn
    (bind-key "C-c C-t" #'haskell-mode-show-type-at
              interactive-haskell-mode-map)
    (bind-key "M-." #'haskell-mode-goto-loc
              interactive-haskell-mode-map)
    (bind-key "C-c u u" #'haskell-mode-find-uses
              interactive-haskell-mode-map)))

(use-package haskell-interactive-mode
  :ensure haskell-mode
  :disabled t
  :config (add-hook 'haskell-interactive-mode-hook #'subword-mode))

(use-package haskell-simple-indent ; Primitive Haskell indentation
  :ensure haskell-mode
  :disabled t
  :init (add-hook 'haskell-mode-hook #'haskell-simple-indent-mode))

(use-package hindent ; Automated Haskell indentation
  :ensure t
  :disabled t
  :init (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package flycheck-haskell ; Setup Flycheck from Cabal projects
  :ensure t
  :disabled t
  :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;; Scheme
;; Requires: guile-2.0
(use-package geiser
  :ensure t
  :disabled t
  :init (progn (setq scheme-program-name "guile")
               (setq geiser-impl-installed-implementations '(guile))))

;;; Common Lisp
(use-package slime
  :ensure t
  :disabled t
  :init (setq inferior-lisp-program "/usr/bin/sbcl")
  :config (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :disabled t
  :init (slime-setup '(slime-company)))

;;; Web development
(use-package web-mode
  :ensure t
  :defer t
  :config (setq web-mode-markup-indent-offset 2))

(use-package js2-mode ; Better JavaScript support
  :ensure t
  :mode "\\.js\\(?:on\\)?\\'"
  :config (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package css-mode
  :defer t
  :mode "\\.css\\'"
  :config (add-hook 'css-mode-hook
                    (lambda () (run-hooks 'prog-mode-hook))))

(use-package css-eldoc
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package php-mode
  :ensure t
  :defer t
  :mode "\\.php\\'")

(provide 'custom-programming)

;;; custom-programming.el ends here
