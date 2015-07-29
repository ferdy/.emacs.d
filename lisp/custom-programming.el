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
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :config (setq-default eldoc-documentation-function #'describe-char-eldoc)
  :diminish eldoc-mode)

(use-package macrostep ; Navigate through macros
  :ensure t
  :init (with-eval-after-load 'lisp-mode
          (bind-key "C-c e e" #'macrostep-expand emacs-lisp-mode-map)
          (bind-key "C-c e e" #'macrostep-expand lisp-interaction-mode-map)))

(use-package compile ; Compile from Emacs
  :config (progn
            (setq compilation-ask-about-save nil
                  ;; Kill old compilation processes before starting new ones
                  compilation-always-kill t
                  ;; Automatically scroll and jump to the first error
                  compilation-scroll-output 'first-error
                  compilation-auto-jump-to-first-error t
                  ;; Skip over warnings and info messages in compilation
                  compilation-skip-threshold 2
                  ;; Don't freeze when process reads from stdin
                  compilation-disable-input t
                  ;; Show three lines of context around the current message
                  compilation-context-lines 3)

            (add-to-list 'display-buffer-alist
                         `(,(rx bos "*compilation")
                           (display-buffer-reuse-window
                            display-buffer-in-side-window)
                           (side            . bottom)
                           (reusable-frames . visible)
                           (window-height   . 0.4)))

            (defun custom/colorize-compilation-buffer ()
              "Colorize a compilation mode buffer."
              (interactive)
              (when (eq major-mode 'compilation-mode)
                (let ((inhibit-read-only t))
                  (ansi-color-apply-on-region (point-min) (point-max)))))

            (add-hook 'compilation-filter-hook
                      #'custom/colorize-compilation-buffer)))

;;; Syntax checking
;; Requires: chktex
(use-package flycheck ; On-the-fly syntax checker
  :ensure t
  :bind ("C-c f" . flycheck-mode)
  :config (progn
            ;; Use italic face for checker name
            (set-face-attribute 'flycheck-error-list-checker-name nil
                                :inherit 'italic)

            (add-to-list 'display-buffer-alist
                         `(,(rx bos "*Flycheck errors*" eos)
                           (display-buffer-reuse-window
                            display-buffer-in-side-window)
                           (side . bottom)
                           (reusable-frames . visible)
                           (window-height . 0.4))))
  :diminish flycheck-mode)

(use-package flycheck-package ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package flycheck-clojure ; Backend for Clojure
  :ensure t
  :defer t
  :init (progn
          (eval-after-load 'flycheck '(flycheck-clojure-setup))
          (add-hook 'after-init-hook #'flycheck-mode)))

;;; Clojure
(use-package cider ; Clojure development environment
  :ensure t
  :defer t
  :config (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  :diminish cider-mode)

(use-package clojure-mode ; Major mode for Clojure files
  :ensure t
  :defer t
  :init (progn (add-hook 'clojure-mode-hook #'cider-mode)
               (add-hook 'clojure-mode-hook #'subword-mode)))

(use-package clojure-mode-extra-font-locking ; Font-locking for Clojure mode
  :ensure t
  :defer t
  :init (with-eval-after-load 'clojure-mode
          (require 'clojure-mode-extra-font-locking)))

(use-package nrepl-client ; Client for Clojure nREPL
  :ensure cider
  :defer t
  :config (setq nrepl-hide-special-buffers t))

(use-package cider-repl ; REPL interactions with CIDER
  :ensure cider
  :defer t
  :config (progn
            ;; Increase the history size and make it permanent
            (setq cider-repl-history-size 1000
                  cider-repl-history-file
                  (locate-user-emacs-file "cider-repl-history")
                  cider-repl-pop-to-buffer-on-connect nil)))

(use-package clj-refactor ; Refactoring utilities
  :ensure t
  :defer t
  :init (progn
          (defun custom/clojure-mode-hook ()
            (clj-refactor-mode 1)
            (yas-minor-mode 1) ; For adding require/use/import
            (cljr-add-keybindings-with-prefix "C-c C-m"))

          (add-hook 'clojure-mode-hook #'custom/clojure-mode-hook))
  :config (setq cljr-suppress-middleware-warnings t)
  :diminish clj-refactor-mode)

(use-package clojure-cheatsheet ; Explore Clojure Cheatsheet from Emacs
  :ensure t
  :commands clojure-cheatsheet)

;;; Scheme
(use-package geiser ; Collection of modes for Scheme interpreters
  :ensure t
  :commands run-geiser
  :init (setq geiser-active-implementations '(chicken guile)))

;;; Web development
(use-package web-mode ; Major mode for editing web templates
  :ensure t
  :mode "\\.html\\'"
  :config (setq web-mode-markup-indent-offset 2))

(use-package js2-mode ; Better JavaScript support
  :ensure t
  :mode "\\.js\\'"
  :config (progn
            (setq-default js2-basic-offset 2)
            (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode)))

(use-package css-mode ; Better CSS support
  :defer t
  :mode "\\.css\\'"
  :config (add-hook 'css-mode-hook
                    (lambda () (run-hooks 'prog-mode-hook))))

(use-package css-eldoc ; Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package php-mode ; Better PHP support
  :ensure t
  :defer t
  :mode "\\.php\\'")

(provide 'custom-programming)

;;; custom-programming.el ends here
