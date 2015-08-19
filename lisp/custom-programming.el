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
          (with-eval-after-load 'flycheck '(flycheck-clojure-setup))
          (add-hook 'after-init-hook #'flycheck-mode)))

;;; Emacs Lisp
(use-package ielm ; Emacs Lisp REPL
  :commands ielm
  :config (bind-key "C-c C-q" #'comint-send-eof inferior-emacs-lisp-mode-map))

(use-package elisp-mode ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :config
  (progn
    (defconst custom/use-package-imenu-expression
      `("Use Package" ,(rx "(use-package" (optional "-with-elapsed-timer")
                           symbol-end (1+ (syntax whitespace)) symbol-start
                           (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                           symbol-end) 1)
      "IMenu expression for `use-package' declarations.")

    (defun custom/add-use-package-to-imenu ()
      "Add `use-package' declarations to `imenu'."
      (add-to-list 'imenu-generic-expression
                   custom/use-package-imenu-expression))

    (add-hook 'emacs-lisp-mode-hook
              #'custom/add-use-package-to-imenu)))

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
(use-package scheme ; Configuration for Scheme
  :config
  (progn
    (require 'cmuscheme)

    (bind-key "C-c C-l" #'scheme-load-current-file scheme-mode-map)
    (bind-key "C-c C-f" #'scheme-compile-current-file scheme-mode-map)

    (defun scheme-load-current-file (&optional switch)
      (interactive "P")
      (let ((file-name (buffer-file-name)))
        (comint-check-source file-name)
        (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                             (file-name-nondirectory file-name)))
        (comint-send-string (scheme-proc) (concat "(load \""
                                                  file-name
                                                  "\"\)\n"))
        (if switch
            (switch-to-scheme t)
          (message "\"%s\" loaded." file-name) ) ) )

    (defun scheme-compile-current-file (&optional switch)
      (interactive "P")
      (let ((file-name (buffer-file-name)))
        (comint-check-source file-name)
        (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                             (file-name-nondirectory file-name)))
        (message "compiling \"%s\" ..." file-name)
        (comint-send-string (scheme-proc) (concat "(compile-file \""
                                                  file-name
                                                  "\"\)\n"))
        (if switch
            (switch-to-scheme t)
          (message "\"%s\" compiled and loaded." file-name))))

    (setq auto-insert-alist
          '(("\\.scm" .
             (insert
              "#!/bin/sh\n#| -*- scheme -*-\nexec csi -s $0 \"$@\"\n|#\n"))))))

(use-package geiser ; Collection of modes for Scheme interpreters
  :ensure t
  :commands run-geiser
  :init (setq geiser-active-implementations '(chicken guile)))

;;; Common Lisp
(use-package sly ; Sylvester the Cat's Common Lisp IDE
  :ensure t
  :commands sly
  :init (setq inferior-lisp-program "/usr/bin/sbcl")
  :config
  (progn
    (bind-keys :map sly-mode-map
               ("C-c C-q" . sly-quit-lisp)
               ("M-h"     . sly-documentation-lookup))
    (bind-key "C-c C-k" 'sly-mrepl-clear-recent-output sly-mrepl-mode-map)))

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

;;; Utilities and keybindings
(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, comment that many sexps."
  (interactive "p")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp)
    (dotimes (_ n)
      (comment-sexp--raw))))

(bind-key "C-M-;" 'comment-dwim) ; Use C-M-; instead of M-;

(eval-after-load 'clojure-mode
  '(bind-key "M-;" #'comment-or-uncomment-sexp clojure-mode-map))
(bind-key "M-;" #'comment-or-uncomment-sexp emacs-lisp-mode-map)
(bind-key "M-;" #'comment-or-uncomment-sexp scheme-mode-map)

(bind-key "C-;" #'comment-line)

(bind-key "C-x C-e" 'pp-eval-last-sexp) ; Pretty-print evaluated expression

(provide 'custom-programming)

;;; custom-programming.el ends here
