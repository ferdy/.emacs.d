;;; mu-programming.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for programming utilities.

;;; Code:

;;; Syntax checking
;; Requires: chktex
(use-package flycheck                   ; On-the-fly syntax checker
  :ensure t
  :bind (("C-c e l" . list-flycheck-errors)
         ("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error)
         ("C-c e c" . flycheck-buffer)
         ("C-c e C" . flycheck-clear)
         ("C-c e f" . flycheck-first-error)
         ("C-c e w" . flycheck-copy-errors-as-kill)
         ("C-c t e" . flycheck-mode))
  :config
  (setq flycheck-emacs-lisp-load-path nil
        flycheck-standard-error-navigation nil
        flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)

  ;; Use italic face for checker name
  (set-face-attribute 'flycheck-error-list-checker-name nil
                      :inherit 'italic))

(use-package flycheck-package          ; Check package conventions with Flycheck
  :ensure t
  :defer t
  :after flycheck
  :init (flycheck-package-setup))

(use-package flycheck-cask              ; Setup Flycheck by Cask projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

(use-package flycheck-clojure           ; Backend for Clojure
  :ensure t
  :defer t
  :after flycheck
  :init (add-hook 'after-init-hook #'flycheck-mode))

;;; Emacs Lisp
(use-package ielm                       ; Emacs Lisp REPL
  :bind ("C-c a z" . ielm)
  :config (bind-key "C-c C-q" #'comint-send-eof inferior-emacs-lisp-mode-map))

(use-package elisp-mode                 ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :config
  (bind-key "C-c m e r" #'eval-region emacs-lisp-mode-map)
  (bind-key "C-c m e b" #'eval-buffer emacs-lisp-mode-map)
  (bind-key "C-c m e e" #'eval-last-sexp emacs-lisp-mode-map)
  (bind-key "C-c m e f" #'eval-defun emacs-lisp-mode-map)

  (defconst mu-use-package-imenu-expression
    `("Use Package" ,(rx "(use-package" (optional "-with-elapsed-timer")
                         symbol-end (1+ (syntax whitespace)) symbol-start
                         (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                         symbol-end) 1)
    "IMenu expression for `use-package' declarations.")

  (defun mu-add-use-package-to-imenu ()
    "Add `use-package' declarations to `imenu'."
    (add-to-list 'imenu-generic-expression
                 mu-use-package-imenu-expression))

  (add-hook 'emacs-lisp-mode-hook
            #'mu-add-use-package-to-imenu))

(use-package ert                        ; Elisp Regression Test
  :defer t
  :after elisp-mode)

(use-package buttercup                  ; Behavior-Driven elisp testing
  :ensure t
  :defer t
  :init
  (defun mu-is-buttercup-buffer ()
    (and (buffer-file-name)
         (string-match-p (rx "/test-" (1+ (not (any "/"))) ".el" eos)
                         (buffer-file-name))))

  ;; Load buttercup automatically for proper indentation in specs
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (when (mu-is-buttercup-buffer)
                (require 'buttercup)))))

;;; Clojure
(use-package cider                      ; Clojure development environment
  :ensure t
  :defer t
  :config (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  :diminish cider-mode)

(use-package clojure-mode               ; Major mode for Clojure files
  :ensure t
  :defer t
  :init
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package clojure-mode-extra-font-locking ; Font-locking for Clojure mode
  :ensure t
  :defer t
  :after clojure-mode)

(use-package nrepl-client               ; Client for Clojure nREPL
  :ensure cider
  :defer t
  :config (setq nrepl-hide-special-buffers t))

(use-package cider-repl                 ; REPL interactions with CIDER
  :ensure cider
  :defer t
  :config
  ;; Run cljs-repl inside Emacs
  (defun cider-figwheel-repl ()
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!)
             (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))   ;; Run cljs-repl inside Emacs


  (bind-key "C-c m f" #'cider-figwheel-repl)

  ;; Increase the history size and make it permanent
  (setq cider-repl-history-size 1000
        cider-repl-history-file
        (locate-user-emacs-file "cider-repl-history")
        cider-repl-display-help-banner nil ; Disable help banner
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-result-prefix ";; => "))

(use-package cider-stacktrace           ; Navigate stacktrace
  :ensure cider
  :defer t
  :config (setq cider-stacktrace-fill-column t))

(use-package clj-refactor               ; Refactoring utilities
  :ensure t
  :defer t
  :init
  (defun mu-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)                ; For adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c m r"))

  (add-hook 'clojure-mode-hook #'mu-clojure-mode-hook)
  :config
  (setq cljr-suppress-middleware-warnings t
        cljr-auto-sort-ns t
        cljr-favor-prefix-notation
        cljr-favor-private-functions)
  :diminish clj-refactor-mode)

(use-package yesql-ghosts               ; Display ghostly yesql queries inline
  :ensure t
  :commands yesql-ghosts-display-query-ghosts
  ;; Do not show defqueries by default
  :config (setq yesql-ghosts-show-ghosts-automatically nil))

;;; Scheme
(use-package scheme                     ; Configuration for Scheme
  :defer t
  :config
  (require 'cmuscheme)

  ;; Use CHICKEN Scheme
  (setq scheme-program-name "csi")
  (add-to-list 'interpreter-mode-alist '("chicken-scheme" . scheme-mode))

  ;; Add custom header to .scm files
  (setq auto-insert-alist
        '(("\\.scm" .
           (insert
            "#!/bin/sh\n#| -*- scheme -*-\nexec csi -s $0 \"$@\"\n|#\n"))))

  (with-eval-after-load 'scheme
    (bind-key "C-c m s" #'run-scheme scheme-mode-map)
    (bind-key "C-c m l" #'scheme-load-current-file scheme-mode-map)
    (bind-key "C-c m f" #'scheme-compile-current-file scheme-mode-map))

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
        (message "\"%s\" compiled and loaded." file-name)))))

(use-package geiser                ; Collection of modes for Scheme interpreters
  :ensure t
  :bind ("C-c d g" . run-geiser)
  :init (setq geiser-active-implementations '(chicken guile)))

(use-package sicp                       ; The Wizard Book in Info format
  :ensure t
  :defer t)

;;; Common Lisp
(use-package sly                        ; Sylvester the Cat's Common Lisp IDE
  :ensure t
  :bind ("C-c d c" . sly)
  :init (setq inferior-lisp-program "/usr/bin/sbcl")
  :config
  (bind-keys :map sly-mode-map
             ("C-c m q" . sly-quit-lisp)
             ("C-c m h" . sly-documentation-lookup)))

(use-package sly-macrostep              ; Macro-expansion via macrostep.el
  :ensure t
  :defer t)

(use-package sly-quicklisp              ; QUICKLISP support for Sly
  :ensure t
  :defer t)

;;; Databases
(use-package sql                        ; SQL editing and REPL
  :commands sql-connect
  :config (add-hook 'sql-interactive-mode-hook #'toggle-truncate-lines))

;;; Web development
(use-package web-mode                   ; Major mode for editing web templates
  :ensure t
  :mode "\\.html\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package js2-mode                   ; Better JavaScript support
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq-default js2-basic-offset 2)
  (add-hook 'js2-mode-hook #'js2-highlight-unused-variables-mode))

(use-package css-mode                   ; Better CSS support
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2)
  (add-hook 'css-mode-hook
            (lambda () (run-hooks 'prog-mode-hook))))

(use-package css-eldoc                  ; Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package php-mode                   ; Better PHP support
  :ensure t
  :mode "\\.php\\'")

(use-package zencoding-mode     ; Unfold CSS-selector-like expressions to markup
  :ensure t
  :defer t
  :init (add-hook 'web-mode-hook #'zencoding-mode)
  :diminish zencoding-mode)

(use-package nxml-mode                  ; XML editing
  :mode "\\.xml\\'"
  :config
  ;; Complete closing tags, and insert XML declarations into empty files
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))

;;; Bugs management
(use-package bug-reference              ; Buttonize bug references
  :defer t
  :init
  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (add-hook 'text-mode-hook #'bug-reference-mode))

(use-package bug-hunter                 ; Find bugs in Emacs configuration
  :ensure t
  :commands bug-hunter-file)

;;; Utilities and keybindings
(use-package eldoc                      ; Documentation in the echo area
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :config (setq-default eldoc-documentation-function #'describe-char-eldoc)
  :diminish eldoc-mode)

(use-package macrostep                  ; Navigate through macros
  :ensure t
  :defer t
  :after lisp-mode
  :init
  (bind-key "C-c m m e" #'macrostep-expand emacs-lisp-mode-map)
  (bind-key "C-c m m e" #'macrostep-expand lisp-interaction-mode-map))

(use-package compile                    ; Compile from Emacs
  :bind (("C-c c C" . compile)
         ("C-c c r" . recompile))
  :config
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

  (defun mu-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))

  (add-hook 'compilation-filter-hook
            #'mu-colorize-compilation-buffer))

(use-package restclient                 ; ReST REPL for Emacs
  :ensure t
  :defer t)

;;;###autoload
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

;;;###autoload
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

(bind-key* "C-M-;" #'comment-dwim)      ; Use C-M-; instead of M-;
(bind-key "C-c c d" #'comment-dwim)
(bind-key "M-;" #'comment-or-uncomment-sexp emacs-lisp-mode-map)
(bind-key "C-c c u" #'comment-or-uncomment-sexp)

(with-eval-after-load 'clojure-mode
  (bind-key "M-;" #'comment-or-uncomment-sexp clojure-mode-map))
(with-eval-after-load 'scheme
  (bind-key "M-;" #'comment-or-uncomment-sexp scheme-mode-map))

(bind-key* "C-;" #'comment-line)

(bind-key "C-c t d" #'toggle-debug-on-error)

(provide 'mu-programming)

;;; mu-programming.el ends here
