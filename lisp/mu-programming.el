;;; mu-programming.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for programming utilities.

;;; Code:

;;; Emacs Lisp
(use-package ielm                       ; Emacs Lisp REPL
  :bind ("C-c a z" . ielm)
  :config (bind-key "C-c C-q" #'comint-send-eof inferior-emacs-lisp-mode-map))

(use-package elisp-mode                 ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c m e r" . eval-region)
              ("C-c m e b" . eval-buffer)
              ("C-c m e e" . eval-last-sexp)
              ("C-c m e f" . eval-defun))
  :config
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

(use-package elisp-slime-nav            ; Navigate elisp code with M-. & M-,
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :diminish elisp-slime-nav-mode)

(use-package el-search                  ; pcase-based search for elisp
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c m s" . el-search-pattern)
              ("C-c m r" . el-search-query-replace)))

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

(use-package cask-mode                  ; Major mode for Cask files
  :defer t
  :ensure t)

(use-package suggest                    ; Suggest Elisp functions
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c m o" . suggest)))

(use-package ipretty                    ; Elisp pretty-printing
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c m p" . ipretty-last-sexp)))

;;; Clojure
(use-package cider                      ; Clojure development environment
  :ensure t
  :defer t
  :config (add-hook 'cider-mode-hook 'eldoc-mode))

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
  :config (validate-setq nrepl-hide-special-buffers t))

(use-package cider-repl                 ; REPL interactions with CIDER
  :ensure cider
  :defer t
  :config
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)

  ;; Set up Figwheel in ClojureScript REPL
  (validate-setq
   cider-cljs-lein-repl
   "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

  ;; Increase the history size and make it permanent
  (validate-setq cider-repl-history-size 1000
                 cider-repl-history-file
                 (locate-user-emacs-file "cider-repl-history")
                 cider-repl-display-help-banner nil ; Disable help banner
                 cider-repl-pop-to-buffer-on-connect nil
                 cider-repl-result-prefix ";; => "))

(use-package cider-stacktrace           ; Navigate stacktrace
  :ensure cider
  :defer t
  :config (validate-setq cider-stacktrace-fill-column t))

(use-package clj-refactor               ; Refactoring utilities
  :ensure t
  :defer t
  :init
  (defun mu-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)                ; For adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))

  (add-hook 'clojure-mode-hook #'mu-clojure-mode-hook)
  :config
  (validate-setq cljr-suppress-middleware-warnings t
                 cljr-auto-sort-ns t
                 cljr-favor-prefix-notation
                 cljr-favor-private-functions)
  :diminish clj-refactor-mode)

;;; Scheme
(use-package scheme                     ; Configuration for Scheme
  :defer t
  :config
  (require 'cmuscheme)

  ;; Use CHICKEN Scheme
  (validate-setq scheme-program-name "csi")
  (add-to-list 'interpreter-mode-alist '("chicken-scheme" . scheme-mode))

  ;; Add custom header to .scm files
  (validate-setq
   auto-insert-alist
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
      (validate-setq
       scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
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
      (validate-setq
       scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
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
  :bind ("C-c d g" . run-geiser))

(use-package sicp                       ; The Wizard Book in Info format
  :ensure t
  :defer t)

;;; Common Lisp
(use-package sly                        ; Sylvester the Cat's Common Lisp IDE
  :ensure t
  :bind ("C-c d c" . sly)
  :bind (:map sly-mode-map
              ("C-c m q" . sly-quit-lisp)
              ("C-c m h" . sly-documentation-lookup)))

(use-package sly-macrostep              ; Macro-expansion via macrostep.el
  :ensure t
  :defer t)

(use-package sly-quicklisp              ; QUICKLISP support for Sly
  :ensure t
  :defer t)

;;; Haskell
(use-package haskell-mode               ; Haskell major mode
  :ensure t
  :defer t
  :bind (:map haskell-mode-map
              ("M-." . haskell-mode-jump-to-def-or-tag)
              ("C-c m i j" . haskell-navigate-imports)
              ("C-c m i s" . haskell-sort-imports)
              ("C-c m i a" . haskell-align-imports)
              ;; Recommended Haskell Mode bindings, see
              ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html
              )
  :config
  (validate-setq haskell-tags-on-save t ; Regenerate TAGS on save
                 haskell-process-log t  ; Show log for GHCI process
                 ;; Remove unused imports and auto-import modules
                 haskell-process-suggest-remove-import-lines t
                 haskell-process-auto-import-loaded-modules t)

  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; IMenu support
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package haskell                    ; Interactive Haskell
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-mode-map
              ("C-c C-l" . haskell-process-load-file)
              ("C-`" . haskell-interactive-bring)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("C-c c" . haskell-process-cabal)
              :map interactive-haskell-mode-map
              ("C-c m t" . haskell-mode-show-type-at))
  :init (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package haskell-compile            ; Haskell compilation
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-mode-map
              ("C-c m c" . haskell-compile)
              ("<f5>" . haskell-compile))
  :config
  ;; Build with Stack
  (validate-setq haskell-compile-cabal-build-command "stack build"))

(use-package cabal-mode                 ; Cabal files
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-cabal-mode-map
              ("C-`" . haskell-interactive-bring)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c c" . haskell-process-cabal)))

(use-package hindent                    ; Haskell indentation
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :config
  (validate-setq hindent-style "gibiansky"))

;;; Python
(use-package python                     ; Python editing
  :defer t
  :config
  ;; PEP 8 compliant filling rules, 79 chars maximum
  (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))
  (add-hook 'python-mode-hook #'subword-mode)

  (let ((ipython (executable-find "ipython")))
    (if ipython
        (setq python-shell-interpreter ipython)
      (warn "IPython is missing, falling back to default python"))))

(use-package anaconda-mode              ; Powerful Python backend for Emacs
  :ensure t
  :defer t
  :after python
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package pip-requirements           ; requirements.txt files
  :ensure t
  :defer t)

;;; Rust
(use-package rust-mode                  ; Rust major mode
  :ensure t
  :bind (:map rust-mode-map ("C-c <tab>" . rust-format-buffer)))

(use-package racer                      ; Completion and navigation for Rust
  :ensure t
  :defer t
  :init (add-hook 'rust-mode-hook #'racer-mode)
  :config
  (validate-setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

(use-package cargo                      ; Control Cargo
  :ensure t
  :bind (:map rust-mode-map ("<f6>" . cargo-process-build))
  :init (add-hook 'rust-mode-hook #'cargo-minor-mode)
  :diminish cargo-minor-mode)

(use-package toml-mode                  ; Toml for Cargo files
  :ensure t
  :defer t)

;;; Databases
(use-package sql                        ; SQL editing and REPL
  :bind (("C-c d s" . sql-connect)
         :map sql-mode-map
         ("C-c m p" . sql-set-product)))

(use-package sqlup-mode                 ; Upcase SQL keywords
  :ensure t
  :bind (:map sql-mode-map
              ("C-c m u" . sqlup-capitalize-keywords-in-region))
  :config (add-hook 'sql-mode-hook #'sqlup-mode))

;;; Web development
(use-package web-mode                   ; Major mode for editing web templates
  :ensure t
  :defer t
  :mode "\\.html\\'")

(use-package js2-mode                   ; Powerful JavaScript mode
  :ensure t
  :defer t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (unbind-key "M-." js2-mode-map)

  ;; Disable parser errors and strict warnings
  (validate-setq js2-mode-show-parse-errors nil
                 js2-mode-show-strict-warnings nil)

  ;; Try to highlight most ECMA built-ins
  (validate-setq js2-highlight-level 3))

(use-package js2-refactor               ; Refactor JavaScript
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c m r"))

(use-package xref-js2                   ; Navigate JS with ag & js2-mode's AST
  :ensure t
  :after js2-mode
  :init
  (defun add-xref-js2-backend ()
    (add-hook 'xref-backend-functions
              #'xref-js2-xref-backend nil t))

  (add-hook 'js2-mode-hook #'add-xref-js2-backend))

(use-package purescript-mode            ; Purescript editing
  :ensure t
  :defer t)

(use-package psc-ide                    ; IDE for Purescript
  :ensure t
  :defer t
  :init
  (add-hook 'purescript-mode-hook #'psc-ide-mode)
  (add-hook 'purescript-mode-hook #'turn-on-purescript-indentation))

(use-package css-mode                   ; Better CSS support
  :defer t
  :config (validate-setq css-indent-offset 2))

(use-package css-eldoc                  ; Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package php-mode                   ; Better PHP support
  :ensure t
  :mode "\\.php\\'")

;;; Other languages
(use-package sh-script                  ; Shell scripts
  :defer t
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (validate-setq sh-indentation 2       ; The basic indentation
                 sh-basic-offset 2      ; The offset for nested indentation
                 ))

(use-package nxml-mode                  ; XML editing
  :mode "\\.xml\\'"
  :config
  ;; Complete closing tags, and insert XML declarations into empty files
  (validate-setq nxml-slash-auto-complete-flag t
                 nxml-auto-insert-xml-declaration-flag t))

(use-package json-mode                  ; JSON editing
  :mode "\\.json\\'"
  :ensure t)

;;; Bugs management
(use-package bug-reference              ; Buttonize bug references
  :defer t
  :init
  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (add-hook 'text-mode-hook #'bug-reference-mode))

(use-package bug-hunter                 ; Find bugs in Emacs configuration
  :ensure t
  :commands bug-hunter-file)

;;; Misc utilities
(use-package eldoc                      ; Documentation in the echo area
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  (validate-setq eldoc-idle-delay 0.1)  ; Show eldoc more promptly
  :diminish eldoc-mode)

(use-package etags                      ; Tag navigation
  :defer t
  :config
  ;; Do not query before reverting TAGS tables
  (validate-setq tags-revert-without-query t))

(use-package macrostep                  ; Navigate through macros
  :ensure t
  :after lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c m m e" . macrostep-expand))
  :bind (:map lisp-interaction-mode-map
              ("C-c m m e" . macrostep-expand)))

(use-package compile                    ; Compile from Emacs
  :bind (("C-c c C" . compile)
         ("C-c c r" . recompile))
  :config
  (validate-setq compilation-ask-about-save nil
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
            #'mu-colorize-compilation-buffer)

  (defun mu-send-input (input &optional nl)
    "Send INPUT to the current process.

Interactively also sends a terminating newline."
    (interactive "MInput: \nd")
    (process-send-string
     (get-buffer-process (current-buffer))
     (concat input (if nl "\n"))))

  (defun mu-send-self ()
    "Send the pressed key to the current process."
    (interactive)
    (mu-send-input
     (apply #'string
            (append (this-command-keys-vector) nil))))

  (bind-key "C-c i" #'mu-send-input compilation-mode-map)

  (dolist (key '("\C-d" "\C-j" "y" "n"))
    (bind-key key #'mu-send-self compilation-mode-map)))

;;; Utilities and keybindings
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
