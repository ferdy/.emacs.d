;;; mu-programming.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for programming utilities.

;;; Code:

;;; Emacs Lisp
(use-package ielm                       ; Emacs Lisp REPL
  :bind ("C-c d i" . ielm)
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

;;; Clojure
(use-package cider                      ; Clojure development environment
  :ensure t
  :defer t
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)

  (validate-setq
   ;; Set up Figwheel in ClojureScript REPL
   cider-cljs-lein-repl
   "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"
   ;; Do not offer to open ClojureScript app in browser
   cider-offer-to-open-cljs-app-in-browser nil))

(use-package cider-mode                 ; CIDER mode for REPL interaction
  :ensure cider
  :defer t
  :bind(:map cider-mode-map
             ("C-c m l" . cider-load-all-files))
  :config
  (require 'cider-client)

  (defun mu-cider-mode-line-info ()
    (if-let ((current-connection (ignore-errors (cider-current-connection))))
        (with-current-buffer current-connection
          (concat
           cider-repl-type
           (format
            ":%s" (or (cider--project-name nrepl-project-dir) "<no project>"))))
      "-"))

  ;; Simplify CIDER mode-line indicator
  (validate-setq
   cider-mode-line '(:eval (format " CIDER[%s]" (mu-cider-mode-line-info)))))

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
  :bind (:map cider-repl-mode-map
              ("C-c C-o" . cider-repl-clear-buffer))
  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)

  (validate-setq
   cider-repl-wrap-history t
   cider-repl-history-size 1000
   cider-repl-history-file (locate-user-emacs-file "cider-repl-history")
   cider-repl-display-help-banner nil
   cider-repl-pop-to-buffer-on-connect nil
   cider-repl-result-prefix ";; => "
   cider-repl-use-pretty-printing t))

(use-package cider-stacktrace           ; Navigate stacktrace
  :ensure cider
  :defer t)

(use-package clj-refactor               ; Refactoring utilities
  :ensure t
  :defer t
  :init
  (defun mu-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)                ; For adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c RET"))

  (add-hook 'clojure-mode-hook #'mu-clojure-mode-hook)
  :config
  (validate-setq cljr-suppress-middleware-warnings t
                 cljr-add-ns-to-blank-clj-files nil
                 cljr-auto-sort-ns t
                 cljr-favor-prefix-notation cljr-favor-private-functions
                 cljr-warn-on-eval nil)
  :diminish clj-refactor-mode)

(use-package clojure-snippets           ; Yasnippets for Clojure
  :ensure t
  :defer t
  :after clojure-mode)

(use-package datomic-snippets           ; Yasnippets for Datomic
  :ensure t
  :defer t
  :after clojure-mode)

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

;;; Common Lisp
(use-package sly                        ; Sylvester the Cat's Common Lisp IDE
  :ensure t
  :bind (("C-c d c" . sly)
         :map sly-mode-map
         ("C-c m q" . sly-quit-lisp)
         ("C-c m h" . sly-documentation-lookup)))

(use-package sly-macrostep              ; Macro-expansion via macrostep.el
  :ensure t
  :defer t)

(use-package sly-quicklisp              ; QUICKLISP support for Sly
  :ensure t
  :defer t)

;;; Rust
(use-package rust-mode                  ; Rust major mode
  :ensure t
  :bind (:map rust-mode-map ("C-c <tab>" . rust-format-buffer)))

(use-package racer                      ; Completion and navigation for Rust
  :ensure t
  :defer t
  :bind (:map racer-mode-map
              ("C-c m h" . racer-describe)
              ("C-c m d" . racer-debug))
  :init (add-hook 'rust-mode-hook #'racer-mode)
  :config
  (validate-setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

(use-package cargo                      ; Control Cargo
  :ensure t
  :bind (:map rust-mode-map
              ("<f6>" . cargo-process-build))
  :init (add-hook 'rust-mode-hook #'cargo-minor-mode)
  :diminish cargo-minor-mode)

(use-package toml-mode                  ; Toml for Cargo files
  :ensure t
  :defer t)

;;; Idris
(use-package idris-mode                 ; Idris editing
  :ensure t
  :mode ("\\.idr\\'" . idris-mode))

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

;;; Databases
(use-package sql                        ; SQL editing and REPL
  :mode ("\\.sql\\'" . sql-mode)
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
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ftl?\\'" . web-mode)))

(use-package js2-mode                   ; Powerful JavaScript mode
  :ensure t
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (unbind-key "M-." js2-mode-map)

  ;; Disable parser errors and strict warnings
  (validate-setq js2-mode-show-parse-errors nil
                 js2-mode-show-strict-warnings nil)

  ;; Try to highlight most ECMA built-ins
  (validate-setq js2-highlight-level 3)

  ;; Better Imenu in j2-mode
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package js2-refactor               ; Refactor JavaScript
  :ensure t
  :after js2-mode
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c m r"))

(use-package xref-js2                 ; Jump to references with Ag in JavaScript
  :ensure t
  :after js2-mode
  :config
  (add-hook
   'js2-mode-hook
   (lambda ()
     (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package rjsx-mode                  ; JSX mode
  :ensure t
  :mode ("\\.jsx\\'" . rjsx-mode))

(use-package css-mode                   ; Better CSS support
  :defer t
  :config (validate-setq css-indent-offset 2))

(use-package css-eldoc                  ; Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package web-beautify               ; Pretty format HTML/CSS/JS files
  :ensure t
  :init
  (with-eval-after-load 'js2-mode
    (bind-key "C-c m f" #'web-beautify-js js2-mode-map))
  (with-eval-after-load 'web-mode
    (bind-key "C-c m f" #'web-beautify-html web-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c m f" #'web-beautify-css css-mode-map)))

(use-package php-mode                   ; Better PHP support
  :ensure t
  :mode "\\.php\\'")

(use-package yaml-mode                  ; Edit YAML files
  :ensure t
  :mode "\\.yaml\\'")

(use-package restclient                 ; Interactive HTTP client
  :ensure t
  :defer t)

;;; Other languages
(use-package sh-script                  ; Shell scripts
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  ;; Use two spaces in shell scripts.
  (validate-setq sh-indentation 2       ; The basic indentation
                 sh-basic-offset 2      ; The offset for nested indentation
                 ))

(use-package nxml-mode                  ; XML editing
  :mode "\\.xml\\'"
  :bind (:map nxml-mode-map
              ("C-c m f" . mu-xml-format))
  :config
  ;; Complete closing tags, and insert XML declarations into empty files
  (validate-setq nxml-slash-auto-complete-flag t
                 nxml-auto-insert-xml-declaration-flag t
                 ;; Treat elements (with children) as sexps
                 nxml-sexp-element-flag t)

  (defun mu-xml-format ()
    "Format an XML buffer with `xmllint'."
    (interactive)
    (shell-command-on-region (point-min) (point-max)
                             "xmllint -format -"
                             (current-buffer) t
                             "*Xmllint Error Buffer*" t)))

(use-package json-mode                  ; JSON editing
  :ensure t
  :mode "\\.json\\'")

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
  :defer t
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
            #'mu-colorize-compilation-buffer))

;;; Utilities and keybindings
(bind-key* "C-;" #'comment-line)

(bind-key "C-c t d" #'toggle-debug-on-error)

(provide 'mu-programming)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-programming.el ends here
