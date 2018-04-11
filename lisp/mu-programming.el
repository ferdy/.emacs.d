;;; mu-programming.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for programming utilities.

;;; Code:

;;; Haskell
(use-package intero                     ; Development mode for Haskell
  :ensure t
  :config
  (intero-global-mode)

  (bind-keys :map intero-mode-map
             ("C-c C-q" . intero-destroy)
             ("C-c m r" . intero-restart))

  (bind-keys :map intero-repl-mode-map
             ("C-c C-q" . intero-destroy)
             ("C-c m r" . intero-restart))

  ;; This binding is for mu-counsel-search-project
  (unbind-key "M-?" intero-mode-map)

  (with-eval-after-load 'flycheck-mode
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))))

(use-package haskell-mode               ; Haskell editing
  :ensure intero
  :mode ("\\.ghci\\'" . haskell-mode)
  :hook ((haskell-mode . eldoc-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-auto-insert-module-template))
  :config
  (with-eval-after-load 'haskell-mode
    (bind-key "C-c m h" #'hoogle haskell-mode-map)))

(use-package hindent                    ; Use hindent to indent Haskell code
  :ensure t
  :hook (haskell-mode . hindent-mode)
  :config
  ;; Suppress errors when hindent--before-save fails
  (with-eval-after-load 'hindent
    (when (require 'nadvice)
      (defun mu-hindent--before-save-wrapper (oldfun &rest args)
        (with-demoted-errors "Error invoking hindent: %s"
          (let ((debug-on-error nil))
            (apply oldfun args))))
      (advice-add
       'hindent--before-save :around 'mu-hindent--before-save-wrapper))))

;;; Clojure
(use-package cider                      ; Clojure development environment
  :ensure t
  :hook (cider-mode . eldoc-mode)
  :config (validate-setq cider-offer-to-open-cljs-app-in-browser nil))

(use-package cider-mode                 ; CIDER mode for REPL interaction
  :ensure cider
  :defer t
  :bind (:map cider-mode-map
              ("C-c m l" . cider-load-all-project-ns))
  :config
  (require 'cider-client)

  (defun mu-cider-mode-line-info ()
    "Simplify CIDER mode-line indicator."
    (if-let* ((current-connection (ignore-errors (cider-current-connection))))
        (with-current-buffer current-connection
          (concat
           cider-repl-type
           (format
            ":%s" (or (cider--project-name nrepl-project-dir) "<no project>"))))
      "-"))

  (validate-setq
   cider-mode-line '(:eval (format " CIDER[%s]" (mu-cider-mode-line-info)))))

(use-package clojure-mode               ; Major mode for Clojure files
  :ensure t
  :mode (("\\.boot$" . clojure-mode)
         ("\\.clj$"  . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.edn$"  . clojure-mode))
  :hook ((clojure-mode . cider-mode)
         (clojure-mode . subword-mode))
  :config
  ;; Fix indentation of some common macros
  (define-clojure-indent
    (for-all 1)
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (reporting 1)))

(use-package clojure-mode-extra-font-locking ; Extra font-locking for Clojure
  :ensure t)

(use-package nrepl-client               ; Client for Clojure nREPL
  :ensure cider
  :defer t
  :config (validate-setq nrepl-hide-special-buffers t))

(use-package cider-repl                 ; REPL interactions with CIDER
  :ensure cider
  :bind (:map cider-repl-mode-map
              ("C-c C-o" . cider-repl-clear-buffer)
              ("C-c t p" . cider-toggle-pretty-printing))
  :hook ((cider-repl-mode . company-mode)
         (cider-repl-mode . eldoc-mode)
         (cider-repl-mode . subword-mode))
  :config
  (validate-setq
   cider-repl-wrap-history t
   cider-repl-history-size 1000
   cider-repl-history-file (locate-user-emacs-file "cider-repl-history")
   cider-repl-display-help-banner nil
   cider-repl-result-prefix ";; => "
   cider-repl-use-pretty-printing t))

(use-package cider-stacktrace           ; Navigate stacktrace
  :ensure cider
  :defer t)

(use-package cider-util                 ; Common utilities
  :ensure cider
  :config
  ;; Set Clojure and Java sources for better stacktrace navigation
  (setq cider-jdk-src-paths '("~/sources/clojure/clojure-1.8.0-sources"
                              "~/sources/clojure/clojure-1.9.0-sources"
                              "~/sources/java/openjdk-8-src")))

(use-package clj-refactor               ; Refactoring utilities
  :ensure t
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (yas-minor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c RET")))
  :config
  (validate-setq
   cljr-suppress-middleware-warnings t
   cljr-add-ns-to-blank-clj-files t
   cljr-auto-sort-ns t
   cljr-favor-prefix-notation nil
   cljr-favor-private-functions nil
   cljr-warn-on-eval nil)

  (validate-setq
   cljr-clojure-test-declaration "[clojure.test :refer :all]"
   cljr-cljs-clojure-test-declaration
   "[cljs.test :refer-macros [deftest is use-fixtures]]")

  (advice-add 'cljr-add-require-to-ns :after
              (lambda (&rest _)
                (yas-next-field)
                (yas-next-field))))

;;; Emacs Lisp
(use-package ielm                       ; Emacs Lisp REPL
  :bind ("C-c d i" . ielm)
  :config (bind-key "C-c C-q" #'comint-send-eof inferior-emacs-lisp-mode-map))

(use-package elisp-mode                 ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-k" . eval-buffer)
              ("C-c m e b" . eval-buffer)
              ("C-c m e f" . eval-defun)
              ("C-c m e e" . eval-last-sexp)
              ("C-c m e r" . eval-region))
  :config
  (defconst mu-use-package-imenu-expression
    `("Use Package" ,(rx "(use-package" (optional "-with-elapsed-timer")
                         symbol-end (1+ (syntax whitespace)) symbol-start
                         (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                         symbol-end) 1)
    "IMenu expression for `use-package' declarations.")

  (defun mu-add-use-package-to-imenu ()
    "Add `use-package' declarations to `imenu'."
    (add-to-list 'imenu-generic-expression mu-use-package-imenu-expression))

  (add-hook 'emacs-lisp-mode-hook #'mu-add-use-package-to-imenu))

;;; Idris
(use-package idris-mode                 ; Idris editing
  :ensure t
  :mode ("\\.idr\\'" . idris-mode)
  :config
  (bind-key "C-c C-q" #'idris-quit idris-mode-map)
  (bind-keys :map idris-repl-mode-map
             ("C-c C-o" . idris-repl-clear-buffer)
             ("C-c C-q" . idris-quit)))

;;; Rust
(use-package rust-mode                  ; Rust major mode
  :ensure t
  :bind (:map rust-mode-map ("C-c <tab>" . rust-format-buffer)))

(use-package racer                      ; Completion and navigation for Rust
  :ensure t
  :bind (:map racer-mode-map
              ("C-c m h" . racer-describe)
              ("C-c m d" . racer-debug))
  :hook (rust-mode . racer-mode)
  :config (validate-setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

(use-package cargo                      ; Control Cargo
  :ensure t
  :bind (:map rust-mode-map
              ("<f6>" . cargo-process-build))
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode                  ; Toml for Cargo files
  :ensure t
  :defer t)

;;; Web development
(use-package web-mode                   ; Major mode for editing web templates
  :ensure t
  :mode ("\\.html?\\'"
         "\\.php\\'"
         "\\.tpl\\'"
         "\\.jsx\\'")
  :config
  ;; Better JSX syntax-highlighting in web-mode
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package js2-mode                   ; Powerful JavaScript mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . js2-imenu-extras-mode)
  :config
  (validate-setq
   ;; Disable parser errors and strict warnings
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil)

  ;; Try to highlight most ECMA built-ins
  (validate-setq js2-highlight-level 3))

(use-package purescript-mode            ; PureScript editing mode
  :ensure t
  :defer t)

(use-package psc-ide                    ; Minor mode for psc-ide
  :ensure t
  :after purescript-mode
  :hook (purescript-mode . (lambda ()
                             (psc-ide-mode)
                             (turn-on-purescript-indentation))))

(use-package psci                       ; PureScript REPL
  :ensure t
  :bind (:map purescript-mode-map
              ("C-c C-z" . psci))
  :hook (purescript-mode . inferior-psci-mode))

(use-package css-mode                   ; Better CSS support
  :defer t
  :config (validate-setq css-indent-offset 2))

(use-package css-eldoc                  ; Eldoc for CSS
  :ensure t
  :commands (turn-on-css-eldoc)
  :hook (css-mode . turn-on-css-eldoc))

(use-package less-css-mode              ; Mode for Less CSS files
  :mode "\\.less\\'")

(use-package scss-mode                  ; Mode for SCSS files
  :mode "\\.scss\\'")

(use-package web-beautify               ; Pretty format HTML/CSS/JS files
  :ensure t
  :init
  (with-eval-after-load 'js2-mode
    (bind-key "C-c m f" #'web-beautify-js js2-mode-map))
  (with-eval-after-load 'web-mode
    (bind-key "C-c m f" #'web-beautify-html web-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c m f" #'web-beautify-css css-mode-map)))

(use-package yaml-mode                  ; Edit YAML files
  :ensure t
  :mode "\\.yaml\\'")

(use-package restclient                 ; Interactive HTTP client
  :ensure t
  :defer t)

;;; Other languages
(use-package sh-script                  ; Shell scripts
  :defer t
  :config

  (validate-setq
   ;; Use two spaces in shell scripts.
   sh-basic-offset 2
   ;; The offset for nested indentation
   sh-basic-offset 2))

(use-package nxml-mode                  ; XML editing
  :mode "\\.xml\\'"
  :bind (:map nxml-mode-map
              ("C-c m f" . mu-xml-format))
  :config
  (validate-setq
   ;; Complete closing tags, and insert XML declarations into empty files
   nxml-slash-auto-complete-flag t
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

;;; Databases
(use-package sql                        ; SQL editing and REPL
  :mode ("\\.sql\\'" . sql-mode)
  :bind (("C-c d s" . sql-connect)
         :map sql-mode-map
         ("C-c m p" . sql-set-product)))

(use-package sql-indent                 ; Indent SQL statements
  :ensure t
  :after sql)

(use-package sqlup-mode                 ; Upcase SQL keywords
  :ensure t
  :bind (:map sql-mode-map
              ("C-c m u" . sqlup-capitalize-keywords-in-region))
  :hook (sql-mode . sqlup-mode))

;;; Bugs management
(use-package bug-reference              ; Buttonize bug references
  :defer t
  :hook ((prog-mode . bug-reference-prog-mode)
         (text-mode . bug-reference-mode)))

;;; Misc utilities
(use-package eldoc                      ; Documentation in the echo area
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :hook (eval-expression-minibuffer-setup . eldoc-mode)
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  ;; Show eldoc more promptly
  (validate-setq eldoc-idle-delay 0.1))

(use-package etags                      ; Tag navigation
  :defer t
  :config
  ;; Do not query before reverting TAGS tables
  (validate-setq tags-revert-without-query t))

(use-package compile                    ; Compile from Emacs
  :defer t
  :config
  (validate-setq
   compilation-ask-about-save nil
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
   compilation-context-lines 3))

(use-package eros                       ; Display evaluation result as overlay
  :ensure t
  :config (eros-mode 1))

;;; Keybindings
(bind-key* "C-;" #'comment-line)
(bind-key "C-c t d" #'toggle-debug-on-error)

(provide 'mu-programming)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-programming.el ends here
