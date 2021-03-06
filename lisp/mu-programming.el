;;; mu-programming.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for programming utilities.

;;; Code:

;;; Clojure
(use-package cider                      ; Clojure development environment
  :ensure t
  :defer t
  :config (setq cider-offer-to-open-cljs-app-in-browser nil))

(use-package cider-mode                 ; CIDER mode for REPL interaction
  :ensure cider
  :defer t
  :bind (:map cider-mode-map
              ("C-c m l" . cider-load-all-project-ns))
  :config
  (defun mu-cider-mode-line-info ()
    "Simplify CIDER mode-line indicator."
    (if-let* ((current-connection (ignore-errors (cider-current-repl))))
        (with-current-buffer current-connection
          (concat
           (symbol-name cider-repl-type)
           (format
            ":%s" (or (cider--project-name nrepl-project-dir) "<no project>"))))
      "-"))

  (setq cider-font-lock-dynamically t
        cider-invert-insert-eval-p t
        cider-switch-to-repl-after-insert-p nil
        cider-mode-line '(:eval
                          (format " CIDER[%s]" (mu-cider-mode-line-info)))))

(use-package cider-common               ; CIDER common use functions
  :ensure cider
  :after cider
  :config (setq cider-prompt-for-symbol nil))

(use-package clojure-mode               ; Major mode for Clojure files
  :ensure t
  :mode (("\\.boot$" . clojure-mode)
         ("\\.clj$"  . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.edn$"  . clojure-mode))
  :hook ((clojure-mode . cider-mode)
         (clojure-mode . subword-mode))
  :config
  (setq clojure-align-reader-conditionals t)

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
    (reporting 1))

  (defun mu--start-with-p (symbol)
    "Check if there is a SYMBOL at (point)."
    (interactive)
    (equal symbol (buffer-substring-no-properties (point) (+ 1 (point)))))

  (defun mu--live-delete-and-extract-sexp ()
    "Delete the sexp and return it."
    (interactive)
    (let* ((begin (point)))
      (forward-sexp)
      (let* ((result (buffer-substring-no-properties begin (point))))
        (delete-region begin (point))
        result)))

  (defun mu--wrap-with-symbols (opening closing)
    "Wrap current coll within OPENING and CLOSING."
    (interactive)
    (insert opening
            (substring (mu--live-delete-and-extract-sexp) 1 -1)
            closing))

  (defun mu-live-cycle-clj-coll ()
    "Convert the coll at (point) from (x) -> {x} -> [x] -> (x)."
    (interactive)
    (let* ((original-point (point)))
      (while (and (> (point) 1)
                  (not (mu--start-with-p "("))
                  (not (mu--start-with-p "{"))
                  (not (mu--start-with-p "[")))
        (backward-char))
      (cond
       ((mu--start-with-p "(")
        (mu--wrap-with-symbols "{" "}"))
       ((mu--start-with-p "{")
        (mu--wrap-with-symbols "[" "]"))
       ((mu--start-with-p "[")
        (mu--wrap-with-symbols "(" ")"))
       ((equal 1 (point))
        (message "Beginning of file reached, this was probably a mistake.")))
      (goto-char original-point)))

  (bind-key "C-`" #'mu-live-cycle-clj-coll clojure-mode-map)

  (defun mu-cider-switch-to-any-repl-buffer (&optional set-namespace)
    "Switch to current REPL buffer, when possible in an existing window.
The type of the REPL is inferred from the mode of current buffer.  With a
prefix arg SET-NAMESPACE sets the namespace in the REPL buffer to that of
the namespace in the Clojure source buffer"
    (interactive "P")
    (cider--switch-to-repl-buffer
     (cider-current-repl "any" t)
     set-namespace))

  (bind-key "C-c z" #'mu-cider-switch-to-any-repl-buffer clojure-mode-map))

(use-package cider-eval                 ; Interactive evaluation functionalities
  :after cider
  :config (unbind-key "C-c C-p" cider-mode-map))

(use-package clojure-mode-extra-font-locking ; Extra font-locking for Clojure
  :ensure t)

(use-package nrepl-client               ; Client for Clojure nREPL
  :ensure cider
  :defer t
  :config (setq nrepl-hide-special-buffers t
                nrepl-repl-buffer-name-template "*cider-repl %j %r:%S*"))

(use-package cider-repl                 ; REPL interactions with CIDER
  :ensure cider
  :bind (:map cider-repl-mode-map
              ("C-c M-o" . cider-repl-clear-buffer)
              ("C-c C-o" . cider-repl-switch-to-other)
              ("C-c t p" . cider-toggle-pretty-printing))
  :hook ((cider-repl-mode . company-mode)
         (cider-repl-mode . subword-mode))
  :config (setq
           cider-repl-display-help-banner nil
           cider-repl-history-file (locate-user-emacs-file "cider-repl-history")
           cider-repl-history-size 1000
           cider-repl-result-prefix ";; => "
           cider-repl-use-pretty-printing t
           cider-repl-wrap-history t))

(use-package cider-stacktrace           ; Navigate stacktrace
  :ensure cider
  :defer t)

(use-package cider-util                 ; Common utilities
  :ensure cider
  :config (setq cider-jdk-src-paths '("~/sources/clojure/clojure-1.8.0-sources"
                                      "~/sources/clojure/clojure-1.9.0-sources"
                                      "~/sources/java/openjdk-8-src")))

(use-package clj-refactor               ; Refactoring utilities
  :ensure t
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (yas-minor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c RET")))
  :config
  (setq cljr-add-ns-to-blank-clj-files t
        cljr-auto-sort-ns t
        cljr-favor-prefix-notation nil
        cljr-favor-private-functions nil
        cljr-warn-on-eval nil
        cljr-suppress-middleware-warnings t)

  (setq cljr-cljs-clojure-test-declaration
        "[cljs.test :refer-macros [deftest is use-fixtures]]"
        cljr-clojure-test-declaration "[clojure.test :refer :all]")

  (advice-add 'cljr-add-require-to-ns :after
              (lambda (&rest _)
                (yas-next-field)
                (yas-next-field))))

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
  :hook ((haskell-mode . haskell-auto-insert-module-template)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . subword-mode))
  :config (with-eval-after-load 'haskell-mode
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

(define-minor-mode stack-exec-path-mode
  "Set `exec-path' to the path \"stack exec\" would use."
  nil
  :lighter ""
  :global nil
  (if stack-exec-path-mode
      (when (and (executable-find "stack")
                 (locate-dominating-file default-directory "stack.yaml"))
        (setq-local
         exec-path
         (seq-uniq
          (append
           (list
            (concat
             (string-trim-right
              (shell-command-to-string "stack path --local-install-root"))
             "/bin"))
           (parse-colon-path
            (replace-regexp-in-string
             "[\r\n]+\\'" ""
             (shell-command-to-string "stack path --bin-path"))))
          'string-equal)))
    (kill-local-variable 'exec-path)))

(add-hook 'haskell-mode-hook #'stack-exec-path-mode)

(use-package haskell-cabal              ; Support for Cabal packages
  :ensure intero
  :mode ("\\.cabal\\'" . haskell-cabal-mode)
  :hook ((haskell-cabal-mode . subword-mode)))

(use-package dhall-mode                 ; Dhall support and editing
  :ensure t
  :mode ("\\.dhall\\'" . dhall-mode)
  :hook ((dhall-mode . mu-no-trailing-whitespace)
         (dhall-mode . stack-exec-path-mode)))

(use-package liquid-types               ; Show inferred liquid-types
  :ensure t
  :defer t
  :commands liquid-types-mode)

;;; Emacs Lisp
(use-package ielm                       ; Emacs Lisp REPL
  :bind ("C-c d i" . ielm)
  :config (bind-key "C-c C-q" #'comint-send-eof inferior-emacs-lisp-mode-map))

(use-package elisp-mode                 ; Emacs Lisp editing
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c C-k" . eval-buffer))
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
             ("C-c M-o" . idris-repl-clear-buffer)
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
  :config (setq racer-rust-src-path (getenv "RUST_SRC_PATH")))

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
  (setq web-mode-markup-indent-offset2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2)

  ;; Better JSX syntax-highlighting in web-mode
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it)))

(use-package js                         ; JavaScript editing
  :defer t
  :config (setq js-indent-level 2))

(use-package js2-mode                   ; Powerful JavaScript mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . js2-imenu-extras-mode)
  :config (setq js2-highlight-level 3
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil))

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
  :config
  (setq css-indent-offset 2)

  ;; Embedding in HTML
  (with-eval-after-load 'mmm-vars
    (mmm-add-group
     'html-css
     '((css-cdata
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
        :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
        :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</style>" @)))
       (css
        :submode css-mode
        :face mmm-code-submode-face
        :front "<style[^>]*>[ \t]*\n?"
        :back "[ \t]*</style>"
        :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                     @ "\n" _ "\n" @ "</style>" @)))
       (css-inline
        :submode css-mode
        :face mmm-code-submode-face
        :front "style=\""
        :back "\"")))
    (dolist (mode (list 'html-mode 'nxml-mode))
      (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))))

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
  :mode "\\.yaml\\'"
  :hook (yaml-mode . goto-address-prog-mode))

;;; Other languages
(use-package sh-script                  ; Shell scripts
  :defer t
  :config (setq sh-basic-offset 2))

(use-package nxml-mode                  ; XML editing
  :mode "\\.xml\\'"
  :bind (:map nxml-mode-map
              ("C-c m f" . mu-xml-format))
  :config
  (setq nxml-auto-insert-xml-declaration-flag t
        nxml-sexp-element-flag t
        nxml-slash-auto-complete-flag t)

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
         ("C-c C-z" . mu-sql-switch-to-sqli)
         ("C-c m p" . sql-set-product))
  :config
  (defun mu-sql-switch-to-sqli ()
    "Switch to SQLi buffer."
    (interactive)
    (unless (and sql-buffer
                 (buffer-live-p (get-buffer sql-buffer)))
      (sql-set-sqli-buffer))
    (pop-to-buffer sql-buffer)))

(use-package sql-indent                 ; SQL indentation
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

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
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  (setq eldoc-idle-delay 0.1))

(use-package etags                      ; Tag navigation
  :defer t
  :config (setq tags-revert-without-query t))

(use-package compile                    ; Compile from Emacs
  :defer t
  :config (setq compilation-always-kill t
                compilation-auto-jump-to-first-error t
                compilation-context-lines 3
                compilation-disable-input t
                compilation-scroll-output 'first-error
                compilation-skip-threshold 2
                compilation-ask-about-save nil))

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
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-programming.el ends here
