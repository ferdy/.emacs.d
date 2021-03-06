;;; init.el --- Emacs configuration of Manuel Uberti -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; URL: https://github.com/manuel-uberti/.emacs.d

;;; Commentary:

;; This is the GNU Emacs configuration of Manuel Uberti, mostly used for
;; Clojure, Haskell, Git, LaTeX and Org mode.

;;; Code:

;; Sane defaults
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings
(setq enable-local-variables :all)      ; Always enable all local variables
(setq load-prefer-newer t)              ; Always load newer compiled files
(setq message-log-max 10000)            ; Debugging

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package diminish                   ; Hide modes in the mode-line
  :ensure t)

(use-package dash                       ; A modern list library
  :ensure t)

(use-package seq                        ; Sequence manipulation functions
  :ensure t)

(require 'subr-x)                       ; Extra Lisp functions
(require 'time-date)                    ; Functions for times and dates

;;; Initialization
(setq inhibit-default-init t)           ; Disable the site default settings

;; Ensure resizing Emacs window doesn't cause display problems
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(use-package exec-path-from-shell       ; Set up environment variables
  :ensure t
  :if (display-graphic-p)
  :config
  (setq exec-path-from-shell-variables
        '("PATH"               ; Full path
          "FULLNAME"           ; First and last name
          "EMAIL"              ; Personal email
          "INFOPATH"           ; Info directories
          "JAVA_OPTS"          ; Options for Java processes
          "RUST_SRC_PATH"      ; Rust sources, for racer
          "CARGO_HOME"         ; Cargo home, for racer
          ))

  ;; Do not run a shell process twice
  (setq-default exec-path-from-shell-arguments nil)

  (exec-path-from-shell-initialize))

;; Personal informations
(setq user-full-name (getenv "FULLNAME")
      user-mail-address (getenv "EMAIL"))

;; Set separate custom file for the customize interface
(defconst mu-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit                   ; Set up custom.el
  :defer t
  :config (setq custom-buffer-done-kill t
                custom-buffer-verbose-help nil
                custom-unlispify-menu-entries nil
                custom-unlispify-tag-names nil
                custom-file mu-custom-file)
  :init (load mu-custom-file 'no-error 'no-message))

;; Disable auto save and backups
(setq auto-save-default nil
      make-backup-files nil)

(use-package no-littering               ; Keep .emacs.d clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (setq create-lockfiles nil
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/")))
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Allow access from `emacsclient'
(add-hook 'after-init-hook (lambda ()
                             (require 'server)
                             (unless (server-running-p)
                               (server-start))))

;; Confirm before quitting Emacs
(setq confirm-kill-emacs #'y-or-n-p)

;; Do not ask for confirm when killing processes
(setq confirm-kill-processes nil)

;;; Require files under ~/.emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(use-package mu-style)
(use-package mu-functions)
(use-package mu-keybindings)
(use-package mu-pairs)
(use-package mu-cursors :defer 1)
(use-package mu-highlight)
(use-package mu-buffers)
(use-package mu-windows)
(use-package mu-ivy)
(use-package mu-editing)
(use-package mu-whitespace)
(use-package mu-navigation)
(use-package mu-search :defer 1)
(use-package mu-flycheck :defer 1)
(use-package mu-files :defer 1)
(use-package mu-dired :defer 1)
(use-package mu-completion)
(use-package mu-languages :defer 1)
(use-package mu-latex :defer 2)
(use-package mu-projectile)
(use-package mu-vers-control :defer 1)
(use-package mu-net :defer 1)
(use-package mu-utilities :defer 2)
(use-package mu-org :defer 1)
(use-package mu-programming)
(use-package mu-shells)
(use-package mu-feed :defer 2)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; init.el ends here
