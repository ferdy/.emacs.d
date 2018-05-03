;;; init.el --- Emacs configuration of Manuel Uberti -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; URL: https://github.com/manuel-uberti/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; This is the GNU Emacs configuration of Manuel Uberti.

;;; Code:

;;; Package setup
(require 'package)

(setq package-archives
      '(("GNU ELPA" . "http://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")
        ("ORG"      . "https://orgmode.org/elpa/")))

(package-activate-all)

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings
(setq message-log-max 10000)            ; Debugging

;; Allow more than 800Kb cache during init
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defun mu-set-gc-threshold ()
  "Reset `gc-cons-threshold' and `gc-cons-percentage' to their default values."
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package diminish                   ; Hide modes in the mode-line
  :ensure t)

(use-package dash                       ; A modern list library
  :ensure t)

(require 'subr-x)
(require 'time-date)

;;; Initialization
(setq inhibit-default-init t)           ; Disable the site default settings

;; Ensure resizing Emacs window doesn't cause display problems
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;; Validation
(use-package validate                   ; Validate options
  :ensure t)

(use-package exec-path-from-shell       ; Set up environment variables
  :ensure t
  :if (display-graphic-p)
  :config
  (validate-setq exec-path-from-shell-variables
                 '("PATH"               ; Full path
                   "FULLNAME"           ; First and last name
                   "EMAIL"              ; Personal email
                   "INFOPATH"           ; Info directories
                   "JAVA_OPTS"          ; Options for Java processes
                   "RUST_SRC_PATH"      ; Rust sources, for racer
                   "CARGO_HOME"         ; Cargo home, for racer
                   ))

  (exec-path-from-shell-initialize))

;; Personal informations
(validate-setq user-full-name (getenv "FULLNAME"))
(validate-setq user-mail-address (getenv "EMAIL"))

;; Set separate custom file for the customize interface
(defconst mu-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit                   ; Set up custom.el
  :defer t
  :config
  (validate-setq
   custom-file mu-custom-file
   custom-buffer-done-kill nil          ; Kill when existing
   custom-buffer-verbose-help nil       ; Remove redundant help text
   custom-unlispify-tag-names nil       ; Show me the real variable name
   custom-unlispify-menu-entries nil)
  :init (load mu-custom-file 'no-error 'no-message))

(use-package no-littering               ; Keep .emacs.d clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (validate-setq
   create-lockfiles nil
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

  (validate-setq
   backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/")))
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package server                     ; The server of `emacsclient'
  :config (or (server-running-p) (server-mode)))

;; Confirm before quitting Emacs
(validate-setq confirm-kill-emacs #'y-or-n-p)

;;; Require files under ~/.emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(use-package mu-style)
(use-package mu-functions)
(use-package mu-keybindings)
(use-package mu-pairs)
(use-package mu-highlight)
(use-package mu-buffers)
(use-package mu-windows)
(use-package mu-ivy)
(use-package mu-editing)
(use-package mu-navigation)
(use-package mu-search)
(use-package mu-flycheck :defer 1)
(use-package mu-files :defer 1)
(use-package mu-dired)
(use-package mu-completion :defer 1)
(use-package mu-languages)
(use-package mu-latex :defer 2)
(use-package mu-projectile)
(use-package mu-vers-control :defer 1)
(use-package mu-net)
(use-package mu-utilities)
(use-package mu-org)
(use-package mu-programming)
(use-package mu-shells)

;; Reset default values
(add-hook 'emacs-startup-hook #'mu-set-gc-threshold)

;; Immediately visit my main GTD file
(find-file "~/org/gtd/gtd.org")

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
