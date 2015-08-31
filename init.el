;;; init.el --- Emacs configuration of Manuel Uberti -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
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

;; This file sets up packages, custom file, username and mail address. It also
;; loads the different configuration files I have in ~/.emacs.d/lisp.

;;; Code:

;;; Package setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq load-prefer-newer t)        ; Always load newer compiled files
(setq gc-cons-threshold 50000000) ; Allow more than 800Kb cache
(setq gnutls-min-prime-bits 4096) ; Avoid GnuTLS warnings

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)
(require 'grep)
(require 'subr-x)
(require 'rx)
(require 'time-date)

;;; Initialization
(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

(setq inhibit-default-init t) ; Disable the site default settings

;; Warn if the current build is more than a week old
(run-with-idle-timer
 2 nil
 (lambda ()
   (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
     (when (> (time-to-number-of-days time-since-build) 7)
       (lwarn 'emacs :warning "Your Emacs build is more than a week old!")))))

;; Set separate custom file for the customize interface
(defconst custom/custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config (setq custom-file custom/custom-file
                custom-buffer-done-kill nil    ; Kill when existing
                custom-buffer-verbose-help nil ; Remove redundant help text
                ;; Show me the real variable name
                custom-unlispify-tag-names nil
                custom-unlispify-menu-entries nil)
  :init (load custom/custom-file 'no-error 'no-message))

;; Set the directory where all backup and autosave files will be saved
(defvar backup-dir "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))

;; Personal informations
(setq user-full-name "Manuel Uberti")
(setq user-mail-address "manuel@boccaperta.com")

;; The server of `emacsclient'
(use-package server
  :defer t
  ;; Start server only if it is not already running
  :config (unless (server-running-p)
            (server-mode))
  :diminish server-buffer-clients)

;; Require files under ~/.emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(use-package custom-style)
(use-package custom-functions)
(use-package custom-pairs)
(use-package custom-keybindings)
(use-package custom-highlight)
(use-package custom-buffers)
(use-package custom-windows)
(use-package custom-helm)
(use-package custom-editing)
(use-package custom-navigation)
(use-package custom-search)
(use-package custom-files :defer 5)
(use-package custom-completion :defer 5)
(use-package custom-formatting)
(use-package custom-languages)
(use-package custom-latex)
(use-package custom-vers-control :defer 5)
(use-package custom-net)
(use-package custom-org :defer 2)
(use-package custom-programming)
(use-package custom-project)
(use-package custom-shells)
(use-package custom-utilities)

;;; init.el ends here
